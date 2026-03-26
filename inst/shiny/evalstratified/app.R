library(shiny)
library(evalstratified)
library(tibble)
library(dplyr)
library(readr)
library(rhandsontable)
library(htmlwidgets)
library(ggplot2)
library(bslib)
library(bsicons)

# define risk options used in multiple tabs
risk_choices <- c("hoog (H)" = "H",
                  "midden (M)" = "M",
                  "laag (L)" = "L")
risk_vec_ui <- c("H", "M", "L")

# helper functie: parseer getallen strict volgens nl notatie
parse_dutch_num <- function(x) {
  stopifnot(is.character(x))
  parse_number(x, locale = locale(decimal_mark = ",", grouping_mark = "."))
}

# helper functie: maakt netjes een label met een (i) tooltip voor de UI
info_label <- function(tekst, tooltip_tekst) {
  tags$span(tekst, tooltip(
    bs_icon("info-circle", class = "ms-1", style = "font-size: 0.9em; color: #007bff;"),
    tooltip_tekst
  ))
}

ui <- navbarPage(
  "evalstratified",
  theme = bs_theme(version = 5),

  # --- head: css en force-resize script ---
  header = tags$head(
    tags$style(HTML("
      /* force all handsontable cells to be white with black text */
      .handsontable td { background-color: #ffffff !important; color: #000000 !important; }
      .handsontable td.current { background-color: #e6f2ff !important; }
      /* right align text columns that act as numbers */
      .handsontable .htRight { text-align: right; }
    ")),
    tags$script(HTML('
      // Forceer rhandsontable om zichzelf expliciet opnieuw te tekenen na een tab-wissel
      $(document).on("shiny:tabshown", function(e) {
        setTimeout(function() {
          // Zoek alle htmlwidgets op het scherm
          $(".html-widget").each(function() {
            var widget = HTMLWidgets.getInstance(this);
            // Als het een handsontable is, forceer een harde render
            if (widget && widget.hot) {
              widget.hot.render();
            }
          });
          // Stuur voor de zekerheid ook nog de algemene resize
          window.dispatchEvent(new Event("resize"));
        }, 150); // 150ms geeft de browser nét genoeg tijd om de tab te openen
      });
    '))
  ),

  # -------------------------------------------------------------------------
  # tab 1: haro nog nodige zekerheid
  # -------------------------------------------------------------------------
  tabPanel(
    "nog nodige zekerheid",
    sidebarLayout(
      sidebarPanel(
        h4("risico-inschatting"),
        helpText(
          "Bereken de nog benodigde zekerheid volgens HARo paragraaf B7.3.4."
        ),
        selectInput(
          "haro_ihr",
          label = info_label(
            "inherent risico (ihr):",
            "De inschatting van de kans op een materiële fout zonder gebruik van interne beheersing en cijferanalyse."
          ),
          choices = risk_choices
        ),
        selectInput(
          "haro_ibr",
          label = info_label(
            "interne beheersingsrisico (ibr):",
            "Het risico dat de interne beheersing niet goed werkt."
          ),
          choices = risk_choices
        ),
        selectInput(
          "haro_car",
          label = info_label(
            "cijferanalyserisico (car):",
            "Het risico dat cijferbeoordelingen en analytische procedures fouten niet vinden."
          ),
          choices = risk_choices
        )
      ),
      mainPanel(
        h3("resultaat"),
        verbatimTextOutput("res_haro"),
        p(
          "Dit is de zekerheid (fractie 0-1) die u nog uit detailcontroles moet halen."
        )
      )
    )
  ),

  # -------------------------------------------------------------------------
  # tab 2: foutlozepostenequivalent
  # -------------------------------------------------------------------------
  tabPanel(
    "foutlozepostenequivalent",
    sidebarLayout(
      sidebarPanel(
        h4("risico & materialiteit"),
        helpText("Bereken hoeveel foutloze posten uw risico-inschatting waard is."),
        selectInput(
          "fpe_ihr",
          label = info_label(
            "inherent risico (ihr):",
            "De inschatting van de kans op een materiële fout zonder gebruik van interne beheersing en cijferanalyse."
          ),
          choices = risk_choices
        ),
        selectInput(
          "fpe_ibr",
          label = info_label(
            "interne beheersingsrisico (ibr):",
            "Het risico dat de interne beheersing niet goed werkt."
          ),
          choices = risk_choices
        ),
        selectInput(
          "fpe_car",
          label = info_label(
            "cijferanalyse (car):",
            "Het risico dat cijferbeoordelingen en analytische procedures fouten niet vinden."
          ),
          choices = risk_choices
        ),
        textInput(
          "fpe_mat",
          label = info_label(
            "materialiteit (fractie):",
            "De grens waarboven een afwijking als materieel wordt beschouwd (bijv. 0,01 voor 1%)."
          ),
          value = "0,01"
        )
      ),
      mainPanel(
        h3("resultaat"),
        verbatimTextOutput("res_fpe"),
        p("Aantal posten dat overeenkomt met de verlaagde risico's.")
      )
    )
  ),

  # -------------------------------------------------------------------------
  # tab 3: eval stratified (main analysis)
  # -------------------------------------------------------------------------
  tabPanel(
    "evaluatie gestratificeerd",
    sidebarLayout(
      sidebarPanel(
        h4("1. data invoer"),

        radioButtons(
          "input_method",
          "methode:",
          choices = c("handmatige invoer" = "manual", "csv-upload" = "upload")
        ),

        conditionalPanel(
          condition = "input.input_method == 'upload'",
          fileInput("file_strat", "upload csv-bestand:", accept = ".csv"),
          downloadButton("download_template", "download csv voorbeeldbestand")
        ),

        conditionalPanel(condition = "input.input_method == 'manual'", helpText("Vul de tabel rechts in.")),

        hr(),
        h4("2. instellingen"),

        # Keuze voor het statistische model
        radioButtons(
          "strat_model",
          label = info_label(
            "statistisch model:",
            "Kies het model voor de extrapolatie. Binomiaal is nauwkeurig. Poisson wordt traditioneel gebruikt maar is minder nauwkeurig en gaat bij grotere foutfracties de mist in."
          ),
          choices = c("binomiaal" = "binomiaal", "poisson" = "poisson"),
          inline = TRUE
        ),

        # NIEUW: Keuze voor de rekenmethode
        radioButtons(
          "strat_methode",
          label = info_label(
            "rekenmethode",
            "FFT of Monte Carlo. De nauwkeurigheid van beide methoden neemt toe bij grotere granulariteit. Beide zijn grofweg even efficient. Beide zijn deterministisch, dit wel afhankelijk van granulariteit, machinenauwkeurigheid, en details van de onderliggende routines."
          ),
          choices = c("FFT" = "FFT", "Monte Carlo" = "MonteCarlo"),
          inline = TRUE
        ),

        textInput(
          "strat_conf",
          label = info_label(
            "zekerheid (0,95 = 95%)",
            "Het gewenste betrouwbaarheidsniveau voor de evaluatie."
          ),
          value = "0,95"
        ),

        # Granulariteit geldt nu voor BEIDE methodes
        numericInput(
          "strat_gran",
          label = info_label(
            "granulariteit / iteraties",
            ">= 1. Hoe meer, hoe nauwkeuriger de berekening van de maximale fout. Maar het rekenen duurt langer."
          ),
          value = 10000,
          min = 100,
          step = 1000
        ),

        # Seed is ALLEEN relevant voor Monte Carlo
        conditionalPanel(
          condition = "input.strat_methode == 'MonteCarlo'",
          numericInput(
            "strat_seed",
            label = info_label(
              "seed (startwaarde)",
              "Een vast startpunt voor de randomgenerator. Gebruik hetzelfde getal om later exact dezelfde uitkomst te reproduceren."
            ),
            value = 1
          )
        ),

        checkboxInput(
          "strat_comp",
          label = info_label(
            "vergelijk met andere methoden",
            "Berekent ter vergelijking ook de uitkomst volgens gewogen gemiddelde en als gepoold."
          ),
          value = TRUE
        ),
        hr(),
        actionButton(
          "run_strat",
          "bereken evaluatie",
          class = "btn-success",
          width = "100%"
        ),
        br(),
        br(),
        downloadButton(
          "download_report",
          "download PDF rapport",
          class = "btn-primary",
          style = "width: 100%;"
        )
      ),

      mainPanel(tabsetPanel(
        id = "hoofd_tabs",
        tabPanel(
          "1. invoertabel",
          br(),
          conditionalPanel(
            condition = "input.input_method == 'manual'",
            helpText("Vul hieronder uw data in en klik links op 'bereken evaluatie'."),
            rHandsontableOutput("hot_input")
          )
        ),
        tabPanel(
          "2. grafiek & hoofdresultaten",
          value = "tab_grafiek",
          br(),
          h3("convolutieresultaten"),
          plotOutput("plot_strat_main", height = "400px"),
          br(),
          tableOutput("table_strat_main")
        ),
        tabPanel(
          "3. details & vergelijking",
          br(),
          h3("vergelijking"),
          tableOutput("table_strat_comp"),
          hr(),
          h4("data en resultaten per steekproef zoals verwerkt door het model"),
          div(style = 'overflow-x: scroll', tableOutput("table_strat_input"))
        )
      ))
    )
  )
)

server <- function(input, output, session) {
  # Zorg dat het scherm automatisch naar de grafiek springt bij het klikken op berekenen
  observeEvent(input$run_strat, {
    updateTabsetPanel(session, "hoofd_tabs", selected = "tab_grafiek")
  })

  # --- logic tab 1 ---
  output$res_haro <- renderText({
    # Wacht veilig tot de inputs geladen zijn
    req(input$haro_ihr, input$haro_ibr, input$haro_car)

    val <- haro_nog_nodige_zekerheid(input$haro_ihr, input$haro_ibr, input$haro_car)
    paste("nog nodige zekerheid:",
          format(round(val, 4), decimal.mark = ",", nsmall = 4))
  })

  # --- logic tab 2 ---
  output$res_fpe <- renderText({
    # Wacht veilig tot de unieke fpe_ inputs geladen zijn
    req(input$fpe_ihr, input$fpe_ibr, input$fpe_car, input$fpe_mat)

    mat_val <- parse_dutch_num(input$fpe_mat)

    validate(need(
      !is.na(mat_val),
      "Vul een geldig getal in voor materialiteit."
    ))

    val <- foutloze_posten_equivalent(input$fpe_ihr, input$fpe_ibr, input$fpe_car, mat_val)

    paste("foutlozepostenequivalent:",
          format(
            round(val, 0),
            big.mark = ".",
            decimal.mark = ","
          ))
  })

  # --- logic tab 3 ---
  output$download_template <- downloadHandler(
    filename = function() {
      "steekproeven_template.csv"
    },
    content = function(file) {
      df <- tibble(
        naam = c("steekproef 1", "steekproef 2"),
        waarde_laag = c(1000000, 500000),
        n_laag = c(30, 20),
        k_laag = c(0, 1),
        fout_hoog = c(0, 5000),
        goed_hoog = c(0, 45000),
        ihr = c("H", "L"),
        ibr = c("H", "L"),
        car = c("H", "H"),
        materialiteit = c(0.01, 0.01)
      )
      write_csv2(df, file)
    }
  )

  # render the editable table
  output$hot_input <- renderRHandsontable({
    df <- data.frame(
      naam = rep(NA_character_, 8),
      waarde_laag = rep(NA_character_, 8),
      n_laag = rep(NA_character_, 8),
      k_laag = rep(NA_character_, 8),
      fout_hoog = rep("0", 8),
      goed_hoog = rep("0", 8),
      ihr = rep("H", 8),
      ibr = rep("H", 8),
      car = rep("H", 8),
      materialiteit = rep("0,01", 8),
      stringsAsFactors = FALSE
    )

    renderer_nl_money <- JS(
      "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL', { minimumFractionDigits: 2, maximumFractionDigits: 2 }); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
    )
    renderer_nl_percent <- JS(
      "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL', { style: 'percent', minimumFractionDigits: 2 }); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
    )
    renderer_nl_general <- JS(
      "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL'); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
    )

    # 1. Alleen het icoontje toevoegen (zonder de 'title' in de span)
    koppen <- c(
      "naam <span>&#9432;</span>",
      "waarde_laag <span>&#9432;</span>",
      "n_laag <span>&#9432;</span>",
      "k_laag <span>&#9432;</span>",
      "fout_hoog <span>&#9432;</span>",
      "goed_hoog <span>&#9432;</span>",
      "ihr <span>&#9432;</span>",
      "ibr <span>&#9432;</span>",
      "car <span>&#9432;</span>",
      "materialiteit <span>&#9432;</span>"
    )

    # 2. De rhandsontable bouwen en de tooltips via JavaScript injecteren
    rhandsontable(df, stretchH = "all") %>%
      hot_col("naam", type = "text") %>%
      hot_col("waarde_laag", type = "text", renderer = renderer_nl_money) %>%
      hot_col("n_laag", type = "text", renderer = renderer_nl_general) %>%
      hot_col("k_laag", type = "text", renderer = renderer_nl_general) %>%
      hot_col("fout_hoog", type = "text", renderer = renderer_nl_money) %>%
      hot_col("goed_hoog", type = "text", renderer = renderer_nl_money) %>%
      hot_col("ihr", type = "dropdown", source = as.list(risk_vec_ui)) %>%
      hot_col("ibr", type = "dropdown", source = as.list(risk_vec_ui)) %>%
      hot_col("car", type = "dropdown", source = as.list(risk_vec_ui)) %>%
      hot_col("materialiteit", type = "text", renderer = renderer_nl_percent) %>%
      hot_cols(colHeaders = koppen) %>%
      onRender(
        "
        function(el, x) {
          var hot = this.hot;
          hot.updateSettings({
            afterGetColHeader: function(col, TH) {
              // Array met tooltips in exact dezelfde volgorde als de kolommen
              var tooltips = [
                'Naam van de steekproef',
                'Totale boekwaarde van het lage stratum, dus van de posten kleiner dan het interval',
                'Aantal gecontroleerde posten kleiner dan het interval van de steekproef',
                'De som van de foutfracties van de getrokken posten kleiner dan het interval',
                'Foute waarde in geld van de 100% gecontroleerde massa',
                'Goede waarde in geld van de 100% gecontroleerde massa',
                'Inherent risico voor deze steekproef',
                'Interne beheersingsrisico voor deze steekproef',
                'Cijferanalyserisico voor deze steekproef',
                'Toegestane afwijking, als fractie (0-1), voor deze steekproef'
              ];

              if (col >= 0 && col < tooltips.length) {
                // Zet de tooltip direct op de fysieke tabelcel
                TH.setAttribute('title', tooltips[col]);
                // Verander de muiscursor in een 'help' icoontje als je eroverheen zweeft
                TH.style.cursor = 'help';
              }
            }
          });
        }
      "
      )
  })

  # reactive calculation
  strat_results <- eventReactive(input$run_strat, {
    conf_val <- parse_dutch_num(input$strat_conf)
    validate(need(!is.na(conf_val), "Vul een geldig getal in voor zekerheid."))

    final_df <- NULL

    if (input$input_method == "upload") {
      req(input$file_strat)
      tryCatch({
        # Lees in als Nederlandse CSV (puntkomma gescheiden)
        final_df <- read_csv2(input$file_strat$datapath, show_col_types = FALSE)

        # Check of de cruciale kolommen wel succesvol zijn gescheiden
        vereiste_kolommen <- c(
          "naam",
          "waarde_laag",
          "n_laag",
          "k_laag",
          "ihr",
          "ibr",
          "car",
          "materialiteit"
        )
        ontbrekend <- setdiff(vereiste_kolommen, names(final_df))

        if (length(ontbrekend) > 0) {
          showNotification(
            paste(
              "Fout bij inlezen. Ontbrekende kolommen:",
              paste(ontbrekend, collapse = ", "),
              "--- Tip: Controleer of uw CSV-bestand is opgeslagen met puntkomma's (;) als scheidingsteken."
            ),
            type = "error",
            duration = 10
          )
          return(NULL) # Breek af voordat de app crasht
        }

        # Vul eventueel ontbrekende hoogstratum-kolommen aan met 0 (gebruiksvriendelijkheid)
        if (!"fout_hoog" %in% names(final_df))
          final_df$fout_hoog <- 0
        if (!"goed_hoog" %in% names(final_df))
          final_df$goed_hoog <- 0

        # Zet alles om naar harde, wiskundige getallen
        final_df <- final_df %>%
          mutate(
            waarde_laag = parse_dutch_num(waarde_laag),
            n_laag = parse_dutch_num(n_laag),
            k_laag = parse_dutch_num(k_laag),
            fout_hoog = parse_dutch_num(fout_hoog),
            goed_hoog = parse_dutch_num(goed_hoog),
            materialiteit = parse_dutch_num(materialiteit)
          )
      }, error = function(e) {
        showNotification("Kan het csv-bestand niet verwerken. Controleer de opmaak.",
                         type = "error")
        return(NULL)
      })
    } else {
      req(input$hot_input)
      raw_df <- hot_to_r(input$hot_input)
      names(raw_df) <- c(
        "naam",
        "waarde_laag",
        "n_laag",
        "k_laag",
        "fout_hoog",
        "goed_hoog",
        "ihr",
        "ibr",
        "car",
        "materialiteit"
      )

      final_df <- raw_df %>%
        as_tibble() %>%
        filter(!is.na(naam) & naam != "") %>%
        mutate(
          waarde_laag = parse_dutch_num(waarde_laag),
          n_laag = parse_dutch_num(n_laag),
          k_laag = parse_dutch_num(k_laag),
          fout_hoog = parse_dutch_num(fout_hoog),
          goed_hoog = parse_dutch_num(goed_hoog),
          materialiteit = parse_dutch_num(materialiteit)
        )

      if (nrow(final_df) == 0) {
        showNotification("Vul tenminste één regel in met een 'naam'.", type = "warning")
        return(NULL)
      }
    }

    final_df <- final_df %>%
      mutate(
        waarde_hoog = fout_hoog + goed_hoog,
        n_hoog = ifelse(waarde_hoog > 0, 1, 0),
        n_totaal = n_laag + n_hoog,
        waarde_populatie = waarde_laag + waarde_hoog,
        ihr = toupper(ihr),
        ibr = toupper(ibr),
        car = toupper(car)
      )

    # Validatie op fysieke onmogelijkheden
    foute_rijen <- final_df %>% filter(k_laag > n_laag)

    if (nrow(foute_rijen) > 0) {
      foute_namen <- paste(foute_rijen$naam, collapse = ", ")
      showNotification(
        paste("Fout in invoer bij", foute_namen, ": De foutfractie (k_laag) kan onmogelijk groter zijn dan het aantal gecontroleerde posten (n_laag)."),
        type = "error",
        duration = 10
      )
      return(NULL) # Breek de berekening af
    }

    tryCatch({
      res <- eval_stratified(
        steekproeven = final_df,
        model = input$strat_model,
        zekerheid = conf_val,
        methode = input$strat_methode,
        granulariteit = as.integer(input$strat_gran),
        start = input$strat_seed,
        vergelijk = input$strat_comp
      )
      return(res)
    }, error = function(e) {
      showNotification(paste("fout in berekening:", e$message), type = "error")
      return(NULL)
    })
  })

  # output plot (de kanskromme)
  output$plot_strat_main <- renderPlot({
    res <- strat_results()
    req(res, res$kanskromme)

    # Roep de nieuwe package functie aan
    plot_kanskromme(res)
  })

  output$table_strat_main <- renderTable({
    res <- strat_results()
    req(res)

    min_geld <- sum(res$steekproeven$fout_hoog)
    min_fractie <- min_geld / res$populatie_totaal

    mw_frac <- max(res$mw_fout_convolutie, min_fractie)
    mw_geld <- max(res$mw_fout_convolutie_geld, min_geld)
    max_frac <- max(res$max_fout_convolutie, min_fractie)
    max_geld <- max(res$max_fout_convolutie_geld, min_geld)

    tibble(
      metriek = c(
        "meest waarschijnlijke fout (fractie)",
        "meest waarschijnlijke fout (geld)",
        "maximale fout (fractie)",
        "maximale fout (geld)"
      ),
      waarde = c(
        format(
          round(mw_frac, 5),
          decimal.mark = ",",
          nsmall = 5
        ),
        format(
          round(mw_geld, 2),
          big.mark = ".",
          decimal.mark = ",",
          nsmall = 2
        ),
        format(
          round(max_frac, 5),
          decimal.mark = ",",
          nsmall = 5
        ),
        format(
          round(max_geld, 2),
          big.mark = ".",
          decimal.mark = ",",
          nsmall = 2
        )
      )
    )
  })

  output$table_strat_comp <- renderTable({
    res <- strat_results()
    req(res)
    if (is.null(res$vergelijk_met))
      return(tibble(info = "Geen vergelijking gevraagd."))
    comp <- res$vergelijk_met
    tibble(
      scenario = c("los (gewogen gemiddelde)", "als 1 (gepoolde data)"),
      `max fout (geld)` = c(
        format(
          round(comp$max_fout_los_geld, 2),
          big.mark = ".",
          decimal.mark = ",",
          nsmall = 2
        ),
        format(
          round(comp$max_fout_als1_geld, 2),
          big.mark = ".",
          decimal.mark = ",",
          nsmall = 2
        )
      )
    )
  })

  output$table_strat_input <- renderTable({
    res <- strat_results()
    req(res)

    df_disp <- res$steekproeven

    geld_cols <- c("waarde_laag",
                   "fout_hoog",
                   "goed_hoog",
                   "waarde_hoog",
                   "waarde_populatie")
    for (col in geld_cols) {
      if (col %in% names(df_disp)) {
        df_disp[[col]] <- format(
          round(df_disp[[col]], 2),
          big.mark = ".",
          decimal.mark = ",",
          nsmall = 2,
          scientific = FALSE
        )
      }
    }

    num_cols <- c("n_laag", "k_laag", "n_totaal", "extra_foutloze_posten")
    for (col in num_cols) {
      if (col %in% names(df_disp)) {
        df_disp[[col]] <- format(
          round(df_disp[[col]], 0),
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
      }
    }

    frac_cols <- c("materialiteit", "mw_fout", "max_fout")
    for (col in frac_cols) {
      if (col %in% names(df_disp)) {
        df_disp[[col]] <- format(
          round(df_disp[[col]], 5),
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
      }
    }

    df_disp %>% select(-any_of("n_hoog"))
  })

  # PDF rapportage
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("evaluatie_rapport_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Zorg dat er eerst berekend is.
      res <- strat_results()
      req(res)

      showNotification(
        "PDF wordt gegenereerd, een moment geduld...",
        type = "message",
        duration = 5
      )

      # Kopieer de template naar een tijdelijke map (noodzakelijk voor shinyapps.io).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Maak de parameters klaar om in de PDF te injecteren.
      conf_val <- parse_dutch_num(input$strat_conf)
      params <- list(res = res, zekerheid = conf_val)

      # Genereer de PDF!
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)
