library(shiny)
library(auditstratified)
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
  "auditstratified",
  theme = bs_theme(version = 5),

  # --- head: css en force-resize script ---
  header = tags$head(tags$style(
    HTML(
      "
      /* force all handsontable cells to be white with black text */
      .handsontable td { background-color: #ffffff !important; color: #000000 !important; }
      .handsontable td.current { background-color: #e6f2ff !important; }
      /* right align text columns that act as numbers */
      .handsontable .htRight { text-align: right; }
    "
    )
  ), tags$script(
    HTML(
      '
      // Forceer rhandsontable om zichzelf expliciet opnieuw te tekenen na een tab-wissel
      $(document).on("shiny:tabshown", function(e) {
        setTimeout(function() {
          $(".html-widget").each(function() {
            var widget = HTMLWidgets.getInstance(this);
            if (widget && widget.hot) {
              widget.hot.render();
            }
          });
          window.dispatchEvent(new Event("resize"));
        }, 150);
      });
    '
    )
  )),

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
  # tab 3: evaluatie gestratificeerd
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
        radioButtons(
          "strat_model",
          label = info_label(
            "statistisch model:",
            "Kies het model voor de extrapolatie. Binomiaal is nauwkeurig. Poisson wordt traditioneel gebruikt maar is minder nauwkeurig en gaat bij grotere foutfracties de mist in."
          ),
          choices = c("binomiaal" = "binomiaal", "poisson" = "poisson"),
          inline = TRUE
        ),
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
  ),

  # -------------------------------------------------------------------------
  # tab 4: planning gestratificeerd
  # -------------------------------------------------------------------------
  tabPanel(
    "planning gestratificeerd",
    sidebarLayout(
      sidebarPanel(
        h4("1. data invoer"),
        radioButtons(
          "plan_input_method",
          "methode:",
          choices = c("handmatige invoer" = "manual", "csv-upload" = "upload")
        ),
        conditionalPanel(
          condition = "input.plan_input_method == 'upload'",
          fileInput("file_plan", "upload csv-bestand:", accept = ".csv"),
          downloadButton("download_plan_template", "download csv voorbeeldbestand")
        ),
        hr(),
        h4("2. instellingen"),
        textInput(
          "plan_totale_mat",
          label = info_label(
            "totale materialiteit (fractie):",
            "Maximaal toegestane afwijking (bijv. 0,05)"
          ),
          value = "0,05"
        ),
        textInput(
          "plan_conf",
          label = info_label(
            "totale zekerheid (fractie):",
            "Algehele betrouwbaarheidsniveau (bijv. 0,95)"
          ),
          value = "0,95"
        ),
        radioButtons(
          "plan_model",
          label = info_label("statistisch model:", "Kies het model"),
          choices = c("binomiaal" = "binomiaal", "poisson" = "poisson"),
          inline = TRUE
        ),
        radioButtons(
          "plan_methode",
          label = info_label(
            "rekenmethode:",
            "FFT is sneller en sterk aanbevolen voor planning."
          ),
          choices = c("FFT" = "FFT", "Monte Carlo" = "MonteCarlo"),
          inline = TRUE
        ),
        hr(),
        actionButton(
          "run_plan",
          "bereken planning",
          class = "btn-success",
          width = "100%"
        )
      ),
      mainPanel(
        h3("planningsdata & resultaten"),
        helpText(
          "Vul de witte velden in en klik links op 'bereken planning'. De grijze velden worden automatisch berekend."
        ),
        rHandsontableOutput("hot_plan_input"),
        br(),
        h4("controlewaarde"),
        p(textOutput("text_plan_fout"), style = "color: #2c3e50; font-weight: bold;")
      )
    )
  )
)

server <- function(input, output, session) {
  # =========================================================================
  # GLOBALE RENDERERS (Beschikbaar voor alle tabbladen)
  # =========================================================================
  renderer_nl_money <- JS(
    "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL', { minimumFractionDigits: 2, maximumFractionDigits: 2 }); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
  )
  renderer_nl_percent <- JS(
    "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL', { style: 'percent', minimumFractionDigits: 2 }); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
  )
  renderer_nl_general <- JS(
    "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL'); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
  )


  # =========================================================================
  # --- logic tab 1 & 2 ---
  # =========================================================================
  output$res_haro <- renderText({
    req(input$haro_ihr, input$haro_ibr, input$haro_car)
    val <- haro_nog_nodige_zekerheid(input$haro_ihr, input$haro_ibr, input$haro_car)
    paste("nog nodige zekerheid:",
          format(round(val, 4), decimal.mark = ",", nsmall = 4))
  })

  output$res_fpe <- renderText({
    req(input$fpe_ihr,
        input$fpe_ibr,
        input$fpe_car,
        input$fpe_mat)
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


  # =========================================================================
  # --- logic tab 3: evaluatie gestratificeerd ---
  # =========================================================================
  observeEvent(input$run_strat, {
    updateTabsetPanel(session, "hoofd_tabs", selected = "tab_grafiek")
  })

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
              var tooltips = ['Naam van de steekproef', 'Totale boekwaarde van het lage stratum', 'Aantal gecontroleerde posten', 'Som van de foutfracties', 'Foute waarde in geld', 'Goede waarde in geld', 'Inherent risico', 'Interne beheersingsrisico', 'Cijferanalyserisico', 'Toegestane afwijking'];
              if (col >= 0 && col < tooltips.length) { TH.setAttribute('title', tooltips[col]); TH.style.cursor = 'help'; }
            }
          });
        }
      "
      )
  })

  strat_results <- eventReactive(input$run_strat, {
    conf_val <- parse_dutch_num(input$strat_conf)
    validate(need(!is.na(conf_val), "Vul een geldig getal in voor zekerheid."))

    final_df <- NULL

    if (input$input_method == "upload") {
      req(input$file_strat)
      tryCatch({
        final_df <- read_csv2(input$file_strat$datapath, show_col_types = FALSE)
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
              paste(ontbrekend, collapse = ", ")
            ),
            type = "error",
            duration = 10
          )
          return(NULL)
        }
        if (!"fout_hoog" %in% names(final_df))
          final_df$fout_hoog <- 0
        if (!"goed_hoog" %in% names(final_df))
          final_df$goed_hoog <- 0

        final_df <- final_df %>% mutate(across(
          c(
            waarde_laag,
            n_laag,
            k_laag,
            fout_hoog,
            goed_hoog,
            materialiteit
          ),
          parse_dutch_num
        ))
      }, error = function(e) {
        showNotification("Kan het csv-bestand niet verwerken.", type = "error")
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
      final_df <- raw_df %>% as_tibble() %>% filter(!is.na(naam) &
                                                      naam != "") %>%
        mutate(across(
          c(
            waarde_laag,
            n_laag,
            k_laag,
            fout_hoog,
            goed_hoog,
            materialiteit
          ),
          parse_dutch_num
        ))

      if (nrow(final_df) == 0) {
        showNotification("Vul tenminste één regel in.", type = "warning")
        return(NULL)
      }
    }

    final_df <- final_df %>% mutate(
      waarde_hoog = fout_hoog + goed_hoog,
      n_hoog = ifelse(waarde_hoog > 0, 1, 0),
      n_totaal = n_laag + n_hoog,
      waarde_populatie = waarde_laag + waarde_hoog,
      ihr = toupper(ihr),
      ibr = toupper(ibr),
      car = toupper(car)
    )

    if (nrow(final_df %>% filter(k_laag > n_laag)) > 0) {
      showNotification(
        "Foutfractie (k_laag) mag niet groter zijn dan (n_laag).",
        type = "error",
        duration = 10
      )
      return(NULL)
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

  output$plot_strat_main <- renderPlot({
    res <- strat_results()
    req(res, res$kanskromme)
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
    tibble(
      scenario = c("los (gewogen gemiddelde)", "als 1 (gepoolde data)"),
      `max fout (geld)` = c(
        format(
          round(res$vergelijk_met$max_fout_los_geld, 2),
          big.mark = ".",
          decimal.mark = ",",
          nsmall = 2
        ),
        format(
          round(res$vergelijk_met$max_fout_als1_geld, 2),
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
    for (col in c("waarde_laag",
                  "fout_hoog",
                  "goed_hoog",
                  "waarde_hoog",
                  "waarde_populatie")) {
      if (col %in% names(df_disp))
        df_disp[[col]] <- format(
          round(df_disp[[col]], 2),
          big.mark = ".",
          decimal.mark = ",",
          nsmall = 2,
          scientific = FALSE
        )
    }
    for (col in c("n_laag", "k_laag", "n_totaal", "extra_foutloze_posten")) {
      if (col %in% names(df_disp))
        df_disp[[col]] <- format(
          round(df_disp[[col]], 0),
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
    }
    for (col in c("materialiteit", "mw_fout", "max_fout")) {
      if (col %in% names(df_disp))
        df_disp[[col]] <- format(
          round(df_disp[[col]], 5),
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
    }
    df_disp %>% select(-any_of("n_hoog"))
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("evaluatie_rapport_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      res <- strat_results()
      req(res)
      showNotification(
        "PDF wordt gegenereerd, een moment geduld...",
        type = "message",
        duration = 5
      )
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = list(res = res, zekerheid = parse_dutch_num(input$strat_conf)),
        envir = new.env(parent = globalenv())
      )
    }
  )

  # =========================================================================
  # --- logic tab 4: planning gestratificeerd (GECOMBINEERD) ---
  # =========================================================================
  output$download_plan_template <- downloadHandler(
    filename = function() {
      "planning_template.csv"
    },
    content = function(file) {
      df <- tibble(
        naam = c("steekproef 1", "steekproef 2"),
        waarde_laag = c(1000000, 500000),
        verwachte_foutfractie = c(0.01, 0.015),
        fout_hoog = c(0, 5000),
        goed_hoog = c(0, 45000),
        n_hoog = c(0, 10),
        ihr = c("H", "L"),
        ibr = c("H", "L"),
        car = c("H", "H"),
        materialiteit = c(0.05, 0.05)
      )
      write_csv2(df, file)
    }
  )

  plan_table_data <- reactiveVal({
    data.frame(
      naam = rep(NA_character_, 8),
      waarde_laag = rep(NA_character_, 8),
      verwachte_foutfractie = rep("0,01", 8),
      fout_hoog = rep("0", 8),
      goed_hoog = rep("0", 8),
      n_hoog = rep("0", 8),
      ihr = rep("H", 8),
      ibr = rep("H", 8),
      car = rep("H", 8),
      materialiteit = rep("0,05", 8),
      n_basis = rep("", 8),
      n_definitief = rep("", 8),
      k_laag = rep("", 8),
      n_totaal = rep("", 8),
      stringsAsFactors = FALSE
    )
  })

  observeEvent(input$hot_plan_input, {
    plan_table_data(hot_to_r(input$hot_plan_input))
  })

  observeEvent(input$file_plan, {
    req(input$file_plan)
    tryCatch({
      df_up <- read_csv2(input$file_plan$datapath, show_col_types = FALSE)
      if (!"fout_hoog" %in% names(df_up))
        df_up$fout_hoog <- 0
      if (!"goed_hoog" %in% names(df_up))
        df_up$goed_hoog <- 0
      if (!"n_hoog" %in% names(df_up))
        df_up$n_hoog <- 0
      df_up$n_basis <- ""
      df_up$n_definitief <- ""
      df_up$k_laag <- ""
      df_up$n_totaal <- ""
      plan_table_data(as.data.frame(df_up))
    }, error = function(e) {
      showNotification("Fout bij inlezen CSV.", type = "error")
    })
  })

  output$hot_plan_input <- renderRHandsontable({
    df <- plan_table_data()
    koppen <- c(
      "naam",
      "waarde_laag",
      "verwacht_fout%",
      "fout_hoog",
      "goed_hoog",
      "n_hoog",
      "ihr",
      "ibr",
      "car",
      "materialiteit",
      "n_basis",
      "n_definitief",
      "k_laag",
      "n_totaal"
    )
    renderer_readonly <- JS(
      "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); td.style.background = '#f0f0f0'; td.style.color = '#333'; td.style.textAlign = 'right'; td.style.fontWeight = 'bold'; }"
    )

    rhandsontable(df, stretchH = "all") %>%
      hot_col("naam", type = "text") %>%
      hot_col("waarde_laag", type = "text", renderer = renderer_nl_money) %>%
      hot_col("verwachte_foutfractie",
              type = "text",
              renderer = renderer_nl_percent) %>%
      hot_col("fout_hoog", type = "text", renderer = renderer_nl_money) %>%
      hot_col("goed_hoog", type = "text", renderer = renderer_nl_money) %>%
      hot_col("n_hoog", type = "text", renderer = renderer_nl_general) %>%
      hot_col("ihr", type = "dropdown", source = as.list(risk_vec_ui)) %>%
      hot_col("ibr", type = "dropdown", source = as.list(risk_vec_ui)) %>%
      hot_col("car", type = "dropdown", source = as.list(risk_vec_ui)) %>%
      hot_col("materialiteit", type = "text", renderer = renderer_nl_percent) %>%
      hot_col(
        "n_basis",
        type = "text",
        readOnly = TRUE,
        renderer = renderer_readonly
      ) %>%
      hot_col(
        "n_definitief",
        type = "text",
        readOnly = TRUE,
        renderer = renderer_readonly
      ) %>%
      hot_col("k_laag",
              type = "text",
              readOnly = TRUE,
              renderer = renderer_readonly) %>%
      hot_col(
        "n_totaal",
        type = "text",
        readOnly = TRUE,
        renderer = renderer_readonly
      ) %>%
      hot_cols(colHeaders = koppen)
  })

  geplande_fout_val <- reactiveVal(NA)

  observeEvent(input$run_plan, {
    conf_val <- parse_dutch_num(input$plan_conf)
    mat_val <- parse_dutch_num(input$plan_totale_mat)
    validate(need(
      !is.na(conf_val),
      "Vul een geldig getal in voor totale zekerheid."
    ))
    validate(need(
      !is.na(mat_val),
      "Vul een geldig getal in voor totale materialiteit."
    ))

    raw_df <- plan_table_data()
    final_df <- raw_df %>% as_tibble() %>% filter(!is.na(naam) &
                                                    naam != "") %>%
      mutate(
        across(
          c(
            waarde_laag,
            verwachte_foutfractie,
            fout_hoog,
            goed_hoog,
            n_hoog,
            materialiteit
          ),
          parse_dutch_num
        ),
        ihr = toupper(ihr),
        ibr = toupper(ibr),
        car = toupper(car)
      )

    if (nrow(final_df) == 0) {
      showNotification("Vul tenminste één regel in.", type = "warning")
      return()
    }

    tryCatch({
      res <- plan_stratified(
        steekproeven = final_df,
        totale_materialiteit = mat_val,
        totale_zekerheid = conf_val,
        model = input$plan_model,
        methode = input$plan_methode
      )
      geplande_fout_val(attr(res, "geplande_max_fout_totaal"))

      res_formatted <- res %>% mutate(
        n_basis = format(
          round(n_basis, 0),
          big.mark = ".",
          decimal.mark = ","
        ),
        n_definitief = format(
          round(n_definitief, 0),
          big.mark = ".",
          decimal.mark = ","
        ),
        k_laag = format(
          round(k_laag, 2),
          big.mark = ".",
          decimal.mark = ","
        ),
        n_totaal = format(
          round(n_totaal, 0),
          big.mark = ".",
          decimal.mark = ","
        )
      )

      berekende_rijen <- nrow(res_formatted)
      raw_df$n_basis[1:berekende_rijen] <- res_formatted$n_basis
      raw_df$n_definitief[1:berekende_rijen] <- res_formatted$n_definitief
      raw_df$k_laag[1:berekende_rijen] <- res_formatted$k_laag
      raw_df$n_totaal[1:berekende_rijen] <- res_formatted$n_totaal
      plan_table_data(raw_df)

    }, error = function(e) {
      fout_tekst <- conditionMessage(e)
      if (fout_tekst == "")
        fout_tekst <- "Onbekende fout bij berekening. Controleer de R-console."
      showNotification(paste("Foutmelding:", fout_tekst),
                       type = "error",
                       duration = 15)
    })
  })

  output$text_plan_fout <- renderText({
    fout <- geplande_fout_val()
    if (is.na(fout))
      return("Nog geen berekening uitgevoerd.")
    paste0(
      "De theoretisch maximaal haalbare fout over de gehele populatie is: ",
      format(
        round(fout * 100, 3),
        decimal.mark = ",",
        nsmall = 3
      ),
      "%"
    )
  })
}

shinyApp(ui, server)
