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
risk_choices <- c("hoog (H)" = "H", "midden (M)" = "M", "laag (L)" = "L")
risk_vec_ui <- c("H", "M", "L")

# helper functie: parseer getallen strict volgens nl notatie
parse_dutch_num <- function(x) {
  if (is.numeric(x)) return(x)
  x_char <- as.character(x)
  parse_number(x_char, locale = locale(decimal_mark = ",", grouping_mark = "."))
}

# helper functie: maakt netjes een label met een (i) tooltip voor de UI
info_label <- function(tekst, tooltip_tekst) {
  tags$span(
    tekst,
    tooltip(
      bs_icon("info-circle", class = "ms-1", style = "font-size: 0.9em; color: #007bff;"),
      tooltip_tekst
    )
  )
}

ui <- navbarPage("evalstratified",
                 theme = bs_theme(version = 5),

                 # --- head: css only (js removed) ---
                 header = tags$head(
                   tags$style(HTML("
      /* force all handsontable cells to be white with black text */
      .handsontable td { background-color: #ffffff !important; color: #000000 !important; }
      .handsontable td.current { background-color: #e6f2ff !important; }
      /* right align text columns that act as numbers */
      .handsontable .htRight { text-align: right; }
    "))
                 ),

                 # -------------------------------------------------------------------------
                 # tab 1: haro nog nodige zekerheid
                 # -------------------------------------------------------------------------
                 tabPanel("nog nodige zekerheid",
                          sidebarLayout(
                            sidebarPanel(
                              h4("risico-inschatting"),
                              helpText("Bereken de nog benodigde zekerheid volgens HARo paragraaf B7.3.4."),
                              selectInput("haro_ihr", label = info_label("inherent risico (ihr):", "De inschatting van de kans op een materiële fout voordat interne beheersing is meegewogen."), choices = risk_choices),
                              selectInput("haro_ibr", label = info_label("interne beheersing (ibr):", "De verwachte effectiviteit van de interne beheersingsmaatregelen."), choices = risk_choices),
                              selectInput("haro_car", label = info_label("cijferanalyse (car):", "De mate van zekerheid die al is verkregen uit cijferbeoordelingen en analytische procedures."), choices = risk_choices)
                            ),
                            mainPanel(
                              h3("resultaat"),
                              verbatimTextOutput("res_haro"),
                              p("Dit is de zekerheid (fractie 0-1) die u nog uit detailcontroles moet halen.")
                            )
                          )
                 ),

                 # -------------------------------------------------------------------------
                 # tab 2: foutlozepostenequivalent
                 # -------------------------------------------------------------------------
                 tabPanel("foutlozepostenequivalent",
                          sidebarLayout(
                            sidebarPanel(
                              h4("risico & materialiteit"),
                              helpText("Bereken hoeveel foutloze posten uw risico-inschatting waard is."),
                              selectInput("fpe_ihr", label = info_label("inherent risico (ihr):", "De inschatting van de kans op een materiële fout voordat interne beheersing is meegewogen."), choices = risk_choices),
                              selectInput("fpe_ibr", label = info_label("interne beheersing (ibr):", "De verwachte effectiviteit van de interne beheersingsmaatregelen."), choices = risk_choices),
                              selectInput("fpe_car", label = info_label("cijferanalyse (car):", "De mate van zekerheid die al is verkregen uit cijferbeoordelingen en analytische procedures."), choices = risk_choices),
                              textInput("fpe_mat", label = info_label("materialiteit (fractie):", "De grens waarboven een afwijking als materieel wordt beschouwd (bijv. 0,01 voor 1%)."), value = "0,01")
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
                 tabPanel("evaluatie gestratificeerd",
                          sidebarLayout(
                            sidebarPanel(
                              h4("1. data invoer"),

                              radioButtons("input_method", "methode:", choices = c("handmatige invoer" = "manual", "csv-upload" = "upload")),

                              conditionalPanel(
                                condition = "input.input_method == 'upload'",
                                fileInput("file_strat", "upload csv-bestand:", accept = ".csv"),
                                downloadButton("download_template", "download csv voorbeeldbestand")
                              ),

                              conditionalPanel(
                                condition = "input.input_method == 'manual'",
                                helpText("Vul de tabel rechts in.")
                              ),

                              hr(),
                              h4("2. instellingen"),
                              textInput("strat_conf", label = info_label("zekerheid (0,95 = 95%):", "Het gewenste betrouwbaarheidsniveau voor de uiteindelijke evaluatie."), value = "0,95"),

                              numericInput("strat_mc", label = info_label("Monte Carlo iteraties:", "Aantal simulaties. Een hoger getal is nauwkeuriger, maar het rekenen duurt iets langer."), value = 100000, min = 1000, step = 10000),
                              numericInput("strat_seed", label = info_label("seed (startwaarde):", "Een vast startpunt voor de simulatie. Gebruik hetzelfde getal om later exact dezelfde uitkomst te reproduceren."), value = 1),
                              checkboxInput("strat_comp", label = info_label("vergelijk met andere methoden", "Berekent ter vergelijking ook de uitkomst volgens de traditionele (gewogen gemiddelde en gepoolde) methoden."), value = TRUE),
                              hr(),
                              actionButton("run_strat", "bereken evaluatie", class = "btn-success", width = "100%")
                            ),

                            mainPanel(
                              tabsetPanel(id = "hoofd_tabs",
                                          tabPanel("1. invoertabel",
                                                   br(),
                                                   conditionalPanel(
                                                     condition = "input.input_method == 'manual'",
                                                     helpText("Vul hieronder uw data in en klik links op 'bereken evaluatie'."),
                                                     rHandsontableOutput("hot_input")
                                                   )
                                          ),
                                          tabPanel("2. grafiek & hoofdresultaten", value = "tab_grafiek",
                                                   br(),
                                                   h3("convolutieresultaten"),
                                                   plotOutput("plot_strat_main", height = "400px"),
                                                   br(),
                                                   tableOutput("table_strat_main")
                                          ),
                                          tabPanel("3. details & vergelijking",
                                                   br(),
                                                   h3("vergelijking"),
                                                   tableOutput("table_strat_comp"),
                                                   hr(),
                                                   h4("data en resultaten per steekproef zoals verwerkt door het model"),
                                                   div(style = 'overflow-x: scroll', tableOutput("table_strat_input"))
                                          )
                              )
                            )
                          )
                 )
)

server <- function(input, output, session) {

  # Zorg dat het scherm automatisch naar de grafiek springt bij het klikken op berekenen
  observeEvent(input$run_strat, {
    updateTabsetPanel(session, "hoofd_tabs", selected = "tab_grafiek")
  })

  # --- logic tab 1 & 2 ---
  output$res_haro <- renderText({
    val <- haro_nog_nodige_zekerheid(input$haro_ihr, input$haro_ibr, input$haro_car)
    paste("nog nodige zekerheid:", format(round(val, 4), decimal.mark = ",", nsmall = 4))
  })

  output$res_fpe <- renderText({
    mat_val <- parse_dutch_num(input$fpe_mat)
    validate(need(!is.na(mat_val), "Vul een geldig getal in voor materialiteit."))
    val <- foutloze_posten_equivalent(input$fpe_ihr, input$fpe_ibr, input$fpe_car, mat_val)
    paste("foutlozepostenequivalent:", format(round(val, 0), big.mark = ".", decimal.mark = ","))
  })

  # --- logic tab 3 ---
  output$download_template <- downloadHandler(
    filename = function() { "steekproeven_template.csv" },
    content = function(file) {
      df <- tibble(
        naam = c("steekproef 1", "steekproef 2"), waarde_laag = c(1000000, 500000), n_laag = c(30, 20),
        k_laag = c(0, 1), fout_hoog = c(0, 5000), goed_hoog = c(0, 45000), ihr = c("H", "L"),
        ibr = c("H", "L"), car = c("H", "H"), materialiteit = c(0.01, 0.01)
      )
      write_csv2(df, file)
    }
  )

  # render the editable table
  output$hot_input <- renderRHandsontable({
    df <- data.frame(
      naam = rep(NA_character_, 8), waarde_laag = rep(NA_character_, 8), n_laag = rep(NA_character_, 8),
      k_laag = rep(NA_character_, 8), fout_hoog = rep("0", 8), goed_hoog = rep("0", 8),
      ihr = rep("H", 8), ibr = rep("H", 8), car = rep("H", 8), materialiteit = rep("0,01", 8),
      stringsAsFactors = FALSE
    )

    renderer_nl_money <- JS("function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL', { minimumFractionDigits: 2, maximumFractionDigits: 2 }); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }")
    renderer_nl_percent <- JS("function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL', { style: 'percent', minimumFractionDigits: 2 }); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }")
    renderer_nl_general <- JS("function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL'); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }")

    koppen <- c(
      "naam <span title='Naam of beschrijving van het stratum/steekproef'>&#9432;</span>",
      "waarde_laag <span title='Totale boekwaarde van de populatie waar deze steekproef uit getrokken is'>&#9432;</span>",
      "n_laag <span title='Aantal gecontroleerde posten in de steekproef'>&#9432;</span>",
      "k_laag <span title='Aantal gevonden foute posten'>&#9432;</span>",
      "fout_hoog <span title='Bekende foute waarde in de 100% gecontroleerde topmassa'>&#9432;</span>",
      "goed_hoog <span title='Bekende correcte waarde in de 100% gecontroleerde topmassa'>&#9432;</span>",
      "ihr <span title='Inherent risico voor dit specifieke stratum'>&#9432;</span>",
      "ibr <span title='Interne beheersing voor dit specifieke stratum'>&#9432;</span>",
      "car <span title='Zekerheid uit cijferanalyse voor dit specifieke stratum'>&#9432;</span>",
      "materialiteit <span title='Toegestane afwijking (fractie) voor dit stratum'>&#9432;</span>"
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
      hot_cols(colHeaders = koppen)
  })

  # reactive calculation
  strat_results <- eventReactive(input$run_strat, {
    conf_val <- parse_dutch_num(input$strat_conf)
    validate(need(!is.na(conf_val), "Vul een geldig getal in voor zekerheid."))

    final_df <- NULL

    if (input$input_method == "upload") {
      req(input$file_strat)
      tryCatch({
        final_df <- read_csv2(input$file_strat$datapath, show_col_types = FALSE)
        if(!"fout_hoog" %in% names(final_df)) final_df$fout_hoog <- 0
        if(!"goed_hoog" %in% names(final_df)) final_df$goed_hoog <- 0
        final_df <- final_df %>%
          mutate(
            waarde_laag = parse_dutch_num(waarde_laag), n_laag = parse_dutch_num(n_laag),
            k_laag = parse_dutch_num(k_laag), fout_hoog = parse_dutch_num(fout_hoog),
            goed_hoog = parse_dutch_num(goed_hoog), materialiteit = parse_dutch_num(materialiteit)
          )
      }, error = function(e) {
        showNotification("Kan het csv-bestand niet lezen. Zorg ervoor dat het is opgeslagen als CSV gescheiden door lijstscheidingsteken (vaak puntkomma).", type = "error")
        return(NULL)
      })

    } else {
      req(input$hot_input)
      raw_df <- hot_to_r(input$hot_input)
      names(raw_df) <- c("naam", "waarde_laag", "n_laag", "k_laag", "fout_hoog", "goed_hoog", "ihr", "ibr", "car", "materialiteit")

      final_df <- raw_df %>%
        as_tibble() %>%
        filter(!is.na(naam) & naam != "") %>%
        mutate(
          waarde_laag = parse_dutch_num(waarde_laag), n_laag = parse_dutch_num(n_laag),
          k_laag = parse_dutch_num(k_laag), fout_hoog = parse_dutch_num(fout_hoog),
          goed_hoog = parse_dutch_num(goed_hoog), materialiteit = parse_dutch_num(materialiteit)
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
        ihr = toupper(ihr), ibr = toupper(ibr), car = toupper(car)
      )

    tryCatch({
      res <- eval_stratified(
        steekproeven = final_df, zekerheid = conf_val, MC = as.integer(input$strat_mc),
        start = input$strat_seed, vergelijk = input$strat_comp
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
    req(res)
    req(res$kanskromme)

    d <- res$kanskromme
    totaal_geld <- res$populatie_totaal
    min_geld <- sum(res$steekproeven$fout_hoog)

    df_plot <- data.frame(x_geld = d$x * totaal_geld, y_dichtheid = d$y)
    df_plot <- df_plot %>% filter(x_geld >= min_geld)

    if(nrow(df_plot) > 0) {
      df_plot <- bind_rows(
        data.frame(x_geld = min_geld, y_dichtheid = 0), df_plot
      ) %>% arrange(x_geld, y_dichtheid)
    }

    mode_val <- max(res$mw_fout_convolutie_geld, min_geld)
    max_val <- max(res$max_fout_convolutie_geld, min_geld)
    zekerheid_pct <- res$invoer$zekerheid * 100

    ggplot(df_plot, aes(x = x_geld, y = y_dichtheid)) +
      geom_area(data = subset(df_plot, x_geld <= max_val), fill = "#e0e0e0", alpha = 0.7) +
      geom_line(color = "#2c3e50", linewidth = 1) +
      geom_vline(xintercept = mode_val, color = "blue", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = max_val, color = "red", linetype = "dashed", linewidth = 1) +
      scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, prefix = "€ ")) +
      labs(
        title = "kanskromme van de geprojecteerde fout",
        subtitle = paste0("blauwe lijn = meest waarschijnlijke fout | rode lijn = maximale fout (", zekerheid_pct, "% zekerheid)"),
        x = "fout in euro's", y = "relatieve kansdichtheid"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14), plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(), panel.grid.minor = element_blank()
      )
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
      metriek = c("meest waarschijnlijke fout (fractie)", "meest waarschijnlijke fout (geld)",
                  "maximale fout (fractie)", "maximale fout (geld)"),
      waarde = c(
        format(round(mw_frac, 5), decimal.mark = ",", nsmall = 5),
        format(round(mw_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2),
        format(round(max_frac, 5), decimal.mark = ",", nsmall = 5),
        format(round(max_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2)
      )
    )
  })

  output$table_strat_comp <- renderTable({
    res <- strat_results()
    req(res)
    if(is.null(res$vergelijk_met)) return(tibble(info = "Geen vergelijking gevraagd."))
    comp <- res$vergelijk_met
    tibble(
      scenario = c("los (gewogen gemiddelde)", "als 1 (gepoolde data)"),
      `max fout (geld)` = c(
        format(round(comp$max_fout_los_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2),
        format(round(comp$max_fout_als1_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2)
      )
    )
  })

  output$table_strat_input <- renderTable({
    res <- strat_results()
    req(res)

    df_disp <- res$steekproeven

    geld_cols <- c("waarde_laag", "fout_hoog", "goed_hoog", "waarde_hoog", "waarde_populatie")
    for (col in geld_cols) {
      if(col %in% names(df_disp)) {
        df_disp[[col]] <- format(round(df_disp[[col]], 2), big.mark = ".", decimal.mark = ",", nsmall = 2, scientific = FALSE)
      }
    }

    num_cols <- c("n_laag", "k_laag", "n_totaal", "extra_foutloze_posten")
    for (col in num_cols) {
      if(col %in% names(df_disp)) {
        df_disp[[col]] <- format(round(df_disp[[col]], 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)
      }
    }

    frac_cols <- c("materialiteit", "mw_fout", "max_fout")
    for (col in frac_cols) {
      if(col %in% names(df_disp)) {
        df_disp[[col]] <- format(round(df_disp[[col]], 5), big.mark = ".", decimal.mark = ",", scientific = FALSE)
      }
    }

    df_disp %>% select(-any_of("n_hoog"))
  })
}

shinyApp(ui, server)
