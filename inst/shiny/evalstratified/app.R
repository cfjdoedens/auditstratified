library(shiny)
library(evalstratified)
library(tibble)
library(dplyr)
library(readr)
library(rhandsontable)
library(htmlwidgets)

# Define risk options used in multiple tabs
risk_choices <- c("Hoog (H)" = "H", "Midden (M)" = "M", "Laag (L)" = "L")
risk_vec <- c("H", "M", "L")

# Helper functie: Parseer getallen STRICT volgens NL notatie
# - input "0,01" -> getal 0.01
# - input "1.000" -> getal 1000
parse_dutch_num <- function(x) {
  if (is.numeric(x)) return(x)

  # Zet om naar text voor veiligheid
  x_char <- as.character(x)

  # STRICTE NL conversie: komma is decimaal, punt is duizendtal.
  parse_number(x_char, locale = locale(decimal_mark = ",", grouping_mark = "."))
}

ui <- navbarPage("EvalStratified",

                 # --- HEAD: CSS ONLY (JS removed) ---
                 header = tags$head(
                   tags$style(HTML("
      /* Force all Handsontable cells to be white with black text */
      .handsontable td {
        background-color: #ffffff !important;
        color: #000000 !important;
      }
      .handsontable td.current {
        background-color: #e6f2ff !important;
      }
      /* Right align text columns that act as numbers */
      .handsontable .htRight {
        text-align: right;
      }
    "))
                 ),

                 # -------------------------------------------------------------------------
                 # TAB 1: Haro Nog Nodige Zekerheid
                 # -------------------------------------------------------------------------
                 tabPanel("Nog Nodige Zekerheid",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Risico Inschatting"),
                              helpText("Bereken de nog benodigde zekerheid volgens HARo paragraaf B7.3.4."),
                              selectInput("haro_ihr", "Inherent Risico (ihr):", choices = risk_choices),
                              selectInput("haro_ibr", "Interne Beheersing (ibr):", choices = risk_choices),
                              selectInput("haro_car", "Cijferanalyse (car):", choices = risk_choices)
                            ),
                            mainPanel(
                              h3("Resultaat"),
                              verbatimTextOutput("res_haro"),
                              p("Dit is de zekerheid (fractie 0-1) die u nog uit detailcontroles moet halen.")
                            )
                          )
                 ),

                 # -------------------------------------------------------------------------
                 # TAB 2: Foutloze Posten Equivalent
                 # -------------------------------------------------------------------------
                 tabPanel("Foutloze Posten Equivalent",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Risico & Materialiteit"),
                              helpText("Bereken hoeveel foutloze posten uw risico-inschatting waard is."),
                              selectInput("fpe_ihr", "Inherent Risico (ihr):", choices = risk_choices),
                              selectInput("fpe_ibr", "Interne Beheersing (ibr):", choices = risk_choices),
                              selectInput("fpe_car", "Cijferanalyse (car):", choices = risk_choices),
                              textInput("fpe_mat", "Materialiteit (fractie):", value = "0,01")
                            ),
                            mainPanel(
                              h3("Resultaat"),
                              verbatimTextOutput("res_fpe"),
                              p("Aantal posten dat overeenkomt met de verlaagde risico's.")
                            )
                          )
                 ),

                 # -------------------------------------------------------------------------
                 # TAB 3: Eval Stratified (Main Analysis)
                 # -------------------------------------------------------------------------
                 tabPanel("Evaluatie Gestratificeerd",
                          sidebarLayout(
                            sidebarPanel(
                              h4("1. Data Invoer"),

                              radioButtons("input_method", "Methode:",
                                           choices = c("Handmatige Invoer" = "manual",
                                                       "CSV Upload" = "upload")),

                              conditionalPanel(
                                condition = "input.input_method == 'upload'",
                                fileInput("file_strat", "Upload CSV Bestand:", accept = ".csv"),
                                downloadButton("download_template", "Download CSV Template")
                              ),

                              conditionalPanel(
                                condition = "input.input_method == 'manual'",
                                helpText("Vul de tabel rechts in.")
                              ),

                              hr(),
                              h4("2. Instellingen"),
                              textInput("strat_conf", "Zekerheid (0,95 = 95%):", value = "0,95"),

                              numericInput("strat_mc", "Monte Carlo Iteraties:", value = 100000, min = 1000, step = 10000),
                              numericInput("strat_seed", "Seed (Startwaarde):", value = 1),
                              checkboxInput("strat_comp", "Vergelijk met andere methoden", value = TRUE),
                              hr(),
                              actionButton("run_strat", "Bereken Evaluatie", class = "btn-success", width = "100%")
                            ),

                            mainPanel(
                              conditionalPanel(
                                condition = "input.input_method == 'manual'",
                                h4("Invoertabel"),
                                rHandsontableOutput("hot_input"),
                                hr()
                              ),

                              tabsetPanel(
                                tabPanel("Resultaten",
                                         h3("Convolutie Resultaten"),
                                         tableOutput("table_strat_main"),
                                         h3("Vergelijking"),
                                         tableOutput("table_strat_comp")
                                ),
                                tabPanel("Samengenomen steekproeven",
                                         h4("Data en resultaten per steekproef zoals verwerkt door het model"),
                                         tableOutput("table_strat_input")
                                )
                              )
                            )
                          )
                 )
)

server <- function(input, output, session) {

  # --- LOGIC TAB 1 & 2 ---
  output$res_haro <- renderText({
    val <- haro_nog_nodige_zekerheid(input$haro_ihr, input$haro_ibr, input$haro_car)
    formatted_val <- format(round(val, 4), decimal.mark = ",", nsmall = 4)
    paste("Nog nodige zekerheid:", formatted_val)
  })

  output$res_fpe <- renderText({
    mat_val <- parse_dutch_num(input$fpe_mat)
    validate(need(!is.na(mat_val), "Vul een geldig getal in voor materialiteit"))

    val <- foutloze_posten_equivalent(input$fpe_ihr, input$fpe_ibr, input$fpe_car, mat_val)
    formatted_val <- format(round(val, 0), big.mark = ".", decimal.mark = ",")
    paste("Equivalent aantal foutloze posten:", formatted_val)
  })

  # --- LOGIC TAB 3 ---

  output$download_template <- downloadHandler(
    filename = function() { "steekproeven_template.csv" },
    content = function(file) {
      df <- tibble(
        naam = c("Steekproef 1", "Steekproef 2"), w = c(1000000, 500000),
        n = c(30, 20), k = c(0, 1),
        ihr = c("H", "L"), ibr = c("H", "L"), car = c("H", "H"),
        materialiteit = c(0.01, 0.01)
      )
      write_csv(df, file)
    }
  )

  # 2. Render the Editable Table
  output$hot_input <- renderRHandsontable({

    # Initiele data als TEXT (strings) om de editor te dwingen strings te tonen
    df <- data.frame(
      naam = rep(NA_character_, 8),
      w = rep(NA_character_, 8),
      n = rep(NA_character_, 8),
      k = rep(NA_character_, 8),
      ihr = rep("H", 8),
      ibr = rep("H", 8),
      car = rep("H", 8),
      materialiteit = rep("0,01", 8), # Default als NL string
      stringsAsFactors = FALSE
    )

    # --- RENDERER 1: GELD (1.000,00) ---
    renderer_nl_money <- JS("
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);

        var numVal = NaN;
        if (value !== null && value !== void 0 && value !== '') {
           var str = value.toString().replace(/\\./g, '').replace(',', '.');
           numVal = parseFloat(str);
        }

        if (!isNaN(numVal)) {
           td.innerHTML = numVal.toLocaleString('nl-NL', {
              minimumFractionDigits: 2,
              maximumFractionDigits: 2
           });
        }
        td.style.background = 'white';
        td.style.color = 'black';
        td.style.textAlign = 'right';
      }
    ")

    # --- RENDERER 2: PERCENTAGES (1,00%) ---
    renderer_nl_percent <- JS("
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);

        var numVal = NaN;
        if (value !== null && value !== void 0 && value !== '') {
           var str = value.toString().replace(/\\./g, '').replace(',', '.');
           numVal = parseFloat(str);
        }

        if (!isNaN(numVal)) {
           td.innerHTML = numVal.toLocaleString('nl-NL', {
              style: 'percent',
              minimumFractionDigits: 2
           });
        }
        td.style.background = 'white';
        td.style.color = 'black';
        td.style.textAlign = 'right';
      }
    ")

    # --- RENDERER 3: GEWOON GETAL (1.000 of 30) ---
    renderer_nl_general <- JS("
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);

        var numVal = NaN;
        if (value !== null && value !== void 0 && value !== '') {
           var str = value.toString().replace(/\\./g, '').replace(',', '.');
           numVal = parseFloat(str);
        }

        if (!isNaN(numVal)) {
           td.innerHTML = numVal.toLocaleString('nl-NL');
        }
        td.style.background = 'white';
        td.style.color = 'black';
        td.style.textAlign = 'right';
      }
    ")

    rhandsontable(df, stretchH = "all") %>%
      hot_col("naam", type = "text") %>%

      hot_col("w", type = "text", renderer = renderer_nl_money) %>%
      hot_col("n", type = "text", renderer = renderer_nl_general) %>%
      hot_col("k", type = "text", renderer = renderer_nl_general) %>%

      hot_col("ihr", type = "dropdown", source = as.list(risk_vec)) %>%
      hot_col("ibr", type = "dropdown", source = as.list(risk_vec)) %>%
      hot_col("car", type = "dropdown", source = as.list(risk_vec)) %>%

      hot_col("materialiteit", type = "text", renderer = renderer_nl_percent)
  })

  # 3. Reactive Calculation
  strat_results <- eventReactive(input$run_strat, {

    conf_val <- parse_dutch_num(input$strat_conf)
    validate(need(!is.na(conf_val), "Vul een geldig getal in voor Zekerheid"))

    final_df <- NULL

    if (input$input_method == "upload") {
      req(input$file_strat)
      tryCatch({
        final_df <- read_csv(input$file_strat$datapath, show_col_types = FALSE)
      }, error = function(e) {
        showNotification("Kan CSV bestand niet lezen.", type = "error")
        return(NULL)
      })

    } else {
      req(input$hot_input)
      raw_df <- hot_to_r(input$hot_input)

      # FIX: Parseer de Strings naar Getallen voor de berekening
      final_df <- raw_df %>%
        as_tibble() %>%
        filter(!is.na(naam) & naam != "") %>%
        mutate(
          w = parse_dutch_num(w),
          n = parse_dutch_num(n),
          k = parse_dutch_num(k),
          materialiteit = parse_dutch_num(materialiteit)
        )

      if (nrow(final_df) == 0) {
        showNotification("Vul tenminste één regel in met een 'naam'.", type = "warning")
        return(NULL)
      }
    }

    tryCatch({
      res <- eval_stratified(
        steekproeven = final_df,
        zekerheid = conf_val,
        MC = as.integer(input$strat_mc),
        start = input$strat_seed,
        vergelijk = input$strat_comp
      )
      return(res)
    }, error = function(e) {
      showNotification(paste("Fout in berekening:", e$message), type = "error")
      return(NULL)
    })
  })

  # 4. Output Tables
  output$table_strat_main <- renderTable({
    res <- strat_results()
    req(res)
    tibble(
      Metriek = c("Meest Waarschijnlijke Fout (fractie)", "Meest Waarschijnlijke Fout (geld)",
                  "Maximale Fout (fractie)", "Maximale Fout (geld)"),
      Waarde = c(
        format(round(res$mw_fout_convolutie, 5), decimal.mark = ",", nsmall = 5),
        format(round(res$mw_fout_convolutie_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2),
        format(round(res$max_fout_convolutie, 5), decimal.mark = ",", nsmall = 5),
        format(round(res$max_fout_convolutie_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2)
      )
    )
  })

  output$table_strat_comp <- renderTable({
    res <- strat_results()
    req(res)
    if(is.null(res$vergelijk_met)) return(tibble(Info = "Geen vergelijking gevraagd."))
    comp <- res$vergelijk_met
    tibble(
      Scenario = c("Los (Gewogen gemiddelde)", "Als 1 (Gepoolde data)"),
      `Max Fout (Geld)` = c(
        format(round(comp$max_fout_los_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2),
        format(round(comp$max_fout_als1_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2)
      )
    )
  })

  # FIX VOOR TAB "SAMENGENOMEN STEEKPROEVEN"
  output$table_strat_input <- renderTable({
    res <- strat_results()
    req(res)

    # Kopieer dataframe
    df_disp <- res$steekproeven

    # 1. Format Waarde (w)
    if("w" %in% names(df_disp)) {
      df_disp$w <- format(round(df_disp$w, 2), big.mark = ".", decimal.mark = ",", nsmall = 2, scientific = FALSE)
    }

    # 2. Format Materialiteit
    if("materialiteit" %in% names(df_disp)) {
      df_disp$materialiteit <- format(df_disp$materialiteit, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }

    # 3. Format Integers (AANGEPAST: nu met NL formattering)
    if("n" %in% names(df_disp)) {
      df_disp$n <- format(df_disp$n, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }
    if("k" %in% names(df_disp)) {
      df_disp$k <- format(df_disp$k, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }

    # 4. Extra Foutloze Posten (0 decimalen)
    if("extra_foutloze_posten" %in% names(df_disp)) {
      df_disp$extra_foutloze_posten <- format(round(df_disp$extra_foutloze_posten, 0),
                                              big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }

    # 5. Fracties (5 decimalen)
    if("mw_fout" %in% names(df_disp)) {
      df_disp$mw_fout <- format(round(df_disp$mw_fout, 5), big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }
    if("max_fout" %in% names(df_disp)) {
      df_disp$max_fout <- format(round(df_disp$max_fout, 5), big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }

    df_disp
  })
}

shinyApp(ui, server)
