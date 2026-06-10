source("global.R")

# Definieer de gebruikersinterface van de applicatie.
ui <- navbarPage(
  "auditstratified",
  theme = bs_theme(version = 5),

  # CSS en scripts voor de tabel-opmaak en vraagteken-cursors bij de tabelkoppen.
  header = tags$head(
    shinyjs::useShinyjs(),
    tags$style(
      HTML(
        "
        /* Toon de zandloper alleen als de berekening langer dan 200ms duurt. */
        html.rekenen-bezig, html.rekenen-bezig * {
          cursor: wait !important;
        }
        .handsontable, .handsontable * {
          box-sizing: content-box;
        }
        .handsontable td {
          background-color: #ffffff;
          color: #000000;
        }
        .handsontable th {
          cursor: help !important;
        }
        .handsontable .readonly-cell {
          background-color: #eeeeee !important;
          color: #555555 !important;
          font-weight: bold;
        }
        .handsontable td.current { background-color: #e6f2ff !important; }
        .handsontable .htRight { text-align: right; }
        .voortgang-tekst-container {
          padding: 10px;
          background-color: #f8f9fa;
          border-left: 4px solid #198754;
          margin-bottom: 15px;
          font-weight: 500;
        }
        "
      )
    ),
    tags$script(
      HTML(
        "
        $(document).on('shiny:tabshown', function(e) {
          setTimeout(function() {
            $('.html-widget').each(function() {
              var widget = HTMLWidgets.getInstance(this);
              if (widget && widget.hot) { widget.hot.render(); }
            });
            window.dispatchEvent(new Event('resize'));
          }, 150);
        });

        /* Trigger de formattering pas als de gebruiker het veld verlaat (focusout). */
        $(document).on('focusout', '#plan_totale_mat, #plan_conf, #plan_klim_granulariteit', function() {
          Shiny.setInputValue('format_plan_sidebar', Math.random(), {priority: 'event'});
        });

        /* Europese notatie op de vertraging-slider via MutationObserver. */
        $(function() {
          var parent = document.querySelector('#plan_vertraging');
          if (!parent) parent = document.querySelector('[id$=\"plan_vertraging\"]');
          if (!parent) return;
          var container = parent.closest('.shiny-input-container') || parent.parentElement;
          function kommafix() {
            container.querySelectorAll('.irs-min, .irs-max, .irs-single, .irs-from, .irs-to, .irs-grid-text').forEach(function(el) {
              var t = el.textContent;
              if (t && t.indexOf('.') !== -1) el.textContent = t.replace(/[.]/g, ',');
            });
          }
          new MutationObserver(kommafix).observe(container, { childList: true, subtree: true, characterData: true });
        });
        "
      )
    )
  ),

  # Tab 1: Nog nodige zekerheid
  tabPanel(
    "nog nodige zekerheid",
    br(),
    fluidRow(
      column(
        width = 3,
        h4("risico-inschatting"),
        selectInput("haro_ihr", info_label("IHR:", "InHerent Risico"), choices = risk_choices),
        selectInput("haro_ibr", info_label("IBR:", "Interne BeheersingsRisico"), choices = risk_choices),
        selectInput("haro_car", info_label("CAR:", "CijferAnalyseRisico"), choices = risk_choices)
      ),
      column(
        width = 9,
        h4("Resultaat"),
        textOutput("res_haro")
      )
    )
  ),

  # Tab 2: Foutlozepostenequivalent
  tabPanel(
    "foutlozepostenequivalent",
    br(),
    fluidRow(
      column(
        width = 3,
        h4("risico-inschatting"),
        selectInput("fpe_ihr", info_label("IHR:", "InHerent Risico"), choices = risk_choices),
        selectInput("fpe_ibr", info_label("IBR:", "Interne BeheersingsRisico"), choices = risk_choices),
        selectInput("fpe_car", info_label("CAR:", "CijferAnalyseRisico"), choices = risk_choices),
        textInput("fpe_mat", info_label("Materialiteit:", "De materialiteitsgrens als fractie (bijv. 0,01)."), value = "0,01")
      ),
      column(
        width = 9,
        h4("Resultaat"),
        verbatimTextOutput("res_fpe"),
        p("Aantal posten dat overeenkomt met de verlaagde risico's.")
      )
    )
  ),

  # Tab 3: Evaluatie gestratificeerd
  tabPanel(
    "evaluatie gestratificeerd",
    sidebarLayout(
      sidebar = sidebarPanel(
        width = 3,
        h4("instellingen"),
        textInput("strat_conf", info_label("zekerheid:", "De gewenste statistische zekerheid (bijv. 0,95 = 95%)."), value = "0,95"),
        radioButtons("strat_model", "model:", choices = c("binomiaal", "poisson"), inline = TRUE),
        textInput("strat_gran", info_label("granulariteit:", "Aantal stappen gebruikt in de FFT-berekening voor de convolutie van de kanskrommen behorend bij de strata. Meer stappen is nauwkeuriger maar trager."), value = "10.000"),
        hr(),
        actionButton("run_strat", "bereken evaluatie", class = "btn-success w-100")
      ),
      mainPanel(
        tabsetPanel(
          id = "hoofd_tabs",
          tabPanel("1. invoertabel", br(), rHandsontableOutput("hot_input")),
          tabPanel(
            "2. resultaten",
            value = "tab_grafiek",
            br(),
            plotOutput("plot_strat_main"),
            br(),
            tableOutput("table_strat_main")
          )
        )
      )
    )
  ),

  # Tab 4: Planning gestratificeerd
  tabPanel(
    "planning gestratificeerd",
    sidebarLayout(
      sidebar = sidebarPanel(
        width = 3,
        h4("instellingen"),
        textInput("plan_totale_mat", info_label("materialiteit:", "De totale materialiteitsgrens als fractie (bijv. 0,01) of als bedrag in euro's."), "0,01"),
        textInput("plan_conf", info_label("zekerheid:", "De gewenste statistische zekerheid (bijv. 0,95 = 95%)."), "0,95"),
        radioButtons("plan_model", "model:", choices = c("binomiaal", "poisson"), inline = TRUE),
        textInput("plan_klim_granulariteit", info_label("granulariteit:", "Aantal stappen gebruikt in de FFT-berekeningen voor de convolutie van de kanskrommen behorend bij de strata. Meer stappen is nauwkeuriger maar trager."), "10.000"),

        # Bepaal de live-vertraging via een schuifknop met een bereik van nul tot twee seconden.
        sliderInput("plan_vertraging", "Live-vertraging (sec per stap):", min = 0, max = 2.0, value = 0.3, step = 0.05, sep = ""),
        actionButton("run_plan", "bereken planning", class = "btn-success w-100")
      ),
      mainPanel(
        h3("planning"),
        uiOutput("plan_live_status"),
        helpText("Vul de witte velden (links) in en klik op 'bereken planning'. De grijze velden (rechts) tikken live omhoog."),
        rHandsontableOutput("hot_plan_input")
      )
    )
  )
)

# Definieer de serverlogica van de applicatie.
server <- function(input, output, session) {
  # Laad de JavaScript renderers in voor de tabellen.
  source("js_renderers.R", local = TRUE)

  # Initialiseer de reactieve data en statusvariabelen voor de live-planning.
  {
    plan_table_data <- reactiveVal({
      data.frame(
        naam = rep(NA_character_, 8),
        waarde_laag = rep(NA_character_, 8),
        verwachte_foutfractie = rep("0,001", 8),
        fout_hoog = rep("0", 8),
        goed_hoog = rep("0", 8),
        n_hoog = rep("0", 8),
        ihr = rep("H", 8),
        ibr = rep("H", 8),
        car = rep("H", 8),
        materialiteit = rep("0,01", 8),
        n_laag = rep("", 8),
        n_laag_extra = rep("", 8),
        n_laag_tot = rep("", 8),
        n_totaal = rep("", 8),
        stringsAsFactors = FALSE
      )
    })

    live_status_tekst <- reactiveVal("Systeem is gereed voor berekening.")
    live_berekening_actief <- reactiveVal(FALSE)
    live_strata_data <- reactiveVal(NULL)
    live_ruwe_tabel <- reactiveVal(NULL)
    live_iteratie <- reactiveVal(0)
    live_reken_mat <- reactiveVal(0)
    live_huidige_fout <- reactiveVal(1.0)
  }

  # Verzorg de serverlogica voor het tabblad over nog nodige zekerheid.
  output$res_haro <- renderText({
    req(input$haro_ihr, input$haro_ibr, input$haro_car)

    # Bereken de nog nodige zekerheid.
    val <- haro_nog_nodige_zekerheid(input$haro_ihr, input$haro_ibr, input$haro_car)
    paste("nog nodige zekerheid:", format(round(val, 4), decimal.mark = ","))
  })

  # Verzorg de serverlogica voor het tabblad over het foutlozepostenequivalent.
  output$res_fpe <- renderText({
    req(input$fpe_ihr, input$fpe_ibr, input$fpe_car, input$fpe_mat)
    mat_val <- parse_dutch_num(input$fpe_mat)
    if (is.na(mat_val)) return("Ongeldige materialiteit")
    val <- foutloze_posten_equivalent(input$fpe_ihr, input$fpe_ibr, input$fpe_car, mat_val)
    paste("foutlozepostenequivalent:", round(val, 0))
  })

  # Server logica voor Tab 3: evaluatie gestratificeerd.
  output$hot_input <- renderRHandsontable({
    # Definieer de structuur van de invoertabel voor de evaluatie met zuivere kolomnamen en tooltips.
    {
      df <- data.frame(
        naam = rep(NA_character_, 8),
        waarde_laag = rep(NA_character_, 8),
        n_laag = rep(NA_character_, 8),
        k_laag = rep(NA_character_, 8),
        fout_hoog = rep("0", 8),
        goed_hoog = rep("0", 8),
        n_hoog = rep("0", 8),
        ihr = rep("H", 8),
        ibr = rep("H", 8),
        car = rep("H", 8),
        materialiteit = rep("0,01", 8),
        stringsAsFactors = FALSE
      )

      koppen_eval <- c(
        "naam", "waarde_laag", "n_laag", "k_laag",
        "fout_hoog", "goed_hoog", "n_hoog",
        "ihr", "ibr", "car", "materialiteit"
      )

      hulpteksten_eval <- c(
        "De unieke naam van het stratum.",
        "De totale geldswaarde van de posten in het laagstratum.",
        "Het aantal posten getrokken uit het laagstratum.",
        "De som van de foutfracties van de uit het laagstratum getrokken posten.",
        "Het totale foutbedrag van het hoogstratum.",
        "Het totale goedbedrag van het hoogstratum.",
        "Het aantal posten in het hoogstratum.",
        "Inherent risico voor het laagstratum.",
        "Interne beheersingsrisico voor het laagstratum.",
        "Cijferanalyserisico voor het laagstratum.",
        "De materialiteit voor dit hele stratum (dus laagstratum + hoogstratum samen)."
      )

      # Bouw de afterGetColHeader-hook die tooltips op de kolomkoppen zet.
      tooltip_json_eval <- paste0("[", paste0('"', hulpteksten_eval, '"', collapse = ","), "]")
      hook_tooltips_eval <- JS(sprintf(
        "function(col, TH) { var tips = %s; if (tips[col]) TH.title = tips[col]; }",
        tooltip_json_eval
      ))
    }

    # Bouw de widget op met tooltip-hulpteksten op de kolomkoppen.
    rhandsontable(df, stretchH = "all", colHeaders = koppen_eval) |>
      hot_table(stretchH = "all", afterGetColHeader = hook_tooltips_eval) |>
      hot_col(col = 2, renderer = renderer_nl_money) |>
      hot_col(col = 3, renderer = renderer_nl_general) |>
      hot_col(col = 4, renderer = renderer_nl_general) |>
      hot_col(col = 5, renderer = renderer_nl_money) |>
      hot_col(col = 6, renderer = renderer_nl_money) |>
      hot_col(col = 7, renderer = renderer_nl_general) |>
      hot_col(col = 8, type = "dropdown", source = risk_vec_ui, strict = TRUE) |>
      hot_col(col = 9, type = "dropdown", source = risk_vec_ui, strict = TRUE) |>
      hot_col(col = 10, type = "dropdown", source = risk_vec_ui, strict = TRUE) |>
      hot_col(col = 11, renderer = renderer_nl_percent)
  })

  strat_results <- eventReactive(input$run_strat, {
    req(input$hot_input)
    raw_df <- hot_to_r(input$hot_input)

    final_df <- raw_df |>
      as_tibble() |>
      filter(!is.na(naam) & naam != "") |>
      mutate(across(c("waarde_laag", "n_laag", "k_laag", "fout_hoog", "goed_hoog", "n_hoog", "materialiteit"), parse_dutch_num)) |>
      mutate(across(c("ihr", "ibr", "car"), toupper)) |>
      mutate(waarde_hoog = fout_hoog + goed_hoog, waarde_populatie = waarde_laag + waarde_hoog, n_totaal = n_laag + n_hoog)

    validate(need(nrow(final_df) > 0, "Vul data in."))

    eval_stratified(
      final_df,
      model = input$strat_model,
      zekerheid = parse_dutch_num(input$strat_conf),
      methode = "FFT samen",
      granulariteit = parse_dutch_num(input$strat_gran)
    )
  })

  observeEvent(input$run_strat, {
    updateTabsetPanel(session, "hoofd_tabs", selected = "tab_grafiek")
  })

  output$plot_strat_main <- renderPlot({
    req(strat_results())
    plot_kanskromme(strat_results())
  })

  output$table_strat_main <- renderTable({
    res <- strat_results()
    req(res)
    data.frame(
      metriek = c("mw fout", "max fout"),
      waarde = c(
        paste0("€ ", format(round(res$mw_fout_convolutie_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2, scientific = FALSE)),
        paste0("€ ", format(round(res$max_fout_convolutie_geld, 2), big.mark = ".", decimal.mark = ",", nsmall = 2, scientific = FALSE))
      )
    )
  })

  # Server logica voor Tab 4: planning gestratificeerd (Live-klimmer).
  output$hot_plan_input <- renderRHandsontable({
    # Definieer de 14 exacte kolomkoppen en hulpteksten synchroon met de datastructuur.
    {
      df <- plan_table_data()

      koppen_plan <- c(
        "naam", "waarde_laag", "verwacht_fout%", "fout_hoog",
        "goed_hoog", "n_hoog", "ihr", "ibr", "car",
        "materialiteit", "n_laag", "n_laag_extra", "n_laag_tot", "n_totaal"
      )

      hulpteksten_plan <- c(
        "De unieke naam van het stratum.",
        "De totale geldswaarde van de posten in het laagstratum.",
        "De verwachte foutfractie binnen dit stratum.",
        "Het totale foutbedrag van het hoogstratum.",
        "Het totale goedbedrag van het hoogstratum.",
        "Het aantal posten in het hoogstratum.",
        "Inherent risico voor het laagstratum.",
        "Interne beheersingsrisico voor het laagstratum.",
        "Cijferanalyserisico voor het laagstratum.",
        "De materialiteit voor dit hele stratum (dus laagstratum + hoogstratum samen).",
        "Het aantal steken uit het laagstratum om onder de materialiteit van dit hele stratum te blijven.",
        "De extra posten om te steken uit dit stratum die de planner heeft toegevoegd voor verlagen van de totale foutfractie (van alle strata samen dus) om onder de totale materialiteit (van alle strata samen dus) te komen.",
        "n_laag + n_laag_extra",
        "n_laag_tot + n_hoog"
      )

      renderer_readonly <- JS(
        "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); td.className = 'readonly-cell htRight'; }"
      )

      # Bouw de naadloze afterGetColHeader-hook die de tooltips op de TH-elementen injecteert.
      tooltip_json <- paste0("[", paste0('"', hulpteksten_plan, '"', collapse = ","), "]")
      hook_tooltips <- JS(sprintf(
        "function(col, TH) { var tips = %s; if (tips[col]) TH.title = tips[col]; }",
        tooltip_json
      ))
    }

    # Koppel de complete set titels via colHeaders en reactiveer de interactieve hovers.
    rhandsontable(df, stretchH = "all", colHeaders = koppen_plan) |>
      hot_table(stretchH = "all", afterGetColHeader = hook_tooltips) |>
      hot_col(col = 2, renderer = renderer_nl_money) |>
      hot_col(col = 3, renderer = renderer_nl_percent) |>
      hot_col(col = 4, renderer = renderer_nl_money) |>
      hot_col(col = 5, renderer = renderer_nl_money) |>
      hot_col(col = 6, renderer = renderer_nl_general) |>
      hot_col(col = 7, type = "dropdown", source = risk_vec_ui, strict = TRUE) |>
      hot_col(col = 8, type = "dropdown", source = risk_vec_ui, strict = TRUE) |>
      hot_col(col = 9, type = "dropdown", source = risk_vec_ui, strict = TRUE) |>
      hot_col(col = 10, renderer = renderer_nl_percent) |>
      hot_col(col = 11, readOnly = TRUE, renderer = renderer_readonly) |>
      hot_col(col = 12, readOnly = TRUE, renderer = renderer_readonly) |>
      hot_col(col = 13, readOnly = TRUE, renderer = renderer_readonly) |>
      hot_col(col = 14, readOnly = TRUE, renderer = renderer_readonly)
  })

  observeEvent(input$format_plan_sidebar, {
    mat_val <- parse_dutch_num(input$plan_totale_mat)
    if (!is.na(mat_val)) {
      fmt_mat <- if (mat_val > 1) paste0("€ ", format(mat_val, big.mark = ".", decimal.mark = ",", scientific = FALSE)) else format(mat_val, decimal.mark = ",", scientific = FALSE)
      if (input$plan_totale_mat != fmt_mat) updateTextInput(session, "plan_totale_mat", value = fmt_mat)
    }
    conf_val <- parse_dutch_num(input$plan_conf)
    if (!is.na(conf_val)) {
      fmt_conf <- format(conf_val, decimal.mark = ",", scientific = FALSE)
      if (input$plan_conf != fmt_conf) updateTextInput(session, "plan_conf", value = fmt_conf)
    }
    klim_gran_val <- parse_dutch_num(input$plan_klim_granulariteit)
    if (!is.na(klim_gran_val)) {
      fmt_klim_gran <- format(klim_gran_val, big.mark = ".", decimal.mark = ",", scientific = FALSE)
      if (input$plan_klim_granulariteit != fmt_klim_gran) updateTextInput(session, "plan_klim_granulariteit", value = fmt_klim_gran)
    }
  })

  observeEvent(input$run_plan, {
    req(input$hot_plan_input)
    raw_df <- hot_to_r(input$hot_plan_input)

    final_df <- raw_df |>
      as_tibble() |>
      mutate(across(c("waarde_laag", "verwachte_foutfractie", "fout_hoog", "goed_hoog", "n_hoog", "materialiteit"), parse_dutch_num)) |>
      mutate(across(c("ihr", "ibr", "car"), toupper)) |>
      filter(!is.na(naam) & naam != "" & !is.na(waarde_laag)) |>
      mutate(waarde_hoog = fout_hoog + goed_hoog, waarde_populatie = waarde_laag + waarde_hoog) |>
      mutate(materialiteit = ifelse(.data$materialiteit > 1 & .data$waarde_populatie > 0, .data$materialiteit / .data$waarde_populatie, .data$materialiteit))

    if (nrow(final_df) == 0) {
      showNotification("Vul data in.", type = "warning")
      return()
    }

    # Parseer de sidebar-invoer voor gebruik in de berekening.
    mat_val <- parse_dutch_num(input$plan_totale_mat)
    conf_val <- parse_dutch_num(input$plan_conf)
    klim_gran_val <- parse_dutch_num(input$plan_klim_granulariteit)

    totale_pop_waarde <- sum(final_df$waarde_populatie, na.rm = TRUE)
    reken_mat <- ifelse(mat_val > 1 && totale_pop_waarde > 0, mat_val / totale_pop_waarde, mat_val)

    strata_init <- plan_stratified_basis(final_df, model = input$plan_model)

    init_fout <- eval_stratified(
      strata_init, model = input$plan_model, zekerheid = conf_val,
      methode = "FFT samen", granulariteit = klim_gran_val, vergelijk = FALSE
    )$max_fout_convolutie

    for (i in 1:nrow(strata_init)) {
      t_idx <- which(raw_df$naam == strata_init$naam[i])
      if (length(t_idx) > 0) {
        raw_df$n_laag[t_idx] <- format(strata_init$n_basis[i], big.mark = ".", decimal.mark = ",")
        raw_df$n_laag_extra[t_idx] <- "0"
        raw_df$n_laag_tot[t_idx] <- format(strata_init$n_basis[i], big.mark = ".", decimal.mark = ",")
        raw_df$n_totaal[t_idx] <- format(strata_init$n_basis[i] + strata_init$n_hoog[i], big.mark = ".", decimal.mark = ",")
      }
    }

    plan_table_data(raw_df)
    live_status_tekst(sprintf("Basisplanning berekend. Initiële algehele fout: %.4f. Live optimalisatie start...", init_fout))

    shinyjs::runjs("$('#hot_plan_input').data('handsontable').render();")

    live_strata_data(strata_init)
    live_ruwe_tabel(raw_df)
    live_reken_mat(reken_mat)
    live_huidige_fout(init_fout)
    live_iteratie(0)
    live_berekening_actief(TRUE)
  })

  observe({
    req(live_berekening_actief())

    vertraging_ms <- if (!is.null(input$plan_vertraging)) input$plan_vertraging * 1000 else 300
    invalidateLater(max(10, vertraging_ms), session)

    isolate({
      strata <- live_strata_data()
      raw_df <- live_ruwe_tabel()
      reken_mat <- live_reken_mat()
      huidige_fout <- live_huidige_fout()
      iteratie <- live_iteratie()
      conf_val <- parse_dutch_num(input$plan_conf)
      klim_gran_val <- parse_dutch_num(input$plan_klim_granulariteit)

      if (huidige_fout > reken_mat && iteratie < 1000) {
        iteratie <- iteratie + 1

        beste_strata_indices <- vind_beste_strata_groep(
          strata, model = input$plan_model, klim_granulariteit = klim_gran_val, totale_zekerheid = conf_val
        )
        aantal_parallel <- length(beste_strata_indices)

        info_strata <- character(aantal_parallel)
        for (idx in seq_along(beste_strata_indices)) {
          s_idx <- beste_strata_indices[idx]
          info_strata[idx] = sprintf("%s; n = %d", strata$naam[s_idx], strata$n_laag[s_idx] + 1)
        }
        strata_info_tekst <- paste(info_strata, collapse = ", ")

        for (beste_stratum in beste_strata_indices) {
          strata$n_laag[beste_stratum] <- strata$n_laag[beste_stratum] + 1
          strata$k_laag[beste_stratum] <- strata$n_laag[beste_stratum] * strata$verwachte_foutfractie[beste_stratum]
          strata$n_totaal[beste_stratum] <- strata$n_laag[beste_stratum] + strata$n_hoog[beste_stratum]
        }

        huidige_fout <- eval_stratified(
          strata, model = input$plan_model, zekerheid = conf_val,
          methode = "FFT samen", granulariteit = klim_gran_val, vergelijk = FALSE
        )$max_fout_convolutie

        for (i in 1:nrow(strata)) {
          t_idx <- which(raw_df$naam == strata$naam[i])
          if (length(t_idx) > 0) {
            n_b <- strata$n_basis[i]
            n_l_t <- strata$n_laag[i]
            n_h <- strata$n_hoog[i]

            raw_df$n_laag[t_idx] <- format(n_b, big.mark = ".", decimal.mark = ",")
            raw_df$n_laag_extra[t_idx] <- format(n_l_t - n_b, big.mark = ".", decimal.mark = ",")
            raw_df$n_laag_tot[t_idx] <- format(n_l_t, big.mark = ".", decimal.mark = ",")
            raw_df$n_totaal[t_idx] <- format(n_l_t + n_h, big.mark = ".", decimal.mark = ",")
          }
        }

        fmt_huidige_fout <- format(round(huidige_fout, 4), decimal.mark = ",")
        fmt_reken_mat <- format(round(reken_mat, 4), decimal.mark = ",")

        live_status_tekst(sprintf("Stap %d | Opgehoogd naar: %s | Resterende fout: %s (doel: %s)", iteratie, strata_info_tekst, fmt_huidige_fout, fmt_reken_mat))

        plan_table_data(raw_df)
        live_ruwe_tabel(raw_df)
        live_strata_data(strata)
        live_huidige_fout(huidige_fout)
        live_iteratie(iteratie)

        shinyjs::runjs("setTimeout(function() { $('#hot_plan_input').data('handsontable').render(); }, 10);")

      } else {
        live_berekening_actief(FALSE)
        fmt_eind_fout <- format(round(huidige_fout, 4), decimal.mark = ",")
        live_status_tekst(sprintf("Optimalisatie voltooid! Eindfout: %s is kleiner dan de materialiteit.", fmt_eind_fout))
        shinyjs::runjs("$('#hot_plan_input').data('handsontable').render();")
      }
    })
  })
}

# Construeer en retourneer het app-object expliciet voor de pakket-wrapper.
shinyApp(ui = ui, server = server)
