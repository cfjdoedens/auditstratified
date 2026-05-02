library(shiny)
library(tibble)
library(dplyr)
library(readr)
library(rhandsontable)
library(htmlwidgets)
library(ggplot2)
library(bslib)
library(bsicons)
library(auditstratified)

# risico opties voor de dropdowns.
{
  risk_choices <- c("hoog (H)" = "H",
                    "midden (M)" = "M",
                    "laag (L)" = "L")

  risk_vec_ui <- c("H", "M", "L")
}

# Verbeterde helper functie die veilig is voor vectoren.
{
  parse_dutch_num <- function(x) {
    if (is.null(x))
      return(NA)

    res <- as.character(x)
    res[res == ""] <- NA

    parse_number(res, locale = locale(decimal_mark = ",", grouping_mark = "."))
  }
}

# Helper functie voor labels met tooltips.
{
  info_label <- function(tekst, tooltip_tekst) {
    tags$span(tekst, tooltip(
      bs_icon("info-circle", class = "ms-1", style = "font-size: 0.9em; color: #007bff;"),
      tooltip_tekst
    ))
  }
}

ui <- navbarPage(
  "auditstratified",
  theme = bs_theme(version = 5),

  # css en scripts voor de tabel-opmaak.
  header = tags$head(tags$style(
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
      .handsontable .readonly-cell {
        background-color: #eeeeee !important;
        color: #555555 !important;
        font-weight: bold;
      }
      .handsontable td.current { background-color: #e6f2ff !important; }
      .handsontable .htRight { text-align: right; }
    "
    )
  ), tags$script(
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
      $(document).on('focusout', '#plan_totale_mat, #plan_conf, #plan_gran', function() {
        Shiny.setInputValue('format_plan_sidebar', Math.random(), {priority: 'event'});
      });

      /* Slimme zandloper-logica om geflikker bij kleine updates te voorkomen. */
      var busyTimer;
      $(document).on('shiny:busy', function() {
        busyTimer = setTimeout(function() {
          $('html').addClass('rekenen-bezig');
        }, 200);
      });
      $(document).on('shiny:idle', function() {
        clearTimeout(busyTimer);
        $('html').removeClass('rekenen-bezig');
      });
    "
    )
  )),

  # Tab 1 voor de berekening van de benodigde zekerheid.
  tabPanel(
    "nog nodige zekerheid",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h4("risico-inschatting"),
        helpText(
          "Bereken de nog benodigde zekerheid volgens HARo paragraaf B7.3.4."
        ),
        selectInput(
          "haro_ihr",
          label = info_label("ihr:", "Inherent risico."),
          choices = risk_choices
        ),
        selectInput(
          "haro_ibr",
          label = info_label("ibr:", "Interne beheersingsrisico."),
          choices = risk_choices
        ),
        selectInput(
          "haro_car",
          label = info_label("car:", "Cijferanalyserisico."),
          choices = risk_choices
        )
      ),
      h3("resultaat"),
      verbatimTextOutput("res_haro"),
      p(
        "Dit is de zekerheid (fractie 0-1) die u nog uit detailcontroles moet halen."
      )
    )
  ),

  # Tab 2 voor de berekening van het postenequivalent.
  tabPanel(
    "foutlozepostenequivalent",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h4("risico & materialiteit"),
        helpText("Bereken hoeveel foutloze posten uw risico-inschatting waard is."),
        selectInput(
          "fpe_ihr",
          label = info_label("ihr:", "Inherent risico."),
          choices = risk_choices
        ),
        selectInput(
          "fpe_ibr",
          label = info_label("ibr:", "Interne beheersingsrisico."),
          choices = risk_choices
        ),
        selectInput(
          "fpe_car",
          label = info_label("car:", "Cijferanalyse."),
          choices = risk_choices
        ),
        textInput(
          "fpe_mat",
          label = info_label("materialiteit:", "Grens (bijv. 0,01)."),
          value = "0,01"
        )
      ),
      h3("resultaat"),
      verbatimTextOutput("res_fpe"),
      p("Aantal posten dat overeenkomt met de verlaagde risico's.")
    )
  ),

  # Tab 3 voor de evaluatie van de resultaten.
  tabPanel(
    "evaluatie gestratificeerd",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h4("instellingen"),
        textInput(
          "strat_conf",
          label = info_label(
            "zekerheid:",
            "De gewenste betrouwbaarheid (bijv. 0,95 voor 95%)."
          ),
          value = "0,95"
        ),
        radioButtons(
          "strat_model",
          label = info_label("model:", "De statistische verdeling gebruikt als model."),
          choices = c("binomiaal", "poisson"),
          inline = TRUE
        ),
        radioButtons(
          "strat_methode",
          label = info_label(
            "methode:",
            paste0(
              "De rekenmethode voor de convolutie (vermenigvuldiging van kanskrommen). ",
              "Er worden vier verschillende methoden aangeboden. ",
              "De methoden verschillen, het zijn verschillende algoritmes. ",
              "Maar ze leveren ongeveer hetzelfde resultaat. ",
              "1. direct: basismethode om convolutie te plegen. ",
              "Kanskrommen worden paarsgewijs met elkaar vermenigvuldigd. ",
              "2. FFT: wiskundige verbetering op direct, altijd sneller dan direct. ",
              "Ook paarsgewijze vermenigvuldiging van de kanskrommen. ",
              "3. FFT gelijktijdig: als FFT maar nu wordt de vermenigvuldiging in 1 keer uitgevoerd. ",
              "Daarom weer sneller dan FFT. ",
              "4. Monte Carlo: vermenigvuldiging wordt in 1 keer uitgevoerd. ",
              "Gebaseerd op toevalstrekking van de krommen. ",
              "Daardoor meestal snel, maar geeft ook een toevalsruis. ",
              "Overigens is er voor alle 4 de methoden ruis door de ",
              "grofheid van de gramulariteit."
            )
          ),
          choices = c(
            "direct" = "direct",
            "FFT" = "FFT",
            "FFT gelijktijdig" = "FFT gelijktijdig",
            "Monte Carlo" = "Monte Carlo"
          ),
          selected = "FFT gelijktijdig",
          inline = TRUE
        ),
        textInput(
          "strat_gran",
          label = info_label(
            "granulariteit:",
            "Nauwkeurigheid van de berekening (hoger = preciezer maar trager)."
          ),
          value = "10.000"
        ),
        numericInput(
          "eval_start",
          label = info_label(
            "startwaarde:",
            "Voor toevalsgenerator (Monte Carlo). Waarde van 0 betekent: baseer op systeemklok, dus min of meer 'echt' bij toeval."
          ),
          value = 1,
          min = 0,
          step = 1
        ),
        hr(),
        hr(),
        actionButton(
          "run_strat",
          "bereken evaluatie",
          class = "btn-success",
          width = "100%"
        )
      ),
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
  ),

  # Tab 4: Planning gestratificeerd met granulariteit onderaan de zijbalk.
  tabPanel(
    "planning gestratificeerd",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h4("instellingen"),
        textInput(
          "plan_totale_mat",
          label = info_label(
            "materialiteit:",
            "Voer een fractie (0,01) of absoluut eurobedrag in."
          ),
          value = "0,01"
        ),
        textInput(
          "plan_conf",
          label = info_label(
            "zekerheid:",
            "De gewenste betrouwbaarheid (bijv. 0,95 voor 95%)."
          ),
          value = "0,95"
        ),
        radioButtons(
          "plan_model",
          label = info_label("model:", "De statistische verdeling gebruikt als model."),
          choices = c("binomiaal" = "binomiaal", "poisson" = "poisson"),
          inline = TRUE
        ),
        radioButtons(
          "plan_klim_methode",
          label = info_label(
            "klimmethode:",
            paste0(
              "De rekenmethode voor de convolutie bij het klimmen. ",
              "Er worden vier verschillende methoden aangeboden. ",
              "De methoden verschillen, het zijn verschillende algoritmes. ",
              "Maar ze leveren ongeveer hetzelfde resultaat. ",
              "1. direct: basismethode om convolutie te plegen. ",
              "Kanskrommen worden paarsgewijs met elkaar vermenigvuldigd. ",
              "2. FFT: wiskundige verbetering op direct, altijd sneller dan direct. ",
              "Ook paarsgewijze vermenigvuldiging van de kanskrommen. ",
              "3. FFT gelijktijdig: als FFT maar nu wordt de vermenigvuldiging in 1 keer uitgevoerd. ",
              "Daarom weer sneller dan FFT. ",
              "4. Monte Carlo: vermenigvuldiging wordt in 1 keer uitgevoerd. ",
              "Gebaseerd op toevalstrekking van de krommen. ",
              "Daardoor meestal snel, maar geeft ook een toevalsruis. ",
              "Overigens is er voor alle 4 de methoden ruis door de ",
              "grofheid van de gramulariteit."
            )
          ),
          choices = c(
            "direct" = "direct",
            "FFT" = "FFT",
            "FFT gelijktijdig" = "FFT gelijktijdig",
            "Monte Carlo" = "Monte Carlo"
          ),
          selected = "FFT gelijktijdig",
          inline = TRUE
        ),
        textInput(
          "plan_klim_granulariteit",
          label = info_label(
            "granulariteit bij klimmen:",
            "Nauwkeurigheid van het klimmen (hoger = preciezer maar trager)."
          ),
          value = "10.000"
        ),
        radioButtons(
          "plan_validatie_methode",
          label = info_label(
            "validatiemethode:",
            paste0(
              "De rekenmethode voor de convolutie bij het valideren. ",
              "Er worden vier verschillende methoden aangeboden. ",
              "De methoden verschillen, het zijn verschillende algoritmes. ",
              "Maar ze leveren ongeveer hetzelfde resultaat. ",
              "1. direct: basismethode om convolutie te plegen. ",
              "Kanskrommen worden paarsgewijs met elkaar vermenigvuldigd. ",
              "2. FFT: wiskundige verbetering op direct, altijd sneller dan direct. ",
              "Ook paarsgewijze vermenigvuldiging van de kanskrommen. ",
              "3. FFT gelijktijdig: als FFT maar nu wordt de vermenigvuldiging in 1 keer uitgevoerd. ",
              "Daarom weer sneller dan FFT. ",
              "4. Monte Carlo: vermenigvuldiging wordt in 1 keer uitgevoerd. ",
              "Gebaseerd op toevalstrekking van de krommen. ",
              "Daardoor meestal snel, maar geeft ook een toevalsruis. ",
              "Overigens is er voor alle 4 de methoden ruis door de ",
              "grofheid van de gramulariteit."
            )
          ),
          choices = c(
            "direct" = "direct",
            "FFT" = "FFT",
            "FFT gelijktijdig" = "FFT gelijktijdig",
            "Monte Carlo" = "Monte Carlo"
          ),
          selected = "Monte Carlo",
          inline = TRUE
        ),
        textInput(
          "plan_validatie_granulariteit",
          label = info_label(
            "granulariteit bij valideren:",
            "Nauwkeurigheid van het valideren (hoger = preciezer maar trager)."
          ),
          value = "10.000"
        ),
        numericInput(
          "plan_start",
          label = info_label(
            "startwaarde:",
            "Voor toevalsgenerator (Monte Carlo). Waarde van 0 betekent: baseer op systeemklok, dus min of meer 'echt' bij toeval."
          ),
          value = 1,
          min = 0,
          step = 1
        ),
        hr(),
        actionButton(
          "run_plan",
          "bereken planning",
          class = "btn-success",
          width = "100%"
        )
      ),
      h3("planning"),
      helpText(
        "Vul de witte velden (links) in en klik op 'bereken planning'. De grijze velden (rechts) worden dan berekend."
      ),
      rHandsontableOutput("hot_plan_input")
    )
  )
)

server <- function(input, output, session) {
  # Renderers voor correcte weergave van valuta met euroteken, percentages en getallen.
  renderer_nl_money <- JS(
    "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL', { style: 'currency', currency: 'EUR' }); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
  )
  renderer_nl_percent <- JS(
    "function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        var numVal = NaN;
        if (value !== null && value !== void 0 && value !== '') {
          var str = value.toString().replace(/\\./g, '').replace(',', '.');
          numVal = parseFloat(str);
        }
        if (!isNaN(numVal)) {
          if (numVal > 1) {
            td.innerHTML = numVal.toLocaleString('nl-NL', { style: 'currency', currency: 'EUR' });
          } else {
            td.innerHTML = numVal.toLocaleString('nl-NL', { style: 'percent', minimumFractionDigits: 2 });
          }
        }
        td.style.background = 'white';
        td.style.color = 'black';
        td.style.textAlign = 'right';
      }"
  )
  renderer_nl_general <- JS(
    "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL'); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
  )

  # berekening voor de eerste twee tabbladen.
  {
    output$res_haro <- renderText({
      val <- haro_nog_nodige_zekerheid(input$haro_ihr, input$haro_ibr, input$haro_car)

      paste("nog nodige zekerheid:",
            format(round(val, 4), decimal.mark = ","))
    })

    output$res_fpe <- renderText({
      mat_val <- parse_dutch_num(input$fpe_mat)
      if (is.na(mat_val))
        return("Ongeldige materialiteit")

      val <- foutloze_posten_equivalent(input$fpe_ihr, input$fpe_ibr, input$fpe_car, mat_val)
      paste("foutlozepostenequivalent:", round(val, 0))
    })
  }

  # Evaluatie logica voor tabblad 3.
  {
    # Werk de tekst van de granulariteit bij met
    # duizendtalscheidingstekens en verspring naar de resultaten.
    observeEvent(input$run_strat, {
      gran_val <- parse_dutch_num(input$strat_gran)

      if (!is.na(gran_val)) {
        opgemaakt_getal <- format(
          gran_val,
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
        updateTextInput(session, "strat_gran", value = opgemaakt_getal)
      }

      updateTabsetPanel(session, "hoofd_tabs", selected = "tab_grafiek")
    })

    # Luister naar wijzigingen in de evaluatiemethode
    observeEvent(input$strat_methode, {
      # Bepaal de verstekwaarde op basis van de methode
      nieuw_gran <- if (input$strat_methode == "Monte Carlo") "10.000.000" else "25.000"

      # Werk het invoerveld op het scherm bij
      updateTextInput(session, "strat_gran", value = nieuw_gran)
    })

    # Voeg de onrender functie toe aan de invoertabel van de evaluatie om tooltips bij de kolomkoppen te tonen.
    output$hot_input <- renderRHandsontable({
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

      rhandsontable(df, stretchH = "all") |>
        hot_col("waarde_laag", renderer = renderer_nl_money) |>
        hot_col("n_laag", renderer = renderer_nl_general) |>
        hot_col("k_laag", renderer = renderer_nl_general) |>
        hot_col("fout_hoog", renderer = renderer_nl_money) |>
        hot_col("goed_hoog", renderer = renderer_nl_money) |>
        hot_col("n_hoog", renderer = renderer_nl_general) |>
        hot_col("ihr",
                type = "dropdown",
                source = risk_vec_ui,
                strict = TRUE) |>
        hot_col("ibr",
                type = "dropdown",
                source = risk_vec_ui,
                strict = TRUE) |>
        hot_col("car",
                type = "dropdown",
                source = risk_vec_ui,
                strict = TRUE) |>
        hot_col("materialiteit", renderer = renderer_nl_percent) |>
        onRender(
          "function(el, x) {
        var hot = this.hot;
        hot.updateSettings({
          afterGetColHeader: function(col, TH) {
            var tooltips = [
              'naam van het stratum',
              'boekwaarde in euro\\'s van het totale laagstratum',
              'aantal gecontroleerde posten in het laagstratum',
              'som van de foutfracties in de steekproef van het laagstratum',
              'de totale som van de foute euro\\'s van de 100%-gecontroleerde posten',
              'de totale som van de goede euro\\'s van de 100%-gecontroleerde posten',
              'aantal posten in het hoogstratum',
              'Inherent Risico (H, M, L)',
              'Interne Beheersingsrisico (H, M, L)',
              'Cijferanalyserisico (H, M, L)',
              'de toegestane afwijking als percentage of absoluut bedrag'
            ];
            if (col >= 0 && col < tooltips.length) {
              TH.setAttribute('title', tooltips[col]);
              TH.style.cursor = 'help';
            }
          }
        });
      }"
        )
    })

    strat_results <- eventReactive(input$run_strat, {
      req(input$hot_input)
      raw_df <- hot_to_r(input$hot_input)

      # Parsen en afgeleide kolommen berekenen.
      {
        final_df <- raw_df |>
          as_tibble() |>
          filter(!is.na(naam) & naam != "") |>
          mutate(across(
            c(
              "waarde_laag",
              "n_laag",
              "k_laag",
              "fout_hoog",
              "goed_hoog",
              "n_hoog",
              "materialiteit"
            ),
            parse_dutch_num
          )) |>
          mutate(across(c("ihr", "ibr", "car"), toupper)) |>
          mutate(
            waarde_hoog = .data$fout_hoog + .data$goed_hoog,
            waarde_populatie = .data$waarde_laag + .data$waarde_hoog,
            n_totaal = .data$n_laag + .data$n_hoog
          ) |>
          mutate(
            materialiteit = ifelse(
              .data$materialiteit > 1 &
                .data$waarde_populatie > 0,
              .data$materialiteit / .data$waarde_populatie,
              .data$materialiteit
            )
          )
      }

      validate(need(nrow(final_df) > 0, "Vul data in."))

      # toon een zandlopertje tijdens de evaluatieberekening.
      withProgress(message = 'Berekening wordt uitgevoerd...', value = 0, {
        Sys.sleep(0.1)

        tryCatch(
          withCallingHandlers({
            eval_stratified(
              final_df,
              model = input$strat_model,
              zekerheid = parse_dutch_num(input$strat_conf),
              methode = input$strat_methode,
              granulariteit = parse_dutch_num(input$strat_gran),
              start = input$eval_start
            )
          }, warning = function(w) {
            showNotification(
              paste("Waarschuwing:", conditionMessage(w)),
              type = "warning",
              duration = 10
            )
            invokeRestart("muffleWarning")
          }),
          error = function(e) {
            showNotification(paste("Fout:", conditionMessage(e)),
                             type = "error",
                             duration = 10)
            NULL
          }
        )
      })
    })

    output$plot_strat_main <- renderPlot({
      req(strat_results())
      plot_kanskromme(strat_results())
    })

    # Maak de waarden in de resultatentabel op als eurobedragen in
    # de Nederlandse stijl.
    {
      output$table_strat_main <- renderTable({
        res <- strat_results()
        req(res)

        tibble(
          metriek = c("mw fout", "max fout"),
          waarde = c(paste0(
            "€ ",
            format(
              round(res$mw_fout_convolutie_geld, 2),
              big.mark = ".",
              decimal.mark = ",",
              nsmall = 2,
              scientific = FALSE
            )
          ), paste0(
            "€ ",
            format(
              round(res$max_fout_convolutie_geld, 2),
              big.mark = ".",
              decimal.mark = ",",
              nsmall = 2,
              scientific = FALSE
            )
          ))
        )

      }, align = "lr")
    }
  }

  # Planning logica voor tabblad 4.
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

    output$hot_plan_input <- renderRHandsontable({
      df <- plan_table_data()
      koppen <- c(
        "naam <span>&#9432;</span>",
        "waarde_laag <span>&#9432;</span>",
        "verwacht_fout% <span>&#9432;</span>",
        "fout_hoog <span>&#9432;</span>",
        "goed_hoog <span>&#9432;</span>",
        "n_hoog <span>&#9432;</span>",
        "ihr <span>&#9432;</span>",
        "ibr <span>&#9432;</span>",
        "car <span>&#9432;</span>",
        "materialiteit <span>&#9432;</span>",
        "n_laag <span>&#9432;</span>",
        "n_laag_extra <span>&#9432;</span>",
        "n_laag_tot <span>&#9432;</span>",
        "n_totaal <span>&#9432;</span>"
      )

      renderer_readonly <- JS(
        "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); td.className = 'readonly-cell htRight'; }"
      )

      rhandsontable(df, stretchH = "all") |>
        hot_col("waarde_laag", renderer = renderer_nl_money) |>
        hot_col("verwachte_foutfractie", renderer = renderer_nl_percent) |>
        hot_col("fout_hoog", renderer = renderer_nl_money) |>
        hot_col("goed_hoog", renderer = renderer_nl_money) |>
        hot_col("n_hoog", renderer = renderer_nl_general) |>
        hot_col("ihr",
                type = "dropdown",
                source = risk_vec_ui,
                strict = TRUE) |>
        hot_col("ibr",
                type = "dropdown",
                source = risk_vec_ui,
                strict = TRUE) |>
        hot_col("car",
                type = "dropdown",
                source = risk_vec_ui,
                strict = TRUE) |>
        hot_col("materialiteit", renderer = renderer_nl_percent) |>
        hot_col("n_laag", readOnly = TRUE, renderer = renderer_readonly) |>
        hot_col("n_laag_extra",
                readOnly = TRUE,
                renderer = renderer_readonly) |>
        hot_col("n_laag_tot", readOnly = TRUE, renderer = renderer_readonly) |>
        hot_col("n_totaal", readOnly = TRUE, renderer = renderer_readonly) |>
        hot_cols(colHeaders = koppen) |>
        onRender(
          "function(el, x) { var hot = this.hot; hot.updateSettings({ afterGetColHeader: function(col, TH) {
          var tooltips = [
            'naam van het stratum',
            'boekwaarde in euro\\'s van het totale laagstratum',
            'de foutfractie die u verwacht aan te treffen in het laagstratum',
            'de totale (verwachte en/of al gevonden) som van de foute euro\\'s van het hoogstratum',
            'de totale (verwachte en/of al gevonden) som van de goede euro\\'s van het hoogstratum',
            'aantal posten in het hoogstratum',
            'Inherent Risico (H, M, L)',
            'Interne Beheersingsrisico (H, M, L)',
            'Cijferanalyserisico (H, M, L)',
            'de toegestane afwijking als percentage of als bedrag van hoogstratum+laagstratum',
            'het aantal posten dat getrokken moet worden uit het laagstratum om de maximale fout voor de afzonderlijke steekproef onder de materialiteit te houden',
            'het aantal extra posten dat getrokken moet worden uit het laagstratum om de maximale fout voor de steekproeven samen onder de gezamenlijke materialiteit te houden',
            'n_laag + n_laag_extra',
            'n_laag_tot + n_hoog'
          ];
          if (col >= 0 && col < tooltips.length) { TH.setAttribute('title', tooltips[col]); TH.style.cursor = 'help'; }
        } }); }"
        )
    })

    # Formatteer de zijbalkvelden alleen als de nieuwe waarde echt afwijkt van de huidige invoer.
    observeEvent(input$format_plan_sidebar, {
      # Verwerk materialiteit.
      mat_val <- parse_dutch_num(input$plan_totale_mat)
      if (!is.na(mat_val)) {
        fmt_mat <- if (mat_val > 1) {
          paste0("€ ",
                 format(
                   mat_val,
                   big.mark = ".",
                   decimal.mark = ",",
                   scientific = FALSE
                 ))
        } else {
          format(mat_val,
                 decimal.mark = ",",
                 scientific = FALSE)
        }
        if (input$plan_totale_mat != fmt_mat) {
          updateTextInput(session, "plan_totale_mat", value = fmt_mat)
        }
      }

      # Verwerk zekerheid.
      conf_val <- parse_dutch_num(input$plan_conf)
      if (!is.na(conf_val)) {
        fmt_conf <- format(conf_val,
                           decimal.mark = ",",
                           scientific = FALSE)
        if (input$plan_conf != fmt_conf) {
          updateTextInput(session, "plan_conf", value = fmt_conf)
        }
      }

      # Verwerk granulariteit klimmen.
      klim_gran_val <- parse_dutch_num(input$plan_klim_granulariteit)
      if (!is.na(klim_gran_val)) {
        fmt_klim_gran <- format(
          klim_gran_val,
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
        if (input$plan_klim_granulariteit != fmt_klim_gran) {
          updateTextInput(session, "plan_klim_granulariteit", value = fmt_klim_gran)
        }
      }

      # Verwerk granulariteit valideren.
      val_gran_val <- parse_dutch_num(input$plan_validatie_granulariteit)
      if (!is.na(val_gran_val)) {
        fmt_val_gran <- format(
          val_gran_val,
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
        if (input$plan_validatie_granulariteit != fmt_val_gran) {
          updateTextInput(session, "plan_validatie_granulariteit", value = fmt_val_gran)
        }
      }
    })
  }

  # Luister naar wijzigingen in de klimmethode
  observeEvent(input$plan_klim_methode, {
    nieuw_klim_gran <- if (input$plan_klim_methode == "Monte Carlo") "1.000.000" else "10.000"
    updateTextInput(session, "plan_klim_granulariteit", value = nieuw_klim_gran)
  })

  # Luister naar wijzigingen in de validatiemethode
  observeEvent(input$plan_validatie_methode, {
    nieuw_val_gran <- if (input$plan_validatie_methode == "Monte Carlo") "10.000.000" else "25.000"
    updateTextInput(session, "plan_validatie_granulariteit", value = nieuw_val_gran)
  })

  observeEvent(input$run_plan, {
    req(input$hot_plan_input)

    mat_val <- parse_dutch_num(input$plan_totale_mat)
    conf_val <- parse_dutch_num(input$plan_conf)
    klim_gran_val <- parse_dutch_num(input$plan_klim_granulariteit)
    val_gran_val <- parse_dutch_num(input$plan_validatie_granulariteit)

    # Formatteer materialiteit
    if (!is.na(mat_val)) {
      fmt_mat <- if (mat_val > 1) {
        paste0("€ ", format(mat_val, big.mark = ".", decimal.mark = ",", scientific = FALSE))
      } else {
        format(mat_val, decimal.mark = ",", scientific = FALSE)
      }
      updateTextInput(session, "plan_totale_mat", value = fmt_mat)
    }

    # Formatteer granulariteiten
    if (!is.na(klim_gran_val)) {
      updateTextInput(session, "plan_klim_granulariteit",
                      value = format(klim_gran_val, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    }

    if (!is.na(val_gran_val)) {
      updateTextInput(session, "plan_validatie_granulariteit",
                      value = format(val_gran_val, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    }

    # Stop-check als 1 van de 4 velden ongeldig/leeg is
    if (is.na(mat_val) || is.na(conf_val) || is.na(klim_gran_val) || is.na(val_gran_val)) {
      showNotification("Ongeldige instellingen.", type = "error")
      return()
    }

    raw_df <- hot_to_r(input$hot_plan_input)

    # Leegmaken van de resultaatkolommen voordat de berekening start.
    {
      raw_df$n_laag <- ""
      raw_df$n_laag_extra <- ""

      raw_df$n_laag_tot <- ""
      raw_df$n_totaal <- ""
    }

    final_df <- raw_df |>
      as_tibble() |>
      mutate(across(
        c("waarde_laag",
          "verwachte_foutfractie",
          "fout_hoog",
          "goed_hoog",
          "n_hoog",
          "materialiteit"
        ),
        parse_dutch_num
      )) |>
      mutate(across(c("ihr", "ibr", "car"), toupper)) |>
      filter(!is.na(naam) & naam != "" & !is.na(waarde_laag)) |>
      mutate(waarde_hoog = fout_hoog + goed_hoog,
             waarde_populatie = waarde_laag + waarde_hoog) |>
      mutate(
        materialiteit = ifelse(
          .data$materialiteit > 1 &
            .data$waarde_populatie > 0,
          .data$materialiteit / .data$waarde_populatie,
          .data$materialiteit
        )
      )

    if (nrow(final_df) == 0) {
      plan_table_data(raw_df)
      showNotification("Vul minimaal een naam en waarde in.", type = "warning")
      return()
    }

    # Berekening uitvoeren en zandlopertje tonen.
    {
      totale_pop_waarde <- sum(final_df$waarde_populatie, na.rm = TRUE)
      reken_mat <- ifelse(mat_val > 1 &&
                            totale_pop_waarde > 0,
                          mat_val / totale_pop_waarde,
                          mat_val)

      withProgress(message = 'Optimalisatie wordt uitgevoerd...', value = 0, {
        Sys.sleep(0.1)

        tryCatch({
          res <- plan_stratified(
            final_df,
            reken_mat,
            conf_val,
            model = input$plan_model,
            klim_methode = input$plan_klim_methode,
            klim_granulariteit = klim_gran_val,
            validatie_methode = input$plan_validatie_methode,
            validatie_granulariteit = val_gran_val,
            max_iteraties = 1000,
            start = input$plan_start
          )
          res_fmt <- res |> mutate(
            n_l = n_basis,
            # Basis.
            n_l_e = n_definitief - n_basis,
            # Extra.
            n_l_t = n_definitief,
            # Definitief = basis + extra.
            n_t = n_definitief + n_hoog     # Totaal = basis + extra + hoog.
          ) |>
            mutate(across(
              c(n_l, n_l_e, n_l_t, n_t),
              ~ format(
                round(., 0),
                big.mark = ".",
                decimal.mark = ","
              )
            ))

          for (i in 1:nrow(res_fmt)) {
            idx <- which(raw_df$naam == res_fmt$naam[i])
            if (length(idx) > 0) {
              raw_df$n_laag[idx] <- res_fmt$n_l[i]
              raw_df$n_laag_extra[idx] <- res_fmt$n_l_e[i]
              raw_df$n_laag_tot[idx] <- res_fmt$n_l_t[i]
              raw_df$n_totaal[idx] <- res_fmt$n_t[i]
            }
          }

          plan_table_data(raw_df)
        }, warning = function(w) {
          showNotification(
            paste("Let op:", conditionMessage(w)),
            type = "warning",
            duration = 15
          )
          plan_table_data(raw_df)
        }, error = function(e) {
          showNotification(paste("Fout:", conditionMessage(e)), type = "error")
          plan_table_data(raw_df)
        })
      })
    }
  })
}

shinyApp(ui, server)
