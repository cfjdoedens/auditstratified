utils::globalVariables(c("x_geld", "y_dichtheid", "gebied"))

#' @title Teken de kanskromme van de evaluatie
#' @description Genereert een ggplot2 object van de resulterende kanskromme.
#' @param res Het resultaat object uit de functie eval_stratified.
#' @returns Een ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_vline annotate
#' @importFrom ggplot2 scale_x_continuous scale_fill_manual scale_color_manual
#' @importFrom ggplot2 labs theme_minimal theme_void theme element_text
#' @importFrom ggplot2 element_blank guide_legend
#' @importFrom dplyr mutate case_when
#' @importFrom stats setNames
plot_kanskromme <- function(res) {
  d <- res$kanskromme
  totaal_geld <- res$populatie_totaal
  min_geld <- sum(res$steekproeven$fout_hoog)
  totaal_laag_geld <- sum(res$steekproeven$waarde_laag)

  mode_val <- max(res$mw_fout_convolutie_geld, min_geld)
  max_val <- max(res$max_fout_convolutie_geld, min_geld)

  # Bereken de percentages voor de legenda
  zekerheid_pct <- res$invoer$zekerheid * 100
  rest_pct <- 100 - zekerheid_pct

  # 1. Dynamische labels instellen voor de legenda
  lbl_binnen <- paste0("linker ", zekerheid_pct, "%")
  lbl_buiten <- paste0("rechter ", rest_pct, "%")
  lbl_onm_min <- "Onmogelijk (< minimum fout)"
  lbl_onm_max <- "Onmogelijk (> populatiewaarde)"

  # Als alle posten zijn gecontroleerd, is er geen statistische onzekerheid.
  if (totaal_laag_geld < 0.01) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = paste0("100% Integraal Gecontroleerd\n\nEr is geen statistische onzekerheid.\nDe fout is exact vastgesteld op \u20ac ",
                              format(min_geld, big.mark = ".", decimal.mark = ",")),
               size = 5, color = "#2c3e50", fontface = "bold", hjust = 0.5) +
      theme_void()
    return(p)
  }

  # Data behandelen met de nieuwe dynamische labels
  df_plot <- data.frame(x_geld = d$x * totaal_geld, y_dichtheid = d$y) |>
    mutate(
      gebied = case_when(
        x_geld < min_geld ~ lbl_onm_min,
        x_geld > totaal_geld ~ lbl_onm_max,
        x_geld <= max_val ~ lbl_binnen,
        TRUE ~ lbl_buiten
      ),
      # Maak van gebied een factor om de volgorde in de legenda vast te zetten
      gebied = factor(gebied, levels = c(lbl_binnen, lbl_buiten, lbl_onm_min, lbl_onm_max))
    )

  # 2. Kleuren dynamisch koppelen aan de variabelen labels (met setNames)
  kleuren_vlak <- setNames(
    # Ik gebruik hier twee mooie, rustige tinten blauw (donker en licht).
    # Rood en oranje blijven voor de onmogelijke waarschuwingen.
    c("#2980b9", "#aed6f1", "#e74c3c", "#e67e22"),
    c(lbl_binnen, lbl_buiten, lbl_onm_min, lbl_onm_max)
  )

  kleuren_lijn <- setNames(
    # Voor de buitenste lijn houden we het hele geldige vlak netjes donkerblauw (#2c3e50)
    # zodat de berg een strak, ononderbroken dak heeft.
    c("#2c3e50", "#2c3e50", "#e74c3c", "#e67e22"),
    c(lbl_binnen, lbl_buiten, lbl_onm_min, lbl_onm_max)
  )

  # Reguliere kanskromme tekenen
  p <- ggplot(df_plot, aes(x = x_geld, y = y_dichtheid)) +
    geom_area(aes(fill = gebied), alpha = 0.8) +
    geom_line(aes(color = gebied, group = 1), linewidth = 1, show.legend = FALSE) +

    geom_vline(xintercept = mode_val, color = "blue", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = max_val, color = "red", linetype = "dashed", linewidth = 1) +

    scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, prefix = "\u20ac ")) +

    # Kleuren voor de ingekleurde vlakken + nieuwe legenda naam
    scale_fill_manual(
      values = kleuren_vlak,
      name = "kansmassa",  # <--- HIER IS DE NAAM AANGEPAST
      guide = guide_legend(nrow = 2,
                           byrow = TRUE,
                           override.aes = list(color = "black", linewidth = 0.5))
    ) +

    # Kleuren voor de buitenste lijn
    scale_color_manual(
      values = kleuren_lijn
    ) +

    labs(
      title = "kanskromme van de geprojecteerde fout",
      subtitle = paste0("blauwe lijn = meest waarschijnlijke fout | rode lijn = maximale fout (", zekerheid_pct, "% zekerheid)"),
      x = "fout in euro's", y = "relatieve kansdichtheid"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      plot.title = element_text(face = "bold"),
      axis.text.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title = element_text(face = "bold")
    )

  return(p)
}
