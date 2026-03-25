utils::globalVariables(c("x_geld", "y_dichtheid", "gebied"))

#' @title Teken de kanskromme van de evaluatie
#' @description Genereert een ggplot2 object van de resulterende kanskromme.
#' @param res Het resultaat object uit de functie eval_stratified.
#' @returns Een ggplot2 object.
#' @export
#' @import ggplot2 dplyr
plot_kanskromme <- function(res) {
  d <- res$kanskromme
  totaal_geld <- res$populatie_totaal
  min_geld <- sum(res$steekproeven$fout_hoog)
  totaal_laag_geld <- sum(res$steekproeven$waarde_laag)

  mode_val <- max(res$mw_fout_convolutie_geld, min_geld)
  max_val <- max(res$max_fout_convolutie_geld, min_geld)
  zekerheid_pct <- res$invoer$zekerheid * 100

  # Als alle posten zijn gecontroleerd, is er geen statistische onzekerheid.
  # De grafiek zou dan alleen maar bestaan uit 1 verticale lijn.
  # In plaats daarvan drukken we een verklarend tekstje af.
  if (totaal_laag_geld < 0.01) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = paste0("100% Integraal Gecontroleerd\n\nEr is geen statistische onzekerheid.\nDe fout is exact vastgesteld op \u20ac ",
                              format(min_geld, big.mark = ".", decimal.mark = ",")),
               size = 5, color = "#2c3e50", fontface = "bold", hjust = 0.5) +
      theme_void()
    return(p)
  }

  # Data behandelen: We filteren niets weg, maar geven elk datapunt een label.
  df_plot <- data.frame(x_geld = d$x * totaal_geld, y_dichtheid = d$y) %>%
    mutate(
      gebied = case_when(
        x_geld < min_geld ~ "Onmogelijk (< minimum fout)",
        x_geld > totaal_geld ~ "Onmogelijk (> populatiewaarde)",
        x_geld <= max_val ~ "Geldig (binnen betrouwbaarheid)",
        TRUE ~ "Geldig (buiten betrouwbaarheid)"
      ),

      # Maak van gebied een factor om de volgorde in de legenda logisch vast te zetten.
      gebied = factor(gebied, levels = c(
        "Geldig (binnen betrouwbaarheid)",
        "Geldig (buiten betrouwbaarheid)",
        "Onmogelijk (< minimum fout)",
        "Onmogelijk (> populatiewaarde)"
      ))
    )

  # Reguliere kanskromme met de nieuwe ingekleurde vlakken
  p <- ggplot(df_plot, aes(x = x_geld, y = y_dichtheid)) +
    # Het vlak inkleuren op basis van de kolom 'gebied'
    geom_area(aes(fill = gebied), alpha = 0.6) +

    # NIEUW: De lijn inkleuren op basis van het gebied (in plaats van 1 vaste kleur)
    geom_line(aes(color = gebied, group = 1), linewidth = 1, show.legend = FALSE) +

    geom_vline(xintercept = mode_val, color = "blue", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = max_val, color = "red", linetype = "dashed", linewidth = 1) +

    scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, prefix = "\u20ac ")) +

    # Hier bepalen we exact de kleuren voor elk vlak
    scale_fill_manual(
      values = c(
        "Onmogelijk (< minimum fout)" = "#e74c3c",       # Waarschuwing: Rood
        "Geldig (binnen betrouwbaarheid)" = "#bdc3c7",   # Normaal: Grijs
        "Geldig (buiten betrouwbaarheid)" = "transparent", # Normaal: Wit/Doorzichtig
        "Onmogelijk (> populatiewaarde)" = "#e67e22"     # Waarschuwing: Oranje
      ),
      name = "Verdeling van de geprojecteerde fout:",
      guide = guide_legend(nrow = 2,
                           byrow = TRUE,
                           override.aes = list(color = "black", linewidth = 0.5))
    ) +

    # NIEUW: Hier bepalen we de specifieke kleuren voor de buitenste lijn
    scale_color_manual(
      values = c(
        "Onmogelijk (< minimum fout)" = "#e74c3c",       # Rood
        "Geldig (binnen betrouwbaarheid)" = "#2c3e50",   # Donkerblauw
        "Geldig (buiten betrouwbaarheid)" = "#2c3e50",   # Donkerblauw
        "Onmogelijk (> populatiewaarde)" = "#e67e22"     # Oranje
      )
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
      legend.position = "bottom",          # Zet de legenda mooi onder de grafiek
      legend.direction = "vertical",
      legend.title = element_text(face = "bold")
    )
  return(p)
}
