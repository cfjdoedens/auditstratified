utils::globalVariables(c("x_geld", "y_dichtheid"))

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

  df_plot <- data.frame(x_geld = d$x * totaal_geld, y_dichtheid = d$y) %>%
    filter(x_geld >= min_geld & x_geld <= totaal_geld)

  if(nrow(df_plot) > 0) {
    df_plot <- bind_rows(
      data.frame(x_geld = min_geld, y_dichtheid = 0),
      df_plot,
      data.frame(x_geld = totaal_geld, y_dichtheid = 0)
    ) %>% arrange(x_geld, y_dichtheid)
  }

  mode_val <- max(res$mw_fout_convolutie_geld, min_geld)
  max_val <- max(res$max_fout_convolutie_geld, min_geld)
  zekerheid_pct <- res$invoer$zekerheid * 100

  totaal_laag_geld <- sum(res$steekproeven$waarde_laag)

  # Geval 2: 100% Integraal gecontroleerd
  if (totaal_laag_geld < 0.01) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = paste0("100% Integraal Gecontroleerd\n\nEr is geen statistische onzekerheid.\nDe fout is exact vastgesteld op \u20ac ",
                              format(min_geld, big.mark = ".", decimal.mark = ",")),
               size = 5, color = "#2c3e50", fontface = "bold", hjust = 0.5) +
      theme_void()
    return(p)
  }

  # Reguliere kanskromme
  p <- ggplot(df_plot, aes(x = x_geld, y = y_dichtheid)) +
    geom_area(data = subset(df_plot, x_geld <= max_val), fill = "#e0e0e0", alpha = 0.7) +
    geom_line(color = "#2c3e50", linewidth = 1) +
    geom_vline(xintercept = mode_val, color = "blue", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = max_val, color = "red", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, prefix = "\u20ac ")) +
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
      panel.grid.minor = element_blank()
    )

  return(p)
}
