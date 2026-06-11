library(auditstratified)
library(bsicons)
library(bslib)
library(dplyr)
library(ggplot2)
library(htmlwidgets)
library(readr)
library(rhandsontable)
library(shiny)
library(shinyjs)
library(tibble)

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

