#' @title
#' Plan de initiële basissteekproefomvang (Stap 1)
#'
#' @description
#' Deze functie berekent de minimale uitgangssituatie per stratum om
#' afzonderlijk onder de eigen stratum-materialiteit te blijven.
#'
#' @param steekproeven Een tibble met de planningsgegevens.
#' @param model Het statistische model ("binomiaal" of "poisson").
#' @returns De tibble verrijkt met de initiële n_basis en n_laag.
#' @export
plan_stratified_basis <- function(steekproeven, model = c("binomiaal", "poisson")) {
  # Valideer het gekozen model en converteer naar de juiste Engelse distributieterm.
  model <- match.arg(model)
  dist_eng <- if (model == "binomiaal") "binomial" else "Poisson"

  # Bereken per stratum de HARo-zekerheid en de daaruit voortvloeiende minimale n_basis.
  strata <- steekproeven |>
    dplyr::mutate(
      cert = purrr::pmap_dbl(
        list(.data$ihr, .data$ibr, .data$car),
        haro_nog_nodige_zekerheid
      ),
      n_basis = ceiling(purrr::pmap_dbl(
        list(.data$verwachte_foutfractie, .data$materialiteit, .data$cert),
        ~ drawsneeded(
          posited_defect_rate = ..1,
          allowed_defect_rate = ..2,
          cert = ..3,
          distribution = dist_eng
        )
      )),
      n_laag = .data$n_basis,
      k_laag = .data$n_laag * .data$verwachte_foutfractie,
      n_totaal = .data$n_laag + .data$n_hoog,
      waarde_hoog = .data$fout_hoog + .data$goed_hoog,
      waarde_populatie = .data$waarde_laag + .data$waarde_hoog
    )

  # Schoon de tijdelijke zekerheidskolom op alvorens de tabel te retourneren.
  strata$cert <- NULL
  return(strata)
}

#' @title
#' Evalueer en selecteer de optimale strata voor de volgende parallelle klimstap
#'
#' @description
#' Deze functie berekent via exacte convolutie welke strata bij ophoging
#' de grootste foutreductie opleveren.
#'
#' @param huidige_strata De actuele tibble met de status van de strata.
#' @param model Het statistische model ("binomiaal" of "poisson").
#' @param klim_granulariteit De resolutie van de kanskromme.
#' @param totale_zekerheid Het algehele zekerheidsniveau.
#' @returns Een vector met indices van de strata die moeten worden opgehoogd.
#' @export
vind_beste_strata_groep <- function(huidige_strata, model, klim_granulariteit, totale_zekerheid) {
  # Definieer de interne functie voor de gelijktijdige FFT convolutie-evaluatie.
  calc_max_fout_klim <- function(s_data) {
    res <- eval_stratified(
      steekproeven = s_data,
      model = model,
      zekerheid = totale_zekerheid,
      methode = "FFT samen",
      granulariteit = klim_granulariteit,
      vergelijk = FALSE
    )
    return(res$max_fout_convolutie)
  }

  # Initialiseer de zoekparameters en meet de huidige nulmeting van de
  # kanskromme.
  huidige_fout_klim <- calc_max_fout_klim(huidige_strata)
  beste_strata <- integer(0)
  beste_verbetering <- 0

  # Loop door elk stratum heen en bereken de foutreductie
  # bij een extra post.
  for (i in 1:nrow(huidige_strata)) {
    test_strata <- huidige_strata
    test_strata$n_laag[i] <- test_strata$n_laag[i] + 1000
    test_strata$k_laag[i] <- test_strata$n_laag[i] * test_strata$verwachte_foutfractie[i]
    test_strata$n_totaal[i] <- test_strata$n_laag[i] + test_strata$n_hoog[i]
    verbetering <- huidige_fout_klim - calc_max_fout_klim(test_strata)
    stopifnot(verbetering >= 0)

    if (verbetering > beste_verbetering) {
      beste_verbetering <- verbetering
      beste_strata <- i
    } else if (verbetering == beste_verbetering && beste_verbetering > 0) {
      beste_strata <- c(beste_strata, i)
    }
  }

  # Aanpassen: als alle verbeteringen gelijk zijn, dan alles ophogen!

  # Aanpassen: als alle verbeteringen 0 zijn: dat mag niet!

  # Wijzig de selectie naar alle strata parallel als de reële winst naar
  # exact 0 afgerond wordt.
  if (length(beste_strata) == 0) {
    beste_strata <- 1:nrow(huidige_strata)
  }

  return(beste_strata)
}
