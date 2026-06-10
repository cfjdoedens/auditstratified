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
plan_stratified_basis <- function(steekproeven,
                                  model = c("binomiaal", "poisson")) {
  # Valideer het gekozen model en converteer naar de juiste Engelse distributieterm.
  model <- match.arg(model)
  dist_eng <- if (model == "binomiaal")
    "binomial"
  else
    "Poisson"

  # Bereken per stratum de HARo-zekerheid en de daaruit voortvloeiende minimale n_basis.
  strata <- steekproeven |>
    dplyr::mutate(
      cert = purrr::pmap_dbl(
        list(.data$ihr, .data$ibr, .data$car),
        haro_nog_nodige_zekerheid
      ),
      n_basis = ceiling(purrr::pmap_dbl(
        list(
          .data$verwachte_foutfractie,
          .data$materialiteit,
          .data$cert
        ),
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

  # Schoon de tijdelijke zekerheidskolom opschonen alvorens de tabel te retourneren.
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
vind_beste_strata_groep <- function(huidige_strata,
                                    model,
                                    klim_granulariteit = 10000,
                                    totale_zekerheid = 0.95) {
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

  # Initialiseer de zoekparameters en meet de huidige nulmeting van de kanskromme.
  huidige_fout_klim <- calc_max_fout_klim(huidige_strata)
  beste_strata <- integer(0)
  beste_verbetering <- 0

  # Loop door elk stratum heen en bereken de foutreductie bij een extra post.
  for (i in 1:nrow(huidige_strata)) {
    # Maak een zuivere werkkopie aan van de huidige strata-matrix.
    test_strata <- huidige_strata

    # Voer een tijdelijke parallelle ophoogstap uit om het effect te isoleren.
    test_strata$n_laag[i] <- test_strata$n_laag[i] + 1
    test_strata$k_laag[i] <- test_strata$n_laag[i] * test_strata$verwachte_foutfractie[i]
    test_strata$n_totaal[i] <- test_strata$n_laag[i] + test_strata$n_hoog[i]

    # Bereken de foutreductie ten opzichte van de nulmeting.
    verbetering <- huidige_fout_klim - calc_max_fout_klim(test_strata)
    stopifnot(verbetering >= 0)

    # Beoordeel of deze ophoogstap een effectievere reductie oplevert dan eerdere strata.
    if (verbetering > beste_verbetering) {
      beste_verbetering <- verbetering
      beste_strata <- i
    } else if (verbetering == beste_verbetering &&
               beste_verbetering > 0) {
      beste_strata <- c(beste_strata, i)
    }
  }

  # Wijzig de selectie naar alle strata parallel als de reële winst naar exact 0 afgerond wordt.
  if (length(beste_strata) == 0) {
    beste_strata <- 1:nrow(huidige_strata)
  }

  return(beste_strata)
}

#' @title
#' Plan de volledige optimale steekproefverdeling via parallelle convolutie-optimalisatie
#'
#' @description
#' Deze functie voert de volledige planningscyclus uit: het start met de basisomvang
#' en verhoogt daarna stapsgewijs de omvang van de meest effectieve strata totdat
#' de gecombineerde convolutiefout onder de algehele materialiteit zakt.
#'
#' @param steekproeven Een tibble met de planningsgegevens per stratum.
#' @param model Het statistische model ("binomiaal" of "poisson").
#' @param materialiteit De algehele materialiteitsgrens.
#' @param zekerheid Het gewenste algehele zekerheidsniveau.
#' @param granulariteit De resolutie voor de FFT-convolutieberekeningen.
#' @param ... Extra argumenten om flexibel om te gaan met 'totale_materialiteit' en 'totale_zekerheid'.
#' @returns De tibble met de definitieve, geoptimaliseerde n_laag en n_totaal.
#' @export
plan_stratified <- function(steekproeven,
                            model = c("binomiaal", "poisson"),
                            materialiteit = NULL,
                            zekerheid = 0.95,
                            granulariteit = 10000,
                            ...) {
  # Vang variaties in argumentnamen op die door testscripts of wrappers gebruikt worden.
  {
    args <- list(...)
    if (is.null(materialiteit) &&
        !is.null(args$totale_materialiteit))
      materialiteit <- args$totale_materialiteit
    if (!is.null(args$totale_zekerheid))
      zekerheid <- args$totale_zekerheid
  }

  # Controleer vooraf op dubbele stratumnamen in de invoertabel.
  if (any(duplicated(steekproeven$naam))) {
    dubbele_naam <- steekproeven$naam[duplicated(steekproeven$naam)][1]
    stop(paste0("Namen komen vaker dan 1 keer voor: ", dubbele_naam))
  }

  # Valideer vooraf of er individuele strata zijn waar de verwachte foutfractie de materialiteit al overschrijdt.
  if (any(steekproeven$verwachte_foutfractie >= materialiteit, na.rm = TRUE)) {
    stop("verwachte foutfractie groter dan of gelijk aan de totale materialiteit")
  }

  # Controleer op stratum-inconsistentie waarbij de verwachte foutfractie groter of gelijk is aan de stratum-materialiteit.
  if (any(steekproeven$verwachte_foutfractie >= steekproeven$materialiteit,
          na.rm = TRUE)) {
    stop("verwachte foutfractie groter dan of gelijk aan de stratum-materialiteit")
  }

  # Valideer de modelkeuze en bereken de initiële basissteekproefomvang op basis van de invoerdata.
  {
    model <- match.arg(model)
    strata <- plan_stratified_basis(steekproeven, model = model)
  }

  # Bereken de totale geldwaarde van de populatie en de absolute bekende fout binnen het hoogstratum.
  {
    totale_pop_waarde <- sum(strata$waarde_populatie, na.rm = TRUE)
    totale_fout_hoog <- sum(strata$fout_hoog, na.rm = TRUE)
  }

  # Werp een fout op als de bekende fout in het hoogstratum de algehele materialiteitsgrens al overschrijdt.
  if (totale_pop_waarde > 0 &&
      (totale_fout_hoog / totale_pop_waarde) >= materialiteit) {
    stop("reeds bekende fout in de hoogstrata")
  }

  # Bereken de initiële algehele convolutiefout van de startpositie.
  huidige_fout <- eval_stratified(
    strata,
    model = model,
    zekerheid = zekerheid,
    methode = "FFT samen",
    granulariteit = granulariteit,
    vergelijk = FALSE
  )$max_fout_convolutie

  # Start de stapsgewijze klimloop totdat de fout onder de gestelde materialiteit zakt.
  iteratie <- 0
  while (huidige_fout > materialiteit && iteratie < 1000) {
    iteratie <- iteratie + 1
    beste_strata_indices <- vind_beste_strata_groep(
      strata,
      model = model,
      klim_granulariteit = granulariteit,
      totale_zekerheid = zekerheid
    )

    # Hoog de geselecteerde strata parallel op met één post.
    for (beste_stratum in beste_strata_indices) {
      strata$n_laag[beste_stratum] <- strata$n_laag[beste_stratum] + 1
      strata$k_laag[beste_stratum] <- strata$n_laag[beste_stratum] * strata$verwachte_foutfractie[beste_stratum]
      strata$n_totaal[beste_stratum] <- strata$n_laag[beste_stratum] + strata$n_hoog[beste_stratum]
    }

    # Evalueer de nieuwe algehele fout na de parallelle ophoogstap.
    huidige_fout <- eval_stratified(
      strata,
      model = model,
      zekerheid = zekerheid,
      methode = "FFT samen",
      granulariteit = granulariteit,
      vergelijk = FALSE
    )$max_fout_convolutie
  }

  # Voeg de door het testscript verwachte synoniemkolom n_definitief en het kwaliteitsattribuut toe.
  {
    strata$n_definitief <- strata$n_laag
    attr(strata, "geplande_max_fout_totaal") <- huidige_fout
  }

  return(strata)
}
