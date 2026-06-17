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
#' Deze functie berekent via exacte convolutie welke strata bij een ophoging van
#' exact 1 post de grootste foutreductie opleveren. Omdat max_fout continu is,
#' geeft de kleinste ophoogstap direct het optimale sturingssignaal.
#'
#' @param huidige_strata De actuele tibble met de status van de strata.
#' @param model Het statistische model ("binomiaal" of "poisson").
#' @param klim_granulariteit De resolutie van de kanskromme.
#' @param totale_zekerheid Het algehele zekerheidsniveau.
#' @returns Een vector met indices van de strata die moeten worden opgehoogd.
#' @export
vind_beste_strata_groep <- function(huidige_strata,
                                    model,
                                    klim_granulariteit = 1000000,
                                    totale_zekerheid = 0.95) {
  # Definieer de interne functie voor de gelijktijdige FFT convolutie-evaluatie.
  {
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
  }

  # Initialiseer de nulmeting van de kanskromme en bepaal het aantal strata.
  huidige_fout_klim <- calc_max_fout_klim(huidige_strata)
  n_strata <- nrow(huidige_strata)

  # Bereken voor een ophoog van exact 1 de verbetering van de maximale fout per stratum.
  {
    verbetering <- numeric(n_strata)

    for (i in 1:n_strata) {
      test_strata <- huidige_strata
      test_strata$n_laag[i] <- test_strata$n_laag[i] + 1
      test_strata$k_laag[i] <- test_strata$n_laag[i] * test_strata$verwachte_foutfractie[i]

      nieuwe_fout <- calc_max_fout_klim(test_strata)
      foutreductie <- huidige_fout_klim - nieuwe_fout

      # Vang numerieke artefacten af die ontstaan door zwevendekommagetallen of interpolatie op grove grids.
      {
        if (foutreductie < 0) {
          foutreductie <- 0
        }
      }

      verbetering[[i]] <- foutreductie
    }
  }

  # Bepaal wiskundig welke strata het maximale rendement opleveren voor deze ene stap.
  {
    max_verbetering <- max(verbetering)

    if (max_verbetering > 0) {
      # We gebruiken een minieme tolerantie om afrondingsverschillen bij exact gelijk presterende strata op te vangen.
      beste_strata <- which(verbetering >= max_verbetering - 1e-12)
    } else {
      # Vang het fenomeen af waarbij een extreem grof grid blind is voor kleine verbeteringen.
      # Val terug op een analytische proxy: het stratum dat relatief de meeste onzekerheid toevoegt.
      {
        # Voorkom deling door nul door n_laag minimaal op 1 te zetten in de noemer.
        veilige_n <- pmax(huidige_strata$n_laag, 1)
        proxy_onzekerheid <- huidige_strata$waarde_laag / sqrt(veilige_n)

        # Selecteer het stratum met de hoogste proxy-waarde.
        max_proxy <- max(proxy_onzekerheid)
        beste_strata <- which(proxy_onzekerheid >= max_proxy - 1e-6)
      }
    }
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
#' @param ... Extra argumenten om flexibel om te gaan met 'totale_materialiteit'
#'   en 'totale_zekerheid'.
#' @returns De tibble met de definitieve, geoptimaliseerde n_laag.
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

  # Valideer vooraf of er individuele strata zijn waar de verwachte
  # foutfractie de materialiteit al overschrijdt.
  if (any(steekproeven$verwachte_foutfractie >= materialiteit, na.rm = TRUE)) {
    stop("verwachte foutfractie groter dan of gelijk aan de totale materialiteit")
  }

  # Controleer op stratum-inconsistentie waarbij de verwachte foutfractie
  # groter of gelijk is aan de stratum-materialiteit.
  if (any(steekproeven$verwachte_foutfractie >= steekproeven$materialiteit,
          na.rm = TRUE)) {
    stop("verwachte foutfractie groter dan of gelijk aan de stratum-materialiteit")
  }

  # Valideer de modelkeuze en bereken de initiële basissteekproefomvang
  # op basis van de invoerdata.
  {
    model <- match.arg(model)
    strata <- plan_stratified_basis(steekproeven, model = model)
  }

  # Bereken de totale geldswaarde van de populatie en de
  # absolute bekende fout binnen het hoogstratum.
  {
    totale_pop_waarde <- sum(strata$waarde_populatie, na.rm = TRUE)
    totale_fout_hoog <- sum(strata$fout_hoog, na.rm = TRUE)
  }

  # Werp een fout op als de bekende fout in het hoogstratum de
  # algehele materialiteitsgrens al overschrijdt.
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

  # Start de stapsgewijze klimloop totdat de fout onder
  # de gestelde materialiteit zakt (met een ruime veiligheidsmarge tegen oneindige loops).
  iteratie <- 0
  while (huidige_fout > materialiteit && iteratie < 10000) {
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

  # Voeg de door het testscript verwachte synoniemkolom n_definitief en
  # het kwaliteitsattribuut toe.
  {
    strata$n_definitief <- strata$n_laag
    attr(strata, "geplande_max_fout_totaal") <- huidige_fout
  }

  return(strata)
}
