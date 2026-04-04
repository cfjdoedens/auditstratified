#' @title
#' Plan de steekproefomvang voor meerdere strata (gestratificeerde steekproef)
#'
#' @description
#' Deze functie berekent de optimale steekproefomvang (n) en verwachte fout (k)
#' voor afzonderlijke strata, zodat deze bij gezamenlijke evaluatie onder
#' de algehele materialiteit blijven.
#'
#' @param steekproeven Een tibble met de planningsgegevens.
#' @param totale_materialiteit De maximaal toegestane foutfractie voor de gehele populatie.
#' @param totale_zekerheid Het algehele zekerheidsniveau (bijv. 0.95).
#' @param model Keuze uit \code{"binomiaal"} (standaard) of \code{"poisson"}.
#' @param methode Rekenmethode voor evaluatie. \code{"FFT"} is sterk aanbevolen voor snelheid.
#' @param max_iteraties Veiligheidslimiet voor de greedy loop om vastlopen te voorkomen.
#'
#' @returns Een verrijkte tibble met de berekende \code{n_basis}, \code{n_definitief},
#' \code{k_laag}, \code{n_totaal} en de uiteindelijke \code{geplande_max_fout_totaal} als attribuut.
#'
#' @export
#' @importFrom dplyr mutate pull rename
#' @importFrom tibble is_tibble
plan_stratified <- function(steekproeven,
                            totale_materialiteit,
                            totale_zekerheid = 0.95,
                            model = c("binomiaal", "poisson"),
                            methode = c("FFT", "MonteCarlo"),
                            max_iteraties = 1000) {

  model <- match.arg(model)
  methode <- match.arg(methode)
  stopifnot(is_tibble(steekproeven))
  stopifnot(is.numeric(totale_materialiteit), totale_materialiteit > 0, totale_materialiteit < 1)

  # Vertaal de Nederlandse modelnaam naar de Engelse voor drawsneeded()
  dist_eng <- if(model == "binomiaal") "binomial" else "Poisson"

  # =========================================================================
  # 1. PREPARATIE VAN DE INVOER
  # =========================================================================
  vereist <- c("naam", "waarde_laag", "verwachte_foutfractie", "ihr", "ibr", "car", "materialiteit")
  ontbrekend <- setdiff(vereist, colnames(steekproeven))
  if (length(ontbrekend) > 0) {
    stop(paste("Ontbrekende kolommen in invoer:", paste(ontbrekend, collapse = ", ")))
  }

  if (!"fout_hoog" %in% colnames(steekproeven)) steekproeven$fout_hoog <- 0
  if (!"goed_hoog" %in% colnames(steekproeven)) steekproeven$goed_hoog <- 0
  if (!"n_hoog" %in% colnames(steekproeven)) steekproeven$n_hoog <- 0

  # =========================================================================
  # 2. UPFRONT VALIDATIE VAN ONMOGELIJKE SITUATIES
  # =========================================================================
  totale_populatie <- sum(steekproeven$waarde_laag + steekproeven$fout_hoog + steekproeven$goed_hoog)

  if (totale_populatie <= 0) stop("Planningsfout: De totale populatiewaarde is 0 of negatief.")

  bekende_foutfractie <- sum(steekproeven$fout_hoog) / totale_populatie
  if (bekende_foutfractie >= totale_materialiteit) {
    stop(sprintf(
      "Planningsfout: De reeds bekende fout in de hoogstrata (%.4f) is al groter dan of gelijk aan de totale materialiteit (%.4f).",
      bekende_foutfractie, totale_materialiteit
    ))
  }

  if (any(steekproeven$verwachte_foutfractie >= steekproeven$materialiteit)) {
    foute_strata <- steekproeven$naam[steekproeven$verwachte_foutfractie >= steekproeven$materialiteit]
    stop(paste("Planningsfout: In de volgende strata is de verwachte foutfractie groter dan of gelijk aan de stratum-materialiteit:",
               paste(foute_strata, collapse = ", ")))
  }

  totale_verwachte_fout_geld <- sum(steekproeven$waarde_laag * steekproeven$verwachte_foutfractie) + sum(steekproeven$fout_hoog)
  algehele_verwachte_foutfractie <- totale_verwachte_fout_geld / totale_populatie

  if (algehele_verwachte_foutfractie >= totale_materialiteit) {
    stop(sprintf(
      "Planningsfout: De algehele verwachte foutfractie (%.4f) is al groter dan of gelijk aan de totale materialiteit (%.4f). Meer posten trekken zal dit niet oplossen.",
      algehele_verwachte_foutfractie, totale_materialiteit
    ))
  }

  # =========================================================================
  # 3. BASISPLANNING (De minimale N per stratum)
  # =========================================================================
  strata <- steekproeven |>
    mutate(
      cert = purrr::pmap_dbl(list(ihr, ibr, car), haro_nog_nodige_zekerheid),

      # Gebruik de Engelse vertaling voor drawsneeded()
      n_basis = ceiling(purrr::pmap_dbl(
        list(verwachte_foutfractie, materialiteit, cert),
        ~ drawsneeded(posited_defect_rate = ..1, allowed_defect_rate = ..2, cert = ..3, distribution = dist_eng)
      )),

      n_laag = n_basis,
      k_laag = n_laag * verwachte_foutfractie,
      n_totaal = n_laag + n_hoog,
      waarde_hoog = fout_hoog + goed_hoog,
      waarde_populatie = waarde_laag + waarde_hoog
    )

  calc_current_max_error <- function(s_data) {
    # eval_stratified verwacht wel de Nederlandse 'model' parameter
    res <- eval_stratified(
      steekproeven = s_data,
      model = model,
      zekerheid = totale_zekerheid,
      methode = methode,
      vergelijk = FALSE
    )
    return(res$max_fout_convolutie)
  }

  huidige_max_fout <- calc_current_max_error(strata)

  # =========================================================================
  # 4. GREEDY OPTIMALISATIE LOOP
  # =========================================================================
  iteratie <- 0

  while (huidige_max_fout > totale_materialiteit && iteratie < max_iteraties) {
    iteratie <- iteratie + 1
    beste_stratum <- NA
    beste_verbetering <- 0

    for (i in 1:nrow(strata)) {
      test_strata <- strata
      test_strata$n_laag[i] <- test_strata$n_laag[i] + 1
      test_strata$k_laag[i] <- test_strata$n_laag[i] * test_strata$verwachte_foutfractie[i]
      test_strata$n_totaal[i] <- test_strata$n_laag[i] + test_strata$n_hoog[i]

      test_max_fout <- calc_current_max_error(test_strata)
      verbetering <- huidige_max_fout - test_max_fout

      if (verbetering > beste_verbetering) {
        beste_verbetering <- verbetering
        beste_stratum <- i
      }
    }

    if (is.na(beste_stratum) || beste_verbetering <= 0) {
      warning("Algoritme kan de totale materialiteit niet bereiken. Optimalisatie gestopt.")
      break
    }

    strata$n_laag[beste_stratum] <- strata$n_laag[beste_stratum] + 1
    strata$k_laag[beste_stratum] <- strata$n_laag[beste_stratum] * strata$verwachte_foutfractie[beste_stratum]
    strata$n_totaal[beste_stratum] <- strata$n_laag[beste_stratum] + strata$n_hoog[beste_stratum]

    huidige_max_fout <- huidige_max_fout - beste_verbetering
  }

  if (iteratie >= max_iteraties) {
    warning("Maximale aantal iteraties bereikt. Het resultaat voldoet mogelijk nog niet aan de totale materialiteit.")
  }

  # =========================================================================
  # 5. AFRONDING EN OUTPUT
  # =========================================================================
  strata$cert <- NULL

  # Hernoem n_laag naar n_definitief voor de eindgebruiker
  strata <- dplyr::rename(strata, n_definitief = n_laag)

  attr(strata, "geplande_max_fout_totaal") <- huidige_max_fout

  return(strata)
}
