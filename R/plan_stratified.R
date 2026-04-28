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
#' @param granulariteit Bepaalt de nauwkeurigheid van de berekening.
#'   Bij \code{"FFT"} is dit het aantal stappen op de kanskromme-as.
#'   Bij \code{"MonteCarlo"} is dit het aantal toevalsiteraties.
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
                            granulariteit = 10000,
                            max_iteraties = 10000) {

  model <- match.arg(model)
  methode <- match.arg(methode)
  stopifnot(is_tibble(steekproeven))
  stopifnot(is.numeric(totale_materialiteit), totale_materialiteit > 0, totale_materialiteit < 1)

  # Vertaal de Nederlandse modelnaam naar de Engelse voor drawsneeded.
  dist_eng <- if(model == "binomiaal") "binomial" else "Poisson"

  # Preparatie van de invoer.
  {
    vereist <- c("naam", "waarde_laag", "verwachte_foutfractie", "ihr", "ibr", "car", "materialiteit")
    ontbrekend <- setdiff(vereist, colnames(steekproeven))
    if (length(ontbrekend) > 0) {
      stop(paste("Ontbrekende kolommen in invoer:", paste(ontbrekend, collapse = ", ")))
    }

    if (any(duplicated(steekproeven$naam))) {
      dubbele <- unique(steekproeven$naam[duplicated(steekproeven$naam)])
      stop(paste("Planningsfout: De volgende stratumnamen komen vaker dan \u00e9\u00e9n keer voor:",
                 paste(dubbele, collapse = ", ")))
    }

    if (!"fout_hoog" %in% colnames(steekproeven)) steekproeven$fout_hoog <- 0
    if (!"goed_hoog" %in% colnames(steekproeven)) steekproeven$goed_hoog <- 0
    if (!"n_hoog" %in% colnames(steekproeven)) steekproeven$n_hoog <- 0
  }

  # Vooraf validatie van onmogelijke situaties.
  {
    totale_populatie <- sum(steekproeven$waarde_laag + steekproeven$fout_hoog + steekproeven$goed_hoog)

    if (totale_populatie <= 0) stop("Planningsfout: De totale populatiewaarde is 0 of negatief.")

    bekende_foutfractie <- sum(steekproeven$fout_hoog) / totale_populatie
    if (bekende_foutfractie >= totale_materialiteit) {
      stop(sprintf(
        "Planningsfout: De reeds bekende fout in de hoogstrata (%.4f) is al groter dan of gelijk aan de totale materialiteit (%.4f).",
        bekende_foutfractie, totale_materialiteit
      ))
    }

    # Voor nu verbieden we dit.
    # Toestaan vereist dat we ergens teruggeven in welke strata dit voorkomt.
    # Die complicatie laten we nu even weg.
    # In de toekomst gaan we dit wel toestaan.
    # Je kunt je namelijk voorstellen dat je toch wilt weten als gebruiker
    # of het mogelijk is om onder de totale materialiteit te blijven, en voor lief
    # neemt dat sommige steekproeven qua maximale foutfractie boven de materialiteit
    # uitkomen.
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
  }

  # Basisplanning (de minimale n per stratum).
  {
    strata <- steekproeven |>
      mutate(
        cert = purrr::pmap_dbl(list(ihr, ibr, car), haro_nog_nodige_zekerheid),

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
      res <- eval_stratified(
        steekproeven = s_data,
        model = model,
        zekerheid = totale_zekerheid,
        methode = methode,
        granulariteit = granulariteit,
        vergelijk = FALSE
      )
      return(res$max_fout_convolutie)
    }

    huidige_max_fout <- calc_current_max_error(strata)
  }

  # Greedy optimalisatie loop: we verhogen steeds n_laag met 1, van dat stratum waarvoor we het beste resultaat krijgen.
  {
    iteratie <- 0

    while (huidige_max_fout > totale_materialiteit && iteratie < max_iteraties) {
      iteratie <- iteratie + 1
      beste_stratum <- NA
      beste_verbetering <- -Inf

      for (i in 1:nrow(strata)) {
        # We maken een kopie van strata: test_strata.
        test_strata <- strata

        # Voor die kopie verhogen we de i-de n_laag met 1.
        test_strata$n_laag[i] <- test_strata$n_laag[i] + 1

        # We passen de i-de k_laag navenant aan.
        test_strata$k_laag[i] <- test_strata$n_laag[i] * test_strata$verwachte_foutfractie[i]

        # We passen de i-de n_totaal navenant aan.
        test_strata$n_totaal[i] <- test_strata$n_laag[i] + test_strata$n_hoog[i]

        # We berekenen nu over het gehele zo gemaakte test_strata wat de maximale fout zou worden.
        test_max_fout <- calc_current_max_error(test_strata)

        # We kijken wat dit oplevert aan verbetering: hoeveel wordt de huidige max fout kleiner.
        verbetering <- huidige_max_fout - test_max_fout

        # Als de huidige verbetering de beste tot nu toe is, dan noteren we dit in beste_verbetering en beste_stratum.
        if (verbetering > beste_verbetering) {
          beste_verbetering <- verbetering
          beste_stratum <- i
        }
      }

      # Voer de beste verbetering door in de definitieve strata.
      strata$n_laag[beste_stratum] <- strata$n_laag[beste_stratum] + 1
      strata$k_laag[beste_stratum] <- strata$n_laag[beste_stratum] * strata$verwachte_foutfractie[beste_stratum]
      strata$n_totaal[beste_stratum] <- strata$n_laag[beste_stratum] + strata$n_hoog[beste_stratum]

      huidige_max_fout <- calc_current_max_error(strata)
    }

    # Waarschuw bij bereiken max_iteraties.
    if (iteratie >= max_iteraties) {
      warning(paste0("Maximale aantal iteraties = maximaal aantal extra steken, ", max_iteraties, ", bereikt."))
    }
  }

  # Afronding en output.
  {
    strata$cert <- NULL

    if ("n_definitief" %in% colnames(strata)) {
      strata$n_definitief <- strata$n_laag
      strata$n_laag <- NULL
    } else {
      strata <- dplyr::rename(strata, n_definitief = n_laag)
    }

    attr(strata, "geplande_max_fout_totaal") <- huidige_max_fout

    return(strata)
  }
}
