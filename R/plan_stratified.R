#' @title
#' Plan de steekproefomvang voor meerdere strata (gestratificeerde steekproef)
#'
#' @description
#' Deze functie tracht de optimale, dat wil zeggen minimale,
#' steekproefomvang (n) en verwachte fout (k)
#' voor afzonderlijke strata te berekenen, zodat deze bij gezamenlijke evaluatie onder
#' de algehele materialiteit blijven.
#'
#' @details
#' Dat optimaliseren gaat via een heuvelklimalgoritme. En wel in drie stappen.
#'
#' Stap 1.
#' Construeren uitgangssituatie.
#' We bepalen per stratum het minimum aantal steekproevenposten dat er nodig is
#' om gezien de verwachte foutfractie van dat stratum
#' onder de materialiteit voor dat stratum te komen.
#'
#' Stap 2.
#' Het eigenlijke klimmen.
#' We verhogen steeds voor elk van de strata de steekproefomvang met 1.
#' We kijken dan welke verhoging de meeste winst oplevert in de vorm
#' van verkleining van de maximale fout. Dat kijken doen we door
#' de maximale fout te berekenen via een
#' algoritme A. Die verhoging kiezen we dan.
#' Daar gaan we mee door tot
#' de maximale fout onder de materialiteit is.
#'
#' Stap 3.
#' Valideren.
#' We valideren tenslotte eventueel de zo gevonden verdeling van steken
#' met een algoritme B.
#'
#' Voor A en B zijn verschillende keuzen en parameters
#' beschikbaar.
#'
#' De gebruiker kan hier zijn voordeel mee doen door A zo te kiezen dat
#' het optimaal (snel en betrouwbaar) is voor het eigenlijke heuvelklimmen en dat B
#' optimaal (juist, maar mogelijk trager) is voor het bepalen dat bij de
#' gekozen verdeling van steken we inderdaad onder de materialiteit
#' uitkomen.
#'
#' Achtergrond van deze architectuur is dat gedurende het heuvelklimmen
#' de verkeerde afslag genomen kan worden. Dat nemen van de
#' verkeerde afslag komt dan door numerieke ruis. De kunst is om
#' die kans zo klein mogelijk te houden terwijl de computationele kosten
#' tegelijk niet de pan uit rijzen.
#'
#' Er is geprobeerd om redelijk bruikbare verstekwaarden voor A en B te kiezen.
#'
#' @param steekproeven Een tibble met de planningsgegevens.Deze
#'   bestaat uit de volgende kolommen:
#'   - naam
#'   - waarde_laag
#'   - verwachte_foutfractie
#'   - ihr
#'   - ibr
#'   - car
#'   - materialiteit
#'   - fout_hoog
#'   - goed_hoog
#'   - n_hoog
#' @param totale_materialiteit De maximaal toegestane foutfractie voor de gehele populatie.
#' @param totale_zekerheid Het algehele zekerheidsniveau (bijv. 0.95).
#' @param model Het statistische model dat gebruikt wordt voor de extrapolatie.
#'   Keuze uit \code{"binomiaal"} (standaard) of \code{"poisson"}.
#' @param klim_methode Methode voor klimdeel van de optimalisatie.
#'   Keuze uit:
#'   - \code{"direct"}
#'   - \code{"FFT"}
#'   - \code{"FFT gelijktijdig"} (standaard)
#'   - \code{"Monte Carlo"}.
#'
#'   \code{"direct"},
#'   \code{"FFT"} en
#'   \code{"FFT gelijktijdig"} zijn deterministische algoritmen.
#'   en zijn opvolgend meer
#'   efficiente vormen van hetzelfde convolutie-algoritme.
#'   \code{"Monte Carlo"} is niet-deterministisch, dus gebaseerd op toeval.
#'   Dat betekent dat het resultaat ervan afhangt van de startwaarde van de
#'   R toevalsgenerator, die je kunt opgeven via de parameter \code{start}.
#' @param klim_granulariteit Bepaalt de nauwkeurigheid van de berekening.
#'   Bij \code{"direct"}, \code{"FFT"} en \code{"FFT gelijktijdig"},
#'   is dit het aantal stappen op de
#'   kanskromme-as.
#'   Alledrie hebben ze als verstekwaarde 10.000.
#'   Bij \code{"Monte Carlo"} is dit het aantal toevalsiteraties.
#'   De verstekwaarde hiervoor is 1.000.000.
#' @param validatie_methode Methode voor validatie.
#'   Heeft dezelfde keuzes als bij klim_methode.
#' @param validatie_granulariteit Granulariteit voor validatie.
#'   Heeft dezelfde keuzes als bij klim_granulariteit.
#'   Als verstekwaarden geldt voor \code{"direct"}, \code{"FFT"}
#'   en \code{"FFT gelijktijdig"} 25.000,
#'   en voor \code{"Monte Carlo"}  10.000.000.
#' @param start De vaste startwaarde voor de toevalsgenerator
#'   (alleen voor Monte Carlo).
#'   Een startwaarde van 0 betekent dat de startwaarde op de systeemklok,
#'   is gebaseerd, dus min of meer 'echt' op toeval is gebaseerd.
#' @param max_iteraties Limiet voor hoeveel extra steekproefposten
#'   we willen trekken. Dit beschermt ook tegen een eventuele
#'   eindeloze lus.
#'
#' @returns De tibble steekproeven verrijkt met de berekende
#'   \code{waarde_hoog}, # <- fout_hoog + goed_hoog
#'   \code{waarde_populatie}, # <- waarde_laag + waarde_hoog
#'   \code{n_basis}, # De n nodig om per steekproef om onder de
#'                   # materialiteit voor die steekproef te blijven.
#'   \code{n_definitief}, # n_basis plus de extra nodige steken voor
#'                        # het lage stratum om onder de totale
#'                        # materialiteit te blijven.
#'   \code{k_laag},
#'   \code{n_totaal} en
#'   met als attribuut de uiteindelijke
#'   \code{geplande_max_fout_totaal}.
#'
#' @export
#' @importFrom dplyr mutate pull rename
#' @importFrom tibble is_tibble
plan_stratified <- function(steekproeven,
                            totale_materialiteit,
                            totale_zekerheid = 0.95,
                            model = c("binomiaal", "poisson"),
                            klim_methode = c("FFT", "FFT gelijktijdig", "direct", "Monte Carlo"),
                            klim_granulariteit = NULL,
                            # Verstekwaarde bepalen we in functie zelf.
                            validatie_methode = c("FFT", "FFT gelijktijdig", "direct", "Monte Carlo"),
                            validatie_granulariteit = NULL,
                            # Verstekwaarde bepalen we in functie zelf.
                            start = 1,
                            max_iteraties = 1e4) {
  # Valideer en bepaal de argumentkeuzes.
  model <- match.arg(model)
  klim_methode <- match.arg(klim_methode)
  validatie_methode <- match.arg(validatie_methode)

  # Bepaal dynamische verstekwaarden.
  {
    klim_granulariteit <-
      klim_granulariteit %||% ifelse(klim_methode == "Monte Carlo", 1e6, 1e4)
    validatie_granulariteit <-
      validatie_granulariteit %||% ifelse(klim_methode == "Monte Carlo", 1e7, 25e3)
  }

  # Controle invoer.
  stopifnot(is_tibble(steekproeven)) # Verdere controle komt nog.
  stopifnot(is.numeric(totale_materialiteit))
  stopifnot(0 < totale_materialiteit)
  stopifnot(totale_materialiteit < 1)
  stopifnot(is.numeric(totale_zekerheid))
  stopifnot(0 < totale_zekerheid)
  stopifnot(totale_zekerheid < 1)
  stopifnot(is.numeric(klim_granulariteit))
  stopifnot(0 < klim_granulariteit)
  stopifnot(is.numeric(validatie_granulariteit))
  stopifnot(0 < validatie_granulariteit)
  stopifnot(is.numeric(start))
  stopifnot(is.numeric(max_iteraties))
  stopifnot(0 < max_iteraties)

  # Verdere controle steekproeven.
  {
    kols_steekproeven <- c(
      "naam",
      "waarde_laag",
      "verwachte_foutfractie",
      "ihr",
      "ibr",
      "car",
      "materialiteit",
      "fout_hoog",
      "goed_hoog",
      "n_hoog"
    )
    ontbrekend <- setdiff(kols_steekproeven, colnames(steekproeven))
    if (length(ontbrekend) > 0) {
      stop(paste(
        "Ontbrekende kolommen in steekproeven:",
        paste(ontbrekend, collapse = ", ")
      ))
    }

    if (any(duplicated(steekproeven$naam))) {
      dubbele <- unique(steekproeven$naam[duplicated(steekproeven$naam)])
      stop(
        paste(
          "Planningsfout: De volgende stratumnamen komen vaker dan \u00e9\u00e9n keer voor:",
          paste(dubbele, collapse = ", ")
        )
      )
    }

    # controle op datatypes en waarden-domeinen van de kolommen.
    {
      if (!is.character(steekproeven$naam)) {
        stop("Planningsfout: 'naam' moet tekst (character) zijn.")
      }

      if (!is.numeric(steekproeven$waarde_laag) ||
          any(steekproeven$waarde_laag < 0)) {
        stop("Planningsfout: 'waarde_laag' moet numeriek en >= 0 zijn.")
      }

      if (!is.numeric(steekproeven$verwachte_foutfractie) ||
          any(
            steekproeven$verwachte_foutfractie < 0 |
            steekproeven$verwachte_foutfractie >= 1
          )) {
        stop("Planningsfout: 'verwachte_foutfractie' moet numeriek, >= 0 en < 1 zijn.")
      }

      if (any(!steekproeven$ihr %in% c("H", "M", "L"))) {
        stop("Planningsfout: 'ihr' mag alleen 'H', 'M' of 'L' bevatten.")
      }

      if (any(!steekproeven$ibr %in% c("H", "M", "L"))) {
        stop("Planningsfout: 'ibr' mag alleen 'H', 'M' of 'L' bevatten.")
      }

      if (any(!steekproeven$car %in% c("H", "M", "L"))) {
        stop("Planningsfout: 'car' mag alleen 'H', 'M' of 'L' bevatten.")
      }

      if (!is.numeric(steekproeven$materialiteit) ||
          any(steekproeven$materialiteit <= 0 |
              steekproeven$materialiteit >= 1)) {
        stop("Planningsfout: 'materialiteit' per stratum moet numeriek, > 0 en < 1 zijn.")
      }

      if (!is.numeric(steekproeven$fout_hoog) ||
          any(steekproeven$fout_hoog < 0)) {
        stop("Planningsfout: 'fout_hoog' moet numeriek en >= 0 zijn.")
      }

      if (!is.numeric(steekproeven$goed_hoog) ||
          any(steekproeven$goed_hoog < 0)) {
        stop("Planningsfout: 'goed_hoog' moet numeriek en >= 0 zijn.")
      }

      if (!is.numeric(steekproeven$n_hoog) ||
          any(steekproeven$n_hoog < 0)) {
        stop("Planningsfout: 'n_hoog' moet numeriek en >= 0 zijn.")
      }
    }


    # Vooraf validatie van onmogelijke situaties.
    {
      totale_populatie <- sum(steekproeven$waarde_laag + steekproeven$fout_hoog + steekproeven$goed_hoog)

      if (totale_populatie <= 0)
        stop("Planningsfout: De totale populatiewaarde is 0 of negatief.")

      bekende_foutfractie <- sum(steekproeven$fout_hoog) / totale_populatie
      if (bekende_foutfractie >= totale_materialiteit) {
        stop(
          sprintf(
            "Planningsfout: De reeds bekende fout in de hoogstrata (%.4f) is al groter dan of gelijk aan de totale materialiteit (%.4f).",
            bekende_foutfractie,
            totale_materialiteit
          )
        )
      }

      # Voor nu verbieden we dat in enig strata
      # de verwachte fout >= de stratum-materialiteit
      # Toestaan vereist dat we ergens teruggeven in welke strata dit voorkomt.
      # Die complicatie laten we nu even weg.
      # In de toekomst gaan we dit wel toestaan.
      # Je kunt je namelijk voorstellen dat je toch wilt weten als gebruiker
      # of het mogelijk is om onder de totale materialiteit te blijven, en voor lief
      # neemt dat sommige steekproeven qua maximale foutfractie boven de materialiteit
      # uitkomen.
      if (any(steekproeven$verwachte_foutfractie >= steekproeven$materialiteit)) {
        foute_strata <- steekproeven$naam[steekproeven$verwachte_foutfractie >= steekproeven$materialiteit]
        stop(
          paste(
            "Planningsfout: In de volgende strata is de verwachte foutfractie groter dan of gelijk aan de stratum-materialiteit:",
            paste(foute_strata, collapse = ", ")
          )
        )
      }

      totale_verwachte_fout_geld <- sum(steekproeven$waarde_laag * steekproeven$verwachte_foutfractie) + sum(steekproeven$fout_hoog)
      algehele_verwachte_foutfractie <- totale_verwachte_fout_geld / totale_populatie

      if (algehele_verwachte_foutfractie >= totale_materialiteit) {
        stop(
          sprintf(
            "Planningsfout: De algehele verwachte foutfractie (%.4f) is al groter dan of gelijk aan de totale materialiteit (%.4f). Meer posten trekken zal dit niet oplossen.",
            algehele_verwachte_foutfractie,
            totale_materialiteit
          )
        )
      }
    }
  }

  # Eerste hulpfunctie voor hybride optimalisatie.
  # De klimfunctie.
  calc_max_error_klim <- function(s_data) {
    res <- eval_stratified(
      steekproeven = s_data,
      model = model,
      zekerheid = totale_zekerheid,
      methode = klim_methode,
      granulariteit = klim_granulariteit,
      vergelijk = FALSE
    )
    return(res$max_fout_convolutie)
  }

  # Tweede hulpfunctie voor hybride optimalisatie.
  # De definitieve toets.
  calc_max_error_validatie <- function(s_data) {
    res <- eval_stratified(
      steekproeven = s_data,
      model = model,
      zekerheid = totale_zekerheid,
      methode = validatie_methode,
      granulariteit = validatie_granulariteit,
      start = start,
      vergelijk = FALSE
    )
    return(res$max_fout_convolutie)
  }

  # Derde hulpfunctie voor hybride optimalsatie.
  # De zoekmachine:
  # Vindt het beste stratum om op te hogen, varend op de klimfunctie.
  vind_beste_stratum <- function(huidige_strata) {
    huidige_fout_klim <- calc_max_error_klim(huidige_strata)
    beste_stratum <- NA
    beste_verbetering <- -Inf

    for (i in 1:nrow(huidige_strata)) {
      # We maken een kopie van strata: test_strata.
      test_strata <- huidige_strata

      # Voor die kopie verhogen we de i-de n_laag met 1.
      test_strata$n_laag[i] <- test_strata$n_laag[i] + 1

      # We passen de i-de k_laag navenant aan.
      test_strata$k_laag[i] <- test_strata$n_laag[i] * test_strata$verwachte_foutfractie[i]

      # We passen de i-de n_totaal navenant aan.
      test_strata$n_totaal[i] <- test_strata$n_laag[i] + test_strata$n_hoog[i]

      # We berekenen nu over het gehele zo gemaakte test_strata wat de maximale fout zou worden.
      test_max_fout <- calc_max_error_klim(test_strata)

      # We kijken wat dit oplevert aan verbetering: hoeveel wordt de huidige max fout kleiner.
      verbetering <- huidige_fout_klim - test_max_fout

      # We kijken wat dit oplevert aan verbetering: hoeveel wordt de huidige max fout kleiner.
      is_ex_aequo <- abs(verbetering - beste_verbetering) < 1e-9

      # We noteren dit stratum als de strikt beste verbetering heeft.
      # ALS het een ex aequo is (verbetering is praktisch gelijk), dan checken we
      # welk stratum tot nu toe het minst is opgehoogd t.o.v. de basisplanning.
      if (verbetering > (beste_verbetering + 1e-9)) {
        beste_verbetering <- verbetering
        beste_stratum <- i
      } else if (is_ex_aequo) {
        # Bereken voor zowel de huidige koploper als dit nieuwe (test) stratum
        # hoeveel ze al zijn opgehoogd ten opzichte van hun n_basis.
        ophoging_beste <- huidige_strata$n_laag[beste_stratum] - huidige_strata$n_basis[beste_stratum]
        ophoging_huidig <- test_strata$n_laag[i] - huidige_strata$n_basis[i]

        # Als het huidige stratum minder vaak is opgehoogd, neemt deze de koppositie over.
        if (ophoging_huidig < ophoging_beste) {
          beste_stratum <- i
        }
      }
    }
    return(beste_stratum)
  }

  # Basisplanning (de minimale n per stratum).
  # Bereken deze met behulp van drawsneeded().
  {
    # Vertaal de Nederlandse modelnaam naar de Engelse voor drawsneeded.
    dist_eng <- if (model == "binomiaal")
      "binomial"
    else
      "Poisson"

    strata <- steekproeven |>
      mutate(
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
  }

  # Hybride maximale marginale verbeteringslus,
  # of ook wel, steilste-helling optimalisatie:
  # we verhogen steeds n_laag met 1,
  # van dat stratum waarvoor we het beste resultaat krijgen.
  # Zeg maar: we nemen steeds het lekkerste snoepje uit de schaal; dat werkt
  # niet altijd goed in het echte leven, maar hier wel.
  {
    iteratie <- 0

    # Fase 1: Iteratie met klim_functie tot we onder de grens zitten.
    huidige_fout_klim <- calc_max_error_klim(strata)
    while (huidige_fout_klim > totale_materialiteit &&
           iteratie < max_iteraties) {
      iteratie <- iteratie + 1

      beste_stratum <- vind_beste_stratum(strata)

      strata$n_laag[beste_stratum] <- strata$n_laag[beste_stratum] + 1
      strata$k_laag[beste_stratum] <- strata$n_laag[beste_stratum] * strata$verwachte_foutfractie[beste_stratum]
      strata$n_totaal[beste_stratum] <- strata$n_laag[beste_stratum] + strata$n_hoog[beste_stratum]

      huidige_fout_klim <- calc_max_error_klim(strata)
    }

    # Fase 2: Valideer met de validatie methode (bijv. MC met 1.000.000).
    # Liggen we er toch nog net boven? Neem dan nog een paar extra stappen (weer via het FFT kompas).
    huidige_fout_validatie <- calc_max_error_validatie(strata)
    while (huidige_fout_validatie > totale_materialiteit &&
           iteratie < max_iteraties) {
      iteratie <- iteratie + 1

      beste_stratum <- vind_beste_stratum(strata)

      strata$n_laag[beste_stratum] <- strata$n_laag[beste_stratum] + 1
      strata$k_laag[beste_stratum] <- strata$n_laag[beste_stratum] * strata$verwachte_foutfractie[beste_stratum]
      strata$n_totaal[beste_stratum] <- strata$n_laag[beste_stratum] + strata$n_hoog[beste_stratum]

      huidige_fout_validatie <- calc_max_error_validatie(strata)
    }

    if (iteratie >= max_iteraties) {
      warning(
        paste0(
          "Maximale aantal iteraties = maximaal aantal extra steken, ",
          max_iteraties,
          ", bereikt."
        )
      )
    }
  }

  # Afronding en output.
  {
    # Verwijder cert uit strata.
    strata$cert <- NULL

    # Hernoem in strata n_laag naar n_definitief
    strata <- dplyr::rename(strata, n_definitief = .data$n_laag)

    attr(strata, "geplande_max_fout_totaal") <- huidige_fout_validatie

    return(strata)
  }
}
