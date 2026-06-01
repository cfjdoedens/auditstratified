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
#'   En zijn opvolgend meer
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
#'   Een startwaarde of 0 betekent dat de startwaarde op de systeemklok,
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
                            validatie_methode = c("FFT", "FFT gelijktijdig", "direct", "Monte Carlo"),
                            validatie_granulariteit = NULL,
                            start = 1,
                            max_iteraties = 1e4) {
  # Valideer en bepaal de argumentkeuzes.
  {
    model <- match.arg(model)
    klim_methode <- match.arg(klim_methode)
    validatie_methode <- match.arg(validatie_methode)
  }

  # Bepaal dynamische verstekwaarden voor de granulariteit.
  {
    klim_granulariteit <-
      klim_granulariteit %||% ifelse(klim_methode == "Monte Carlo", 1e6, 1e4)
    validatie_granulariteit <-
      validatie_granulariteit %||% ifelse(klim_methode == "Monte Carlo", 1e7, 25e3)
  }

  # Valideer de basistypes en waarden van de algemene parameters.
  {
    stopifnot(is_tibble(steekproeven))
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
  }

  # Controleer op de aanwezigheid van alle verplichte kolommen in de invoertibble.
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
  }

  # Controleer of de namen van de strata uniek zijn om verwarring te voorkomen.
  {
    if (any(duplicated(steekproeven$naam))) {
      dubbele <- unique(steekproeven$naam[duplicated(steekproeven$naam)])
      stop(
        paste(
          "Planningsfout: De volgende stratumnamen komen vaker dan \u00e9\u00e9n keer voor:",
          paste(dubbele, collapse = ", ")
        )
      )
    }
  }

  # Controleer de datatypes en toegestane domeinen per afzonderlijke kolom.
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

  # Controleer op mathematisch onmogelijke uitgangssituaties in de data.
  {
    totale_populatie <- sum(steekproeven$waarde_laag + steekproeven$fout_hoog + steekproeven$goed_hoog)

    if (totale_populatie <= 0) {
      stop("Planningsfout: De totale populatiewaarde is 0 of negatief.")
    }

    text_bekende_foutfractie <- sum(steekproeven$fout_hoog) / totale_populatie
    if (text_bekende_foutfractie >= totale_materialiteit) {
      stop(
        sprintf(
          "Planningsfout: De reeds bekende fout in de hoogstrata (%.4f) is al groter dan of gelijk aan de totale materialiteit (%.4f).",
          text_bekende_foutfractie,
          totale_materialiteit
        )
      )
    }

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

  # Definieer de interne aanroep naar de snellere klim-evaluatie.
  {
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
  }

  # Definieer de interne aanroep naar de definitieve validatie-toets.
  {
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
  }

  # Definieer de interne zoekmachine die het beste stratum selecteert om op te hogen.
  {
    vind_beste_stratum <- function(huidige_strata) {
      huidige_fout_klim <- calc_max_error_klim(huidige_strata)
      beste_stratum <- NA
      beste_verbetering <- -Inf
      beste_proxy_verbetering <- -Inf
      z_val <- qnorm(totale_zekerheid)
      tot_geld <- sum(huidige_strata$waarde_laag + huidige_strata$fout_hoog + huidige_strata$goed_hoog)
      fout_hi <- sum(huidige_strata$fout_hoog)
      # Definieer een continue proxy-functie op basis van de totale weging van de s-strata.
      calc_proxy <- function(data_strata) {
        m_sum <- 0
        v_sum <- 0
        for (j in 1:nrow(data_strata)) {
          w <- data_strata$waarde_laag[j]
          n <- data_strata$n_laag[j]
          ef <- data_strata$verwachte_foutfractie[j]
          k <- n * ef
          if (model == "binomiaal") {
            a <- 1 + k
            b_p <- 1 + n - k
            m_j <- a / (a + b_p)
            v_j <- (a * b_p) / ((a + b_p)^2 * (a + b_p + 1))
          } else {
            shape <- 1 + k
            rate <- n
            m_j <- shape / rate
            v_j <- shape / (rate^2)
          }
          m_sum <- m_sum + w * m_j
          v_sum <- v_sum + (w^2) * v_j
        }
        return(((m_sum + fout_hi) / tot_geld) + z_val * sqrt(v_sum / (tot_geld^2)))
      }
      huidige_proxy <- calc_proxy(huidige_strata)
      for (i in 1:nrow(huidige_strata)) {
        test_strata <- huidige_strata
        test_strata$n_laag[i] <- test_strata$n_laag[i] + 1
        test_strata$k_laag[i] <- test_strata$n_laag[i] * test_strata$verwachte_foutfractie[i]
        test_strata$n_totaal[i] <- test_strata$n_laag[i] + test_strata$n_hoog[i]
        test_max_fout <- calc_max_error_klim(test_strata)
        verbetering <- huidige_fout_klim - test_max_fout
        proxy_verbetering <- huidige_proxy - calc_proxy(test_strata)
        is_ex_aequo <- abs(verbetering - beste_verbetering) < 1e-9
        # Evalueer de verbetering en breek een ex aequo met de oneindig nauwkeurige analytische proxy.
        if (verbetering > (beste_verbetering + 1e-9)) {
          beste_verbetering <- verbetering
          beste_proxy_verbetering <- proxy_verbetering
          beste_stratum <- i
        } else if (is_ex_aequo) {
          if (proxy_verbetering > beste_proxy_verbetering) {
            beste_proxy_verbetering <- proxy_verbetering
            beste_stratum <- i
          }
        }
      }
      return(beste_stratum)
    }
  }

  # Bereken de minimale basisplanning per stratum met drawsneeded.
  {
    dist_eng <- if (model == "binomiaal") {
      "binomial"
    } else {
      "Poisson"
    }

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

  # Voer de optimalisatielussen uit tot de totale materialiteit is bereikt.
  {
    iteratie <- 0

    # Verhoog de steekproefomvang stapsgewijs op basis van de snelle klimmethode.
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

    # Controleer en corrigeer de uitkomst aan de hand van de zwaardere validatiemethode.
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

  # Rond de tabel netjes af en retourneer het resultaat met het fout-attribuut.
  {
    strata$cert <- NULL
    strata <- dplyr::rename(strata, n_definitief = .data$n_laag)
    attr(strata, "geplande_max_fout_totaal") <- huidige_fout_validatie
    return(strata)
  }
}
