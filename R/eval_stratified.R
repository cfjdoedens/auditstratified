#' @title
#'   Evalueer samen de resultaten van 1 of meer steekproeven op uitgaand geld
#'
#' @description
#'   Het samennemen van de resultaten gebeurt door convolutie van
#'   de foutkanskrommes van de afzonderlijke steekproeven tot
#'   1 foutkanskromme.
#'
#'   Waar van toepassing worden 100%-getoetste posten (het hoogstratum of
#'   topstratum) als deterministische factoren opgeteld bij de resulterende
#'   statistische verdeling.
#'
#'   We berekenen de meest waarschijnlijke en de minimale en
#'   maximale fout als fractie en in geld.
#'
#'   De meest waarschijnlijke fout is de modus van de kanskromme.
#'   De minimale en maximale fout zijn afhankelijk van de gevraagde zekerheid.
#'   De minimale fout is de fout bij een cumulatieve kans gelijk
#'   aan 1 - deze zekerheid.
#'   De maximale fout is de fout bij een cumulatieve kans gelijk
#'   aan deze zekerheid.
#'
#'   De statistische interpretatie van de risico waarden
#'   hoog, midden en laag voor IHR, IBR en CAR, die deze module hanteert is
#'   volgens het HARO, het Handboek Auditing Rijksoverheid.
#'   Het HARo wordt beheerd door Auditdienst Rijk, de ADR.
#'
#' @details
#'   We gaan uit van de som van de foutfracties, de k-waarde, dus we kijken niet
#'   naar de foutfracties per post.
#'
#'   De minimale en maximale fout worden bepaald aan de hand van de
#'   resulterende kanskromme,
#'   op basis van de gewenste zekerheid. Visueel zijn de
#'   minimale fout, pmin, en de maximale fout, pmax, te
#'   bepalen in een tweedimensionaal, haaks, assenstelsel.
#'   De horizontale as, de p-as, loopt van 0 tot 1.
#'   De waarden langs die as geven de mogeljke foutfracties weer,
#'   lopend van 0 (geen fouten) tot 1 (alles fout).
#'   De verticale as, de c-as, loopt van 0 to oneindig.
#'   Deze as geeft de kanswaarden van de foutfracties aan.
#'   In dit assenstelsel kunnen we de kanskromme afbeelden.
#'   Het oppervlak onder de kanskromme is 1.
#'   Hierbij praten we over het oppervlak begrenst door de p-as, aan de
#'   onderkant, en de verticale lijnen p = 0, en p = 1.
#'   pmin is het punt op de p-as waarbij de verticale lijn p = pmin,
#'   het oppervlak onder de kanskromme begrenst zodat _rechts_ van deze lijn
#'   het oppervlak gelijk is aan de zekerheid, bijvoorbeeld 0,95.
#'   pmax is het punt op de p-as waarbij de verticale lijn p = pmax,
#'   het oppervlak onder de kanskromme begrenst zodat _links_ van deze lijn
#'   het oppervlak gelijk is aan de zekerheid, bijvoorbeeld 0,95.
#'
#'   Aggregatie is puur op statistische
#'   gronden: namelijk risico's op fouten boven de meest waarschijnlijke fout
#'   en op onder de meest waarschijnlijke fout vlakken elkaar enigszins uit
#'   genomen over de meerdere steekproeven.
#'   Dus, bij het aggregeren van de resultaten van de verschillende steekproeven
#'   wordt geen enkele aanname gedaan over gelijkenis tussen
#'   de eigenschappen van de afzonderlijke administraties waaruit is
#'   getrokken.
#'
#' @param steekproeven Een tibble met de steekproefgegevens. Deze
#'   bestaat uit de volgende kolommen:
#'   - naam
#'   - waarde_laag
#'   - n_laag
#'   - k_laag
#'   - ihr
#'   - ibr
#'   - car
#'   - materialiteit
#'   - fout_hoog
#'   - goed_hoog
#'   - n_hoog
#'   - n_totaal
#'   - waarde_hoog
#'   - waarde_populatie
#' @param model Het statistische model dat gebruikt wordt.
#'   Keuze uit \code{"binomiaal"} (standaard) of \code{"poisson"}.
#' @param zekerheid Het zekerheidsniveau waarop we de
#'   maximale foutfractie berekenen.
#' @param methode Methode voor de berekening.
#'   Keuze uit:
#'   - \code{"direct"}
#'   - \code{"FFT paarsgewijs"}
#'   - \code{"FFT samen"} (standaard)
#'   - \code{"Monte Carlo"}.
#'
#'   \code{"direct"},
#'   \code{"FFT paarsgewijs"} en
#'   \code{"FFT samen"} zijn deterministische algoritmen.
#'   en zijn opvolgend meer
#'   efficiente vormen van hetzelfde convolutie-algoritme.
#'   \code{"Monte Carlo"} is niet-deterministisch, dus gebaseerd op toeval.
#'   Dat betekent dat het resultaat ervan afhangt van de startwaarde van de
#'   R toevalsgenerator, die je kunt opgeven via de parameter \code{start}.
#' @param granulariteit Bepaalt de nauwkeurigheid van de berekening.
#'   Bij \code{"direct"}, \code{"FFT paarsgewijs"} en \code{"FFT samen"},
#'   is dit het aantal stappen op de
#'   kanskromme-as.
#'   Als verstekwaarden geldt voor \code{"direct"}, \code{"FFT paarsgewijs"}
#'   en \code{"FFT samen"} 25.000,
#'   en voor \code{"Monte Carlo"} 10.000.000.
#' @param start De vaste startwaarde voor de toevalsgenerator
#'   (alleen voor MonteCarlo).
#'   Een startwaarde van 0 betekent dat de startwaarde op de systeemklok,
#'   is gebaseerd, dus min of meer 'echt' op toeval is gebaseerd.
#' @param vergelijk TRUE of FALSE, als TRUE dan worden wat vergelijkende
#'   berekeningen uitgevoerd.
#' @returns
#'   Een lijst, bestaande uit de convolutie-uitkomsten (fracties en geld),
#'   eventuele vergelijkingen, en de verrijkte invoergegevens.
#'
#' @export
#' @importFrom dplyr pull mutate
#' @importFrom tibble is_tibble tribble add_row
eval_stratified <-
  function(steekproeven,
           model = c("binomiaal", "poisson"),
           zekerheid = 0.95,
           methode = c("FFT paarsgewijs", "FFT samen", "direct", "Monte Carlo"),
           granulariteit = NULL, # Verstekwaarde bepalen we in functie zelf.
           start = 1,
           vergelijk = TRUE) {
    # Bepaal en valideer de argumentkeuzes.
    model <- match.arg(model)
    methode <- match.arg(methode)

    # Bepaal dynamische verstekwaarde granulariteit.
    granulariteit <-
      granulariteit %||% ifelse(methode == "Monte Carlo", 1e7, 25e3)

    # Controleer de invoer.
    {
      stopifnot(is_tibble(steekproeven))

      # Strikte controle op alle vereiste kolommen.
      stopifnot("naam" %in% colnames(steekproeven))
      stopifnot("waarde_laag" %in% colnames(steekproeven))
      stopifnot("n_laag" %in% colnames(steekproeven))
      stopifnot("k_laag" %in% colnames(steekproeven))
      stopifnot("ihr" %in% colnames(steekproeven))
      stopifnot("ibr" %in% colnames(steekproeven))
      stopifnot("car" %in% colnames(steekproeven))
      stopifnot("materialiteit" %in% colnames(steekproeven))
      stopifnot("fout_hoog" %in% colnames(steekproeven))
      stopifnot("goed_hoog" %in% colnames(steekproeven))
      stopifnot("n_hoog" %in% colnames(steekproeven))
      stopifnot("n_totaal" %in% colnames(steekproeven))
      stopifnot("waarde_hoog" %in% colnames(steekproeven))
      stopifnot("waarde_populatie" %in% colnames(steekproeven))

      # Inlezen van de variabelen.
      naam <- steekproeven |> pull("naam")
      waarde_laag <- steekproeven |> pull("waarde_laag")
      n_laag <- steekproeven |> pull("n_laag")
      k_laag <- steekproeven |> pull("k_laag")
      ihr <- steekproeven |> pull("ihr")
      ibr <- steekproeven |> pull("ibr")
      car <- steekproeven |> pull("car")
      materialiteit <- steekproeven |> pull("materialiteit")
      fout_hoog <- steekproeven |> pull("fout_hoog")
      goed_hoog <- steekproeven |> pull("goed_hoog")
      n_hoog <- steekproeven |> pull("n_hoog")
      n_totaal <- steekproeven |> pull("n_totaal")
      waarde_hoog <- steekproeven |> pull("waarde_hoog")
      waarde_populatie <- steekproeven |> pull("waarde_populatie")

      # Basiscontroles.
      {
        len_naam <- length(naam)
        stopifnot(len_naam > 0)
        stopifnot(length(waarde_laag) == len_naam)
        stopifnot(length(n_laag) == len_naam)
        stopifnot(length(k_laag) == len_naam)

        stopifnot(is.numeric(waarde_laag))
        stopifnot(0 <= waarde_laag)
        stopifnot(0 <= n_laag)
        stopifnot(0 <= k_laag)
        stopifnot(k_laag <= n_laag)

        stopifnot(length(zekerheid) == 1)
        stopifnot(is.numeric(zekerheid))
        stopifnot(0 <= zekerheid && zekerheid <= 1)

        stopifnot(length(granulariteit) == 1)
        stopifnot(is.numeric(granulariteit))
        stopifnot(granulariteit >= 1)
      }

      # Invoercontrole aan de hand van redundantie in parameters.
      if (any(n_totaal != (n_laag + n_hoog)))
        stop("Inconsistentie: 'n_totaal' is onjuist.")
      if (any(abs(waarde_hoog - (fout_hoog + goed_hoog)) > 0.01))
        stop("Inconsistentie: 'waarde_hoog' is onjuist.")
      if (any(abs(waarde_populatie - (waarde_laag + waarde_hoog)) > 0.01))
        stop("Inconsistentie: 'waarde_populatie' is onjuist.")

      # Controle op dubbele stratumnamen.
      if (any(duplicated(steekproeven$naam))) {
        dubbele <- unique(steekproeven$naam[duplicated(steekproeven$naam)])
        stop(
          paste(
            "Evaluatiefout: De volgende stratumnamen komen vaker dan \u00e9\u00e9n keer voor:",
            paste(dubbele, collapse = ", ")
          )
        )
      }
      }

    # Bepaal totaal geldswaarde, inclusief het 100%-getoetste deel.
    totaalgeld_laag <- sum(waarde_laag)
    totaalgeld_fout_hoog <- sum(fout_hoog)
    totaalgeld_goed_hoog <- sum(goed_hoog)
    totaalgeld_algeheel <- totaalgeld_laag + totaalgeld_fout_hoog + totaalgeld_goed_hoog

    # Creeer uitvoertibble, t_uit, met regels per steekproef.
    {
      t_uit <-
        tribble(
          ~ naam,
          ~ waarde_laag,
          ~ n_laag,
          ~ k_laag,
          ~ ihr,
          ~ ibr,
          ~ car,
          ~ materialiteit,
          ~ fout_hoog,
          ~ goed_hoog,
          ~ n_hoog,
          ~ n_totaal,
          ~ waarde_hoog,
          ~ waarde_populatie,
          ~ extra_foutloze_posten,
          ~ toch_fouten,
          ~ mw_fout,
          ~ min_fout,
          ~ max_fout
        )

      n_steekproeven <- nrow(steekproeven)
      for (i in 1:n_steekproeven) {
        t_uit <- add_row(
          t_uit,
          naam = steekproeven$naam[[i]],
          waarde_laag = steekproeven$waarde_laag[[i]],
          n_laag = steekproeven$n_laag[[i]],
          k_laag = steekproeven$k_laag[[i]],
          ihr = steekproeven$ihr[[i]],
          ibr = steekproeven$ibr[[i]],
          car = steekproeven$car[[i]],
          materialiteit = steekproeven$materialiteit[[i]],
          fout_hoog = steekproeven$fout_hoog[[i]],
          goed_hoog = steekproeven$goed_hoog[[i]],
          n_hoog = steekproeven$n_hoog[[i]],
          n_totaal = steekproeven$n_totaal[[i]],
          waarde_hoog = steekproeven$waarde_hoog[[i]],
          waarde_populatie = steekproeven$waarde_populatie[[i]],
          extra_foutloze_posten = foutloze_posten_equivalent(
            steekproeven$ihr[[i]],
            steekproeven$ibr[[i]],
            steekproeven$car[[i]],
            steekproeven$materialiteit[[i]]
          ),
          toch_fouten = (
            !(
              steekproeven$ihr[[i]] == "H" &&
                steekproeven$ibr[[i]] == "H" &&
                steekproeven$car[[i]] == "H"
            ) && steekproeven$k_laag[[i]] > 0
          ),
          mw_fout = NA,
          min_fout = NA,
          max_fout = NA
        )
      }
    }

    # Convolutie: FFT, FFT samen, direct, of MonteCarlo.
    {
      conv <- if (methode == "FFT paarsgewijs")
        convolutie_fft(
          t_uit,
          model,
          zekerheid,
          granulariteit,
          totaalgeld_laag,
          totaalgeld_fout_hoog,
          totaalgeld_algeheel
        )
      else if (methode == "FFT samen")
        convolutie_fft_gelijktijdig(
          t_uit,
          model,
          zekerheid,
          granulariteit,
          totaalgeld_laag,
          totaalgeld_fout_hoog,
          totaalgeld_algeheel
        )
      else if (methode == "direct")
        convolutie_direct(
          t_uit,
          model,
          zekerheid,
          granulariteit,
          totaalgeld_laag,
          totaalgeld_fout_hoog,
          totaalgeld_algeheel
        )
      else
        convolutie_montecarlo(
          t_uit,
          model,
          zekerheid,
          granulariteit,
          start,
          totaalgeld_laag,
          totaalgeld_fout_hoog,
          totaalgeld_algeheel
        )

      d <- conv$d
      min_fout_convolutie <- conv$min_fout
      max_fout_convolutie <- conv$max_fout
      mediaan_fout_convolutie <- conv$mediaan_fout
      modus_fout_convolutie <- conv$modus_fout
      gemiddelde_fout_convolutie <- conv$gemiddelde_fout
    }

    # Ter vergelijking: los en als1.
    {
      mw_fout_los <- NA
      min_fout_los <- NA
      max_fout_los <- NA
      mw_fout_als1 <- NA
      min_fout_als1 <- NA
      max_fout_als1 <- NA

      if (vergelijk) {
        verg <- vergelijk_los_en_als1(
          t_uit,
          model,
          zekerheid,
          totaalgeld_laag,
          totaalgeld_fout_hoog,
          totaalgeld_algeheel
        )
        t_uit <- verg$t_uit
        mw_fout_los <- verg$mw_fout_los
        min_fout_los <- verg$min_fout_los
        max_fout_los <- verg$max_fout_los
        mw_fout_als1 <- verg$mw_fout_als1
        min_fout_als1 <- verg$min_fout_als1
        max_fout_als1 <- verg$max_fout_als1
      }
    }

    # Resultaat opstellen.
    {
      invoer <- list(
        steekproeven = steekproeven,
        model = model,
        zekerheid = zekerheid,
        methode = methode,
        granulariteit = granulariteit,
        start = start,
        vergelijk = vergelijk
      )

      list(
        kanskromme = d,
        populatie_totaal = totaalgeld_algeheel,
        modus_fout_convolutie = modus_fout_convolutie,
        modus_fout_convolutie_geld = modus_fout_convolutie * totaalgeld_algeheel,
        mediaan_fout_convolutie = mediaan_fout_convolutie,
        mediaan_fout_convolutie_geld = mediaan_fout_convolutie * totaalgeld_algeheel,
        gemiddelde_fout_convolutie = gemiddelde_fout_convolutie,
        gemiddelde_fout_convolutie_geld = gemiddelde_fout_convolutie * totaalgeld_algeheel,
        mw_fout_convolutie = modus_fout_convolutie,
        mw_fout_convolutie_geld = modus_fout_convolutie * totaalgeld_algeheel,
        min_fout_convolutie = min_fout_convolutie,
        min_fout_convolutie_geld = min_fout_convolutie * totaalgeld_algeheel,
        max_fout_convolutie = max_fout_convolutie,
        max_fout_convolutie_geld = max_fout_convolutie * totaalgeld_algeheel,
        vergelijk_met = list(
          mw_fout_los = mw_fout_los,
          mw_fout_los_geld = mw_fout_los * totaalgeld_algeheel,
          min_fout_los = min_fout_los,
          min_fout_los_geld = min_fout_los * totaalgeld_algeheel,
          max_fout_los = max_fout_los,
          max_fout_los_geld = max_fout_los * totaalgeld_algeheel,
          mw_fout_als1 = mw_fout_als1,
          mw_fout_als1_geld = mw_fout_als1 * totaalgeld_algeheel,
          min_fout_als1 = min_fout_als1,
          min_fout_als1_geld = min_fout_als1 * totaalgeld_algeheel,
          max_fout_als1 = max_fout_als1,
          max_fout_als1_geld = max_fout_als1 * totaalgeld_algeheel
        ),
        steekproeven = t_uit,
        invoer = invoer
      )
    }
  }


#' @title
#' Convolutie via Monte Carlo simulatie.
#'
#' @param t_uit Verrijkte steekproef-tibble met n_laag, k_laag, extra_foutloze_posten, waarde_laag.
#' @param model "binomiaal" of "poisson".
#' @param zekerheid Zekerheidsniveau (0-1).
#' @param granulariteit Aantal toevalsiteraties.
#' @param start Startwaarde voor de toevalsgenerator (alleen voor MonteCarlo).
#'   Een startwaarde van 0 betekent dat de startwaarde op de systeemklok,
#'   is gebaseerd, dus min of meer 'echt' op toeval is gebaseerd.
#' @param totaalgeld_laag Totale geldswaarde van het laagstratum.
#' @param totaalgeld_fout_hoog Totale fout in het hoogstratum.
#' @param totaalgeld_algeheel Totale geldswaarde van de gehele populatie.
#' @returns Een lijst met d, min_fout, max_fout, mediaan_fout, modus_fout, gemiddelde_fout.
#'
#' @importFrom stats rbeta rgamma density quantile
convolutie_montecarlo <- function(t_uit,
                                  model,
                                  zekerheid,
                                  granulariteit,
                                  start,
                                  totaalgeld_laag,
                                  totaalgeld_fout_hoog,
                                  totaalgeld_algeheel) {
  if (start == 0) {
    set.seed(NULL)
  }
  n_steekproeven <- nrow(t_uit)

  # Simuleer foutfracties per stratum.
  {
    krommen <- matrix(NA, nrow = granulariteit, ncol = n_steekproeven)
    set.seed(start)

    for (i in 1:n_steekproeven) {
      n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
      k_calc <- t_uit$k_laag[[i]]

      if (model == "binomiaal") {
        krommen[, i] <- rbeta(granulariteit,

                              shape1 = 1 + k_calc,
                              shape2 = 1 + n_calc - k_calc)
      } else if (model == "poisson") {
        krommen[, i] <- rgamma(granulariteit,
                               shape = 1 + k_calc,
                               rate = n_calc)
      }
    }
  }

  # Convolutie via matrixvermenigvuldiging en kernel density.
  {
    convolutie <- (krommen %*% t_uit$waarde_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel

    bovengrens <- if (model == "binomiaal")
      1
    else
      max(convolutie) + 0.1
    d <- density(convolutie,
                 adjust = 1.5,
                 from = 0,
                 to = bovengrens)

    # Normaliseren.
    oppervlakte <- sum(d$y) * (d$x[2] - d$x[1])
    d$y <- d$y / oppervlakte
  }

  list(
    d = d,
    min_fout = unname(quantile(convolutie, probs = 1 - zekerheid)),
    max_fout = unname(quantile(convolutie, probs = zekerheid)),
    mediaan_fout = unname(quantile(convolutie, probs = 0.5)),
    modus_fout = d$x[which.max(d$y)],
    gemiddelde_fout = mean(convolutie)
  )
}


#' @title
#' Convolutie via paarsgewijze Fast Fourier Transformatie.
#'
#' @inheritParams convolutie_montecarlo
#' @returns Een lijst met d, min_fout, max_fout, mediaan_fout, modus_fout, gemiddelde_fout.
#'
#' @importFrom stats dbeta dgamma qgamma convolve
convolutie_fft <- function(t_uit,
                           model,
                           zekerheid,
                           granulariteit,
                           totaalgeld_laag,
                           totaalgeld_fout_hoog,
                           totaalgeld_algeheel) {
  n_steekproeven <- nrow(t_uit)

  # Edge case: geen statistische controle, alles integraal.
  if (totaalgeld_laag < 0.01) {
    frac <- totaalgeld_fout_hoog / totaalgeld_algeheel
    return(
      list(
        d = list(x = frac, y = 1),
        min_fout = frac,
        max_fout = frac,
        mediaan_fout = frac,
        modus_fout = frac,
        gemiddelde_fout = frac
      )
    )
  }

  # Bouw kansmassavectoren per stratum.
  {
    dx <- totaalgeld_laag / granulariteit
    p_strata <- list()

    for (i in 1:n_steekproeven) {
      w_laag <- t_uit$waarde_laag[[i]]
      n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
      k_calc <- t_uit$k_laag[[i]]

      if (w_laag > 0) {
        # Bepaal hoe ver de as moet doorlopen. Binomiaal is max w_laag.
        # Poisson kan daar theoretisch overheen gaan,
        # Dus nemen we een veilige grens via qgamma.
        max_frac <- if (model == "binomiaal")
          1
        else
          qgamma(0.9999, shape = k_calc + 1, rate = n_calc)
        x_grens <- max(w_laag, max_frac * w_laag)
        x_as <- seq(0, x_grens, by = dx)

        if (model == "binomiaal") {
          p <- dbeta(x_as / w_laag,
                     shape1 = k_calc + 1,
                     shape2 = n_calc - k_calc + 1)
        } else {
          p <- dgamma(x_as / w_laag, shape = k_calc + 1, rate = n_calc)
        }

        p[is.na(p) | is.infinite(p)] <- 0
        p <- p / sum(p)
        p_strata[[length(p_strata) + 1]] <- p
      }
    }
  }

  # Paarsgewijze convolutie en statistieken afleiden.
  {
    if (length(p_strata) > 0) {
      p_totaal <- p_strata[[1]]
      if (length(p_strata) > 1) {
        for (j in 2:length(p_strata)) {
          p_totaal <- convolve(p_totaal, rev(p_strata[[j]]), type = "open")
        }
      }

      p_totaal <- p_totaal / sum(p_totaal)

      # Bouw de totale assen op.
      x_totaal_laag <- seq(0, by = dx, length.out = length(p_totaal))
      x_totaal_geld <- x_totaal_laag + totaalgeld_fout_hoog
      x_totaal_fractie <- x_totaal_geld / totaalgeld_algeheel

      # Omzetten naar kansdichtheid t.o.v. de fractie-as.
      dx_fractie <- dx / totaalgeld_algeheel
      d <- list(x = x_totaal_fractie, y = p_totaal / dx_fractie)

      cum_p <- cumsum(p_totaal)

      # Bereken de minimale fout.
      idx_min <- which(cum_p >= 1 - zekerheid)[1]
      min_fout <- if (is.na(idx_min))
        x_totaal_fractie[1]
      else
        x_totaal_fractie[idx_min]

      # Bereken de maximale fout.
      idx_max <- which(cum_p >= zekerheid)[1]
      max_fout <- if (is.na(idx_max))
        x_totaal_fractie[length(x_totaal_fractie)]
      else
        x_totaal_fractie[idx_max]

      # Bereken de mediaan.
      idx_med <- which(cum_p >= 0.5)[1]
      mediaan_fout <- if (is.na(idx_med))
        x_totaal_fractie[length(x_totaal_fractie)]
      else
        x_totaal_fractie[idx_med]
    }
  }

  list(
    d = d,
    min_fout = min_fout,
    max_fout = max_fout,
    mediaan_fout = mediaan_fout,
    modus_fout = x_totaal_fractie[which.max(p_totaal)],
    gemiddelde_fout = sum(x_totaal_fractie * p_totaal)
  )
}

#' @title
#' Convolutie via gelijktijdige Fast Fourier Transform.
#'
#' @description
#' Transformeert alle strata tegelijk naar het frequentiedomein,
#' vermenigvuldigt ze, en transformeert in een keer terug.
#' Dit vermijdt de paarsgewijze iteratielus.
#'
#' @inheritParams convolutie_montecarlo
#' @returns Een lijst met d, min_fout, max_fout, mediaan_fout, modus_fout, gemiddelde_fout.
#'
#' @importFrom stats dbeta dgamma qgamma fft nextn
convolutie_fft_gelijktijdig <- function(t_uit,
                                        model,
                                        zekerheid,
                                        granulariteit,
                                        totaalgeld_laag,
                                        totaalgeld_fout_hoog,
                                        totaalgeld_algeheel) {
  n_steekproeven <- nrow(t_uit)

  if (totaalgeld_laag < 0.01) {
    frac <- totaalgeld_fout_hoog / totaalgeld_algeheel
    return(
      list(
        d = list(x = frac, y = 1),
        min_fout = frac,
        max_fout = frac,
        mediaan_fout = frac,
        modus_fout = frac,
        gemiddelde_fout = frac
      )
    )
  }

  # Bouw kansmassavectoren per stratum.
  {
    dx <- totaalgeld_laag / granulariteit
    p_strata <- list()

    for (i in 1:n_steekproeven) {
      w_laag <- t_uit$waarde_laag[[i]]
      n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
      k_calc <- t_uit$k_laag[[i]]

      if (w_laag > 0) {
        max_frac <- if (model == "binomiaal") {
          1
        } else {
          qgamma(0.9999, shape = k_calc + 1, rate = n_calc)
        }
        x_grens <- max(w_laag, max_frac * w_laag)
        x_as <- seq(0, x_grens, by = dx)

        if (model == "binomiaal") {
          p <- dbeta(x_as / w_laag,
                     shape1 = k_calc + 1,
                     shape2 = n_calc - k_calc + 1)
        } else {
          p <- dgamma(x_as / w_laag, shape = k_calc + 1, rate = n_calc)
        }

        p[is.na(p) | is.infinite(p)] <- 0
        p <- p / sum(p)
        p_strata[[length(p_strata) + 1]] <- p
      }
    }
  }

  # Gelijktijdige convolutie via zero-padding en fft.
  {
    if (length(p_strata) > 0) {
      if (length(p_strata) == 1) {
        p_totaal <- p_strata[[1]]
      } else {

        # Bepaal de benodigde rekentotaallengte en de geoptimaliseerde pad-lengte.
        totale_lengte <- sum(sapply(p_strata, length)) - length(p_strata) + 1
        pad_lengte <- nextn(totale_lengte)

        # Transformeer naar frequentiedomein en vermenigvuldig alle vectoren.
        fft_product <- rep(1 + 0i, pad_lengte)
        for (p in p_strata) {
          p_padded <- c(p, rep(0, pad_lengte - length(p)))
          fft_product <- fft_product * fft(p_padded)
        }

        # Transformeer terug naar tijdsdomein en snijd de padding af.
        p_totaal <- Re(fft(fft_product, inverse = TRUE)) / pad_lengte
        p_totaal <- p_totaal[1:totale_lengte]

        # Verwijder afrondingsruis en normaliseer de kansmassa.
        p_totaal[p_totaal < 0 & abs(p_totaal) < 1e-12] <- 0
        p_totaal <- p_totaal / sum(p_totaal)
      }

      # Bereken de totale assen en parameters.
      x_totaal_laag <- seq(0, by = dx, length.out = length(p_totaal))
      x_totaal_geld <- x_totaal_laag + totaalgeld_fout_hoog
      x_totaal_fractie <- x_totaal_geld / totaalgeld_algeheel

      dx_fractie <- dx / totaalgeld_algeheel
      d <- list(x = x_totaal_fractie, y = p_totaal / dx_fractie)

      cum_p <- cumsum(p_totaal)

      # Bepaal de kwantielen voor minimale, maximale en mediane fout.
      idx_min <- which(cum_p >= 1 - zekerheid)[1]
      min_fout <- if (is.na(idx_min)) {
        x_totaal_fractie[1]
      } else {
        x_totaal_fractie[idx_min]
      }

      idx_max <- which(cum_p >= zekerheid)[1]
      max_fout <- if (is.na(idx_max)) {
        x_totaal_fractie[length(x_totaal_fractie)]
      } else {
        x_totaal_fractie[idx_max]
      }

      idx_med <- which(cum_p >= 0.5)[1]
      mediaan_fout <- if (is.na(idx_med)) {
        x_totaal_fractie[length(x_totaal_fractie)]
      } else {
        x_totaal_fractie[idx_med]
      }
    }
  }

  list(
    d = d,
    min_fout = min_fout,
    max_fout = max_fout,
    mediaan_fout = mediaan_fout,
    modus_fout = x_totaal_fractie[which.max(p_totaal)],
    gemiddelde_fout = sum(x_totaal_fractie * p_totaal)
  )
}

#' @title
#' Convolutie via directe, lineaire vectorberekening.
#'
#' @description
#' Dit is het tragere, iteratieve alternatief voor de Fast Fourier Transform methode.
#'
#' @inheritParams convolutie_montecarlo
#' @returns Een lijst met d, min_fout, max_fout, mediaan_fout, modus_fout, gemiddelde_fout.
#'
#' @importFrom stats dbeta dgamma qgamma
convolutie_direct <- function(t_uit,
                              model,
                              zekerheid,
                              granulariteit,
                              totaalgeld_laag,
                              totaalgeld_fout_hoog,
                              totaalgeld_algeheel) {
  n_steekproeven <- nrow(t_uit)

  if (totaalgeld_laag < 0.01) {
    frac <- totaalgeld_fout_hoog / totaalgeld_algeheel
    return(
      list(
        d = list(x = frac, y = 1),
        min_fout = frac,
        max_fout = frac,
        mediaan_fout = frac,
        modus_fout = frac,
        gemiddelde_fout = frac
      )
    )
  }

  # Bouw kansmassavectoren per stratum.
  {
    dx <- totaalgeld_laag / granulariteit
    p_strata <- list()

    for (i in 1:n_steekproeven) {
      w_laag <- t_uit$waarde_laag[[i]]
      n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
      k_calc <- t_uit$k_laag[[i]]

      if (w_laag > 0) {
        max_frac <- if (model == "binomiaal") {
          1
        } else {
          qgamma(0.9999, shape = k_calc + 1, rate = n_calc)
        }
        x_grens <- max(w_laag, max_frac * w_laag)
        x_as <- seq(0, x_grens, by = dx)

        if (model == "binomiaal") {
          p <- dbeta(x_as / w_laag, shape1 = k_calc + 1, shape2 = n_calc - k_calc + 1)
        } else {
          p <- dgamma(x_as / w_laag, shape = k_calc + 1, rate = n_calc)
        }

        p[is.na(p) | is.infinite(p)] <- 0
        p <- p / sum(p)
        p_strata[[length(p_strata) + 1]] <- p
      }
    }
  }

  # Paarsgewijze convolutie via de directe iteratieve methode.
  {
    if (length(p_strata) > 0) {
      p_totaal <- p_strata[[1]]

      if (length(p_strata) > 1) {
        for (j in 2:length(p_strata)) {
          p_next <- p_strata[[j]]
          out <- numeric(length(p_totaal) + length(p_next) - 1)

          for (i in seq_along(p_totaal)) {
            out[i:(i + length(p_next) - 1)] <-
              out[i:(i + length(p_next) - 1)] + p_totaal[i] * p_next
          }

          p_totaal <- out

          # Afrondingsfouten netjes opruimen.
          p_totaal[p_totaal < 0 & abs(p_totaal) < 1e-12] <- 0
          p_totaal <- p_totaal / sum(p_totaal)
        }
      }

      p_totaal <- p_totaal / sum(p_totaal)

      x_totaal_laag <- seq(0, by = dx, length.out = length(p_totaal))
      x_totaal_geld <- x_totaal_laag + totaalgeld_fout_hoog
      x_totaal_fractie <- x_totaal_geld / totaalgeld_algeheel

      dx_fractie <- dx / totaalgeld_algeheel
      d <- list(x = x_totaal_fractie, y = p_totaal / dx_fractie)

      cum_p <- cumsum(p_totaal)

      # Bereken de minimale fout.
      idx_min <- which(cum_p >= 1 - zekerheid)[1]
      min_fout <- if (is.na(idx_min)) {
        x_totaal_fractie[1]
      } else {
        x_totaal_fractie[idx_min]
      }

      # Bereken de maximale fout.
      idx_max <- which(cum_p >= zekerheid)[1]
      max_fout <- if (is.na(idx_max)) {
        x_totaal_fractie[length(x_totaal_fractie)]
      } else {
        x_totaal_fractie[idx_max]
      }

      # Bereken de mediaan.
      idx_med <- which(cum_p >= 0.5)[1]
      mediaan_fout <- if (is.na(idx_med)) {
        x_totaal_fractie[length(x_totaal_fractie)]
      } else {
        x_totaal_fractie[idx_med]
      }
    }
  }

  list(
    d = d,
    min_fout = min_fout,
    max_fout = max_fout,
    mediaan_fout = mediaan_fout,
    modus_fout = x_totaal_fractie[which.max(p_totaal)],
    gemiddelde_fout = sum(x_totaal_fractie * p_totaal)
  )
}


#' @title
#' Vergelijkingsmethoden: los (per stratum) en als1 (alles samengevoegd).
#'
#' @details
#' Vult ook de kolommen mw_fout, min_fout en max_fout in t_uit per stratum.
#'
#' @param t_uit Verrijkte steekproef-tibble.
#' @param model "binomiaal" of "poisson".
#' @param zekerheid Zekerheidsniveau (0-1).
#' @param totaalgeld_laag Totale geldswaarde van het laagstratum.
#' @param totaalgeld_fout_hoog Totale fout in het hoogstratum.
#' @param totaalgeld_algeheel Totale geldswaarde van de gehele populatie.
#' @returns Een lijst met t_uit, mw_fout_los, min_fout_los, max_fout_los, mw_fout_als1, min_fout_als1, max_fout_als1.
#
#' @importFrom stats qbeta qgamma
vergelijk_los_en_als1 <- function(t_uit,
                                  model,
                                  zekerheid,
                                  totaalgeld_laag,
                                  totaalgeld_fout_hoog,
                                  totaalgeld_algeheel) {
  n_steekproeven <- nrow(t_uit)

  # Los: per stratum apart extrapoleren, dan gewogen optellen.
  {
    for (i in 1:n_steekproeven) {
      n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
      k_calc <- t_uit$k_laag[[i]]

      t_uit$mw_fout[[i]] <- k_calc / n_calc
      if (model == "binomiaal") {
        t_uit$min_fout[[i]] <- qbeta(1 - zekerheid, k_calc + 1, n_calc - k_calc + 1)
        t_uit$max_fout[[i]] <- qbeta(zekerheid, k_calc + 1, n_calc - k_calc + 1)
      } else if (model == "poisson") {
        t_uit$min_fout[[i]] <- qgamma(1 - zekerheid, shape = k_calc + 1, rate = n_calc)
        t_uit$max_fout[[i]] <- qgamma(zekerheid, shape = k_calc + 1, rate = n_calc)
      }
    }

    mw_fout_los_geld <- sum(t_uit$mw_fout * t_uit$waarde_laag) + totaalgeld_fout_hoog
    min_fout_los_geld <- sum(t_uit$min_fout * t_uit$waarde_laag) + totaalgeld_fout_hoog
    max_fout_los_geld <- sum(t_uit$max_fout * t_uit$waarde_laag) + totaalgeld_fout_hoog

    mw_fout_los <- mw_fout_los_geld / totaalgeld_algeheel
    min_fout_los <- min_fout_los_geld / totaalgeld_algeheel
    max_fout_los <- max_fout_los_geld / totaalgeld_algeheel
  }

  # Als1: alle strata samenvoegen alsof het 1 steekproef is.
  {
    n_calc_als1 <- sum(t_uit$n_laag) + sum(t_uit$extra_foutloze_posten)
    k_calc_als1 <- sum(t_uit$k_laag)

    mw_fout_als1_laag <- k_calc_als1 / n_calc_als1
    if (model == "binomiaal") {
      min_fout_als1_laag <- qbeta(1 - zekerheid, k_calc_als1 + 1, n_calc_als1 - k_calc_als1 + 1)
      max_fout_als1_laag <- qbeta(zekerheid, k_calc_als1 + 1, n_calc_als1 - k_calc_als1 + 1)
    } else if (model == "poisson") {
      min_fout_als1_laag <- qgamma(1 - zekerheid, shape = k_calc_als1 + 1, rate = n_calc_als1)
      max_fout_als1_laag <- qgamma(zekerheid, shape = k_calc_als1 + 1, rate = n_calc_als1)
    }

    mw_fout_als1 <- (mw_fout_als1_laag * totaalgeld_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel
    min_fout_als1 <- (min_fout_als1_laag * totaalgeld_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel
    max_fout_als1 <- (max_fout_als1_laag * totaalgeld_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel
  }

  list(
    t_uit = t_uit,
    mw_fout_los = mw_fout_los,
    min_fout_los = min_fout_los,
    max_fout_los = max_fout_los,
    mw_fout_als1 = mw_fout_als1,
    min_fout_als1 = min_fout_als1,
    max_fout_als1 = max_fout_als1
  )
}
