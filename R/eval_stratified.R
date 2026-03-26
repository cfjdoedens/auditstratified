#' @title
#' Evalueer samen de resultaten van 1 of meer steekproeven op uitgaand geld
#'
#' @description
#' Het samennemen van de resultaten gebeurt door convolutie van
#' de foutkanskrommes van de afzonderlijke steekproeven tot
#' 1 foutkanskromme.
#'
#' Waar van toepassing worden 100%-getoetste posten (het hoogstratum of
#' topstratum) als deterministische factoren opgeteld bij de resulterende
#' statistische verdeling.
#'
#' We berekenen de meest waarschijnlijke en de maximale fout als fractie
#' en in geld.
#'
#' De meest waarschijnlijke fout is de modus van de kanskromme.
#' De maximale fout is afhankelijk van de gevraagde zekerheid, en
#' is de fout bij een cumulatieve kans gelijk aan deze zekerheid.
#'
#' De statistische interpretatie van de risico waarden
#' hoog, midden en laag voor IHR, IBR en CAR, die deze module hanteert is volgens
#' het HARO, het Handboek Auditing Rijksoverheid.
#' Het HARo wordt beheerd door Auditdienst Rijk, de ADR.
#'
#' @details
#' We gaan uit van de som van de foutfracties, de k-waarde, dus we kijken niet
#' naar de foutfracties per post.
#'
#' De maximale fout wordt bepaald aan de hand van de resulterende kanskromme,
#' op basis van de gewenste zekerheid. Visueel is de maximale fout, pm, te bepalen in een
#' tweedimensionaal, haaks, assenstelsel.
#' De horizontale as, de p-as, loopt van 0 tot 1.
#' De waarden langs die as geven de mogeljke foutfracties weer,
#' lopend van 0 (geen fouten) tot 1 (alles fout).
#' De verticale as, de c-as, loopt van 0 to oneindig.
#' Deze as geeft de kanswaarden van de foutfracties aan.
#' In dit assenstelsel kunnen we de kanskromme afbeelden.
#' Het oppervlak onder de kanskromme is 1.
#' Hierbij praten we over het oppervlak begrenst door de p-as, aan de onderkant,
#' en de verticale lijnen p = 0, en p = 1.
#' pm is het punt op de p-as waarbij de verticale lijn p = pm,
#' het oppervlak onder de kanskromme begrenst zodat links van deze lijn
#' het oppervlak gelijk is aan de zekerheid, bijvoorbeeld 0,95.
#'
#' Aggregatie is puur op statistische
#' gronden: namelijk risico's op fouten boven de meest waarschijnlijke fout
#' en op onder de meest waarschijnlijke fout vlakken elkaar enigszins uit
#' genomen over de meerdere steekproeven.
#' Dus, bij het aggregeren van de resultaten van de verschillende steekproeven
#' wordt geen enkele aanname gedaan over gelijkenis tussen
#' de eigenschappen van de afzonderlijke administraties waaruit is
#' getrokken.
#'
#' @param steekproeven Een tibble met de steekproefgegevens.
#' @param model Het statistische model dat gebruikt wordt voor de extrapolatie.
#' Keuze uit \code{"binomiaal"} (standaard) of \code{"poisson"}.
#' @param zekerheid Het zekerheidsniveau waarop we de maximale foutfractie berekenen.
#' @param methode De rekenmethode voor de convolutie. Keuze uit \code{"FFT"} (standaard, numerieke convolutie via Fast Fourier Transform) of \code{"MonteCarlo"} (stochastische benadering).
#' @param granulariteit Aantal stappen om de kanskromme in te verdelen (indien methode = "FFT").
#' @param MC Het aantal Monte Carlo iteraties dat gebruikt wordt (indien methode = "MonteCarlo").
#' @param start Startwaarde voor de toevalsgenerator (alleen voor MonteCarlo).
#' @param vergelijk TRUE of FALSE, als TRUE dan worden wat vergelijkende berekeningen uitgevoerd.
#' @returns
#' Een lijst, bestaande uit de convolutie-uitkomsten (fracties en geld),
#' eventuele vergelijkingen, en de verrijkte invoergegevens.
#'
#' @export
#' @importFrom dplyr pull mutate
#' @importFrom stats density quantile rbeta qbeta rgamma qgamma dbeta dgamma convolve
#' @importFrom tibble add_row is_tibble tribble
eval_stratified <-
  function(steekproeven,
           model = c("binomiaal", "poisson"),
           zekerheid = 0.95,
           methode = c("FFT", "MonteCarlo"),
           granulariteit = 10000,
           MC = 1e7,
           start = 1,
           vergelijk = TRUE) {

    # Valideer en bepaal de argumentkeuzes
    model <- match.arg(model)
    methode <- match.arg(methode)

    # Controleer de invoer.
    {
      stopifnot(is_tibble(steekproeven))

      # Strikte controle op álle vereiste kolommen
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

      # Inlezen van de variabelen
      naam <- steekproeven |> pull(naam)
      waarde_laag <- steekproeven |> pull(waarde_laag)
      n_laag <- steekproeven |> pull(n_laag)
      k_laag <- steekproeven |> pull(k_laag)
      ihr <- steekproeven |> pull(ihr)
      ibr <- steekproeven |> pull(ibr)
      car <- steekproeven |> pull(car)
      materialiteit <- steekproeven |> pull(materialiteit)
      fout_hoog <- steekproeven |> pull(fout_hoog)
      goed_hoog <- steekproeven |> pull(goed_hoog)
      n_hoog <- steekproeven |> pull(n_hoog)
      n_totaal <- steekproeven |> pull(n_totaal)
      waarde_hoog <- steekproeven |> pull(waarde_hoog)
      waarde_populatie <- steekproeven |> pull(waarde_populatie)

      # Basis controles
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

      # Invoercontrole aan de hand van redundantie in parameters.
      {
        if (any(n_totaal != (n_laag + n_hoog))) stop("Inconsistentie: 'n_totaal' is onjuist.")
        if (any(abs(waarde_hoog - (fout_hoog + goed_hoog)) > 0.01)) stop("Inconsistentie: 'waarde_hoog' is onjuist.")
        if (any(abs(waarde_populatie - (waarde_laag + waarde_hoog)) > 0.01)) stop("Inconsistentie: 'waarde_populatie' is onjuist.")
      }

      stopifnot(length(zekerheid) == 1)
      stopifnot(is.numeric(zekerheid))
      stopifnot(0 <= zekerheid && zekerheid <= 1)

      stopifnot(length(granulariteit) == 1)
      stopifnot(is.numeric(granulariteit))
      stopifnot(granulariteit >= 100)
    }

    # Bepaal totaal geldswaarde, inclusief het 100%-getoetste deel.
    totaalgeld_laag <- sum(waarde_laag)
    totaalgeld_fout_hoog <- sum(fout_hoog)
    totaalgeld_goed_hoog <- sum(goed_hoog)
    totaalgeld_algeheel <- totaalgeld_laag + totaalgeld_fout_hoog + totaalgeld_goed_hoog

    # Creeer uitvoertibble, t_uit, met regels per steekproef.
    t_uit <-
      tribble(
        ~ naam, ~ waarde_laag, ~ n_laag, ~ k_laag, ~ ihr, ~ ibr, ~ car,
        ~ materialiteit, ~ fout_hoog, ~ goed_hoog, ~ n_hoog, ~ n_totaal,
        ~ waarde_hoog, ~ waarde_populatie, ~ extra_foutloze_posten, ~ toch_fouten,
        ~ mw_fout, ~ max_fout
      )

    n_steekproeven <- nrow(steekproeven)
    for (i in 1:n_steekproeven) {
      t_uit <- add_row(t_uit,
                       naam = steekproeven$naam[[i]], waarde_laag = steekproeven$waarde_laag[[i]],
                       n_laag = steekproeven$n_laag[[i]], k_laag = steekproeven$k_laag[[i]],
                       ihr = steekproeven$ihr[[i]], ibr = steekproeven$ibr[[i]], car = steekproeven$car[[i]],
                       materialiteit = steekproeven$materialiteit[[i]], fout_hoog = steekproeven$fout_hoog[[i]],
                       goed_hoog = steekproeven$goed_hoog[[i]], n_hoog = steekproeven$n_hoog[[i]],
                       n_totaal = steekproeven$n_totaal[[i]], waarde_hoog = steekproeven$waarde_hoog[[i]],
                       waarde_populatie = steekproeven$waarde_populatie[[i]],
                       extra_foutloze_posten = foutloze_posten_equivalent(steekproeven$ihr[[i]], steekproeven$ibr[[i]], steekproeven$car[[i]], steekproeven$materialiteit[[i]]),
                       toch_fouten = (!(steekproeven$ihr[[i]] == "H" && steekproeven$ibr[[i]] == "H" && steekproeven$car[[i]] == "H") && steekproeven$k_laag[[i]] > 0),
                       mw_fout = NA, max_fout = NA
      )
    }

    # =========================================================================
    # CONVOLUTIE BLOK: Keuze tussen Monte Carlo of FFT
    # =========================================================================

    if (methode == "MonteCarlo") {
      krommen <- matrix(NA, nrow = MC, ncol = n_steekproeven)
      set.seed(start)
      for (i in 1:n_steekproeven) {
        n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
        k_calc <- t_uit$k_laag[[i]]

        if (model == "binomiaal") {
          krommen[, i] <- rbeta(MC, shape1 = 1 + k_calc, shape2 = 1 + n_calc - k_calc)
        } else if (model == "poisson") {
          krommen[, i] <- rgamma(MC, shape = 1 + k_calc, rate = n_calc)
        }
      }

      convolutie <- (krommen %*% t_uit$waarde_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel

      bovengrens <- if (model == "binomiaal") 1 else max(convolutie) + 0.1
      d <- density(convolutie, adjust = 1.5, from = 0, to = bovengrens)

      # Normaliseren
      oppervlakte <- sum(d$y) * (d$x[2] - d$x[1])
      d$y <- d$y / oppervlakte

      max_fout_convolutie <- unname(quantile(convolutie, probs = zekerheid))
      mediaan_fout_convolutie <- unname(quantile(convolutie, probs = 0.5))
      modus_fout_convolutie <- d$x[which.max(d$y)]
      gemiddelde_fout_convolutie <- mean(convolutie)

    } else if (methode == "FFT") {

      if (totaalgeld_laag < 0.01) {
        # Edge case: Geen statistische controle, alles integraal.
        frac <- totaalgeld_fout_hoog / totaalgeld_algeheel
        d <- list(x = frac, y = 1)
        max_fout_convolutie <- frac
        mediaan_fout_convolutie <- frac
        modus_fout_convolutie <- frac
        gemiddelde_fout_convolutie <- frac
      } else {
        # We baseren de stapgrootte op het totale bedrag van het laagstratum
        dx <- totaalgeld_laag / granulariteit
        p_strata <- list()

        for (i in 1:n_steekproeven) {
          w_laag <- t_uit$waarde_laag[[i]]
          n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
          k_calc <- t_uit$k_laag[[i]]

          if (w_laag > 0) {
            # Bepaal hoe ver de as moet doorlopen. Binomiaal is max w_laag.
            # Poisson kan daar theorethisch overheen gaan, dus nemen we een veilige grens via qgamma.
            max_frac <- if (model == "binomiaal") 1 else qgamma(0.9999, shape = k_calc + 1, rate = n_calc)
            x_grens <- max(w_laag, max_frac * w_laag)
            x_as <- seq(0, x_grens, by = dx)

            if (model == "binomiaal") {
              p <- dbeta(x_as / w_laag, shape1 = k_calc + 1, shape2 = n_calc - k_calc + 1)
            } else {
              p <- dgamma(x_as / w_laag, shape = k_calc + 1, rate = n_calc)
            }

            p[is.na(p) | is.infinite(p)] <- 0
            p <- p / sum(p) # Normaliseer kansmassa voor deze steekproef naar 1
            p_strata[[length(p_strata) + 1]] <- p
          }
        }

        # Paarsgewijze convolutie starten
        if (length(p_strata) > 0) {
          p_totaal <- p_strata[[1]]
          if (length(p_strata) > 1) {
            for (j in 2:length(p_strata)) {
              # Convolve vereist rev() op de tweede array voor statistische convolutie
              p_totaal <- convolve(p_totaal, rev(p_strata[[j]]), type = "open")
            }
          }

          p_totaal <- p_totaal / sum(p_totaal) # Finale opschoning/normalisatie

          # Bouw de totale assen op
          x_totaal_laag <- seq(0, by = dx, length.out = length(p_totaal))
          x_totaal_geld <- x_totaal_laag + totaalgeld_fout_hoog
          x_totaal_fractie <- x_totaal_geld / totaalgeld_algeheel

          # Omzetten naar het 'd' formaat, schalen naar kansdichtheid t.o.v. de fractie-as
          dx_fractie <- dx / totaalgeld_algeheel
          d <- list(x = x_totaal_fractie, y = p_totaal / dx_fractie)

          cum_p <- cumsum(p_totaal)

          # Max fout (zekerheid)
          idx_max <- which(cum_p >= zekerheid)[1]
          max_fout_convolutie <- if (is.na(idx_max)) x_totaal_fractie[length(x_totaal_fractie)] else x_totaal_fractie[idx_max]

          # Mediaan
          idx_med <- which(cum_p >= 0.5)[1]
          mediaan_fout_convolutie <- if (is.na(idx_med)) x_totaal_fractie[length(x_totaal_fractie)] else x_totaal_fractie[idx_med]

          modus_fout_convolutie <- x_totaal_fractie[which.max(p_totaal)]
          gemiddelde_fout_convolutie <- sum(x_totaal_fractie * p_totaal)
        }
      }
    }

    # =========================================================================
    # LOS EN ALS1 (VERGELIJKING)
    # =========================================================================

    mw_fout_los <- NA; max_fout_los <- NA
    mw_fout_als1 <- NA; max_fout_als1 <- NA

    if (vergelijk) {
      # LOS
      {
        for (i in 1:n_steekproeven) {
          n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
          k_calc <- t_uit$k_laag[[i]]

          t_uit$mw_fout[[i]] <- k_calc / n_calc
          if (model == "binomiaal") {
            t_uit$max_fout[[i]] <- qbeta(zekerheid, k_calc + 1, n_calc - k_calc + 1)
          } else if (model == "poisson") {
            t_uit$max_fout[[i]] <- qgamma(zekerheid, shape = k_calc + 1, rate = n_calc)
          }
        }

        mw_fout_los_geld <- sum(t_uit$mw_fout * t_uit$waarde_laag) + totaalgeld_fout_hoog
        max_fout_los_geld <- sum(t_uit$max_fout * t_uit$waarde_laag) + totaalgeld_fout_hoog

        mw_fout_los <- mw_fout_los_geld / totaalgeld_algeheel
        max_fout_los <- max_fout_los_geld / totaalgeld_algeheel
      }

      # ALS1
      n_calc_als1 <- sum(t_uit$n_laag) + sum(t_uit$extra_foutloze_posten)
      k_calc_als1 <- sum(t_uit$k_laag)

      mw_fout_als1_laag <- k_calc_als1 / n_calc_als1
      if (model == "binomiaal") {
        max_fout_als1_laag <- qbeta(zekerheid, k_calc_als1 + 1, n_calc_als1 - k_calc_als1 + 1)
      } else if (model == "poisson") {
        max_fout_als1_laag <- qgamma(zekerheid, shape = k_calc_als1 + 1, rate = n_calc_als1)
      }
      mw_fout_als1 <- (mw_fout_als1_laag * totaalgeld_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel
      max_fout_als1 <- (max_fout_als1_laag * totaalgeld_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel
    }

    invoer <- list(
      steekproeven = steekproeven,
      model = model,
      zekerheid = zekerheid,
      methode = methode,
      granulariteit = granulariteit,
      MC = MC,
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
      max_fout_convolutie = max_fout_convolutie,
      max_fout_convolutie_geld = max_fout_convolutie * totaalgeld_algeheel,
      vergelijk_met = list(
        mw_fout_los = mw_fout_los,
        mw_fout_los_geld = mw_fout_los * totaalgeld_algeheel,
        max_fout_los = max_fout_los,
        max_fout_los_geld = max_fout_los * totaalgeld_algeheel,
        mw_fout_als1 = mw_fout_als1,
        mw_fout_als1_geld = mw_fout_als1 * totaalgeld_algeheel,
        max_fout_als1 = max_fout_als1,
        max_fout_als1_geld = max_fout_als1 * totaalgeld_algeheel
      ),
      steekproeven = t_uit,
      invoer = invoer
    )
  }
