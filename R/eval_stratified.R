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
#' Deze module kan ook steekproeven combineren over massa's waarvoor
#' een verschillende risicoinschatting geldt.
#'
#' Bijvoorbeeld:
#' Steekproef1 is gebaseerd op een zekerheid van 95% omdat
#' ihr, ibr en car alledrie op hoog (H) staan.
#' De materialiteit is 2%.
#' Het betreft 100 miljoen euro.
#' Voor steekproef1 trekken we 148 posten, waarbij 1 fout blijkt.
#' Steekproef2 is gebaseerd op een zekerheid van 64% omdat
#' ihr en ibr allebei op laag staan en alleen car op hoog.
#' Het betreft ook 100 miljoen euro en een materialiteit van 2%.
#' Voor steekproef2 trekken we 50 posten waarvan er 0 fout blijken.
#'
#' Bij een risicoinschatting onder 95% van een of meer van de massa's waarover
#' wordt gestoken worden deze lagere risicoinschattingen vertaald naar
#' extra getrokken foutloze posten.
#' In ons voorbeeld bepalen we voor steekproef2 het aantal foutloze
#' posten nodig om een positieve uitspraak te doen bij 64% en bij 95%.
#' Dit zijn respectievelijk 50 en 148.
#' Het verschil is 148-50 = 98 posten.
#' Daarna berekenen we totale maximale fout op basis van
#' een zekerheid van 95%,
#' en steekproef1, 148 posten waarvan 1 fout,
#' en steekproef2, met 50 + 98 posten, waarvan 0 fout.
#' De maximale fout is dan 1,83% ofwel ongeveer 3.660.000.
#' De meest waarschijnlijke fout 0,52% ofwel ongeveer 1.160.000 euro.
#'
#' Het is de verantwoordelijkheid van de auditor hoe om te gaan
#' met een steekproef waarbij de risicoinschatting niet op H staat
#' en er toch fouten worden gevonden. Dit probleem staat los
#' van hoe de uitkomsten van meerdere steekproeven samen te nemen.
#'
#' Als de parameter vergelijk TRUE is doen we, ter vergelijking, ook een
#' evaluatie:
#' - voor elke steekproef los
#' - voor alle steekproeven samen, waarbij ze beschouwd worden als te zijn
#'   getrokken op 1 massa,
#'   en als 1 steekproef.
#'
#' @param steekproeven
#' Een tibble.
#' Elke regel van de tibble beschrijft 1 steekproef, dus 1 van de genomen
#' steekproeven.
#' De tibble eist de volgende expliciete kolommen voor redundantie en veiligheid:
#' \code{naam}, een aanduiding van de steekproef,
#' \code{waarde_laag}, de omvang in geld van de massa waaruit de steekproef (laagstratum) is getrokken,
#' \code{n_laag}, het aantal getrokken posten in het laagstratum,
#' \code{k_laag}, de som van de foutfracties van de posten in het laagstratum,
#' \code{ihr}, inherent risico, te weten H, M of L,
#' \code{ibr}, intern beheersingsrisico, te weten H, M of L,
#' \code{car}, cijferanalyserisico, te weten H, M of L, en
#' \code{materialiteit}, als fractie van de totale massa.
#' \code{fout_hoog}, Het gevonden foutbedrag in het 100%-getoetste topstratum.
#' \code{goed_hoog}, Het goedgekeurde bedrag in het 100%-getoetste topstratum.
#' \code{n_hoog}, Het aantal posten in het hoogstratum.
#' \code{n_totaal}, Het totale aantal posten in deze audit (n_laag + n_hoog).
#' \code{waarde_hoog}, De totale boekwaarde van het hoogstratum (fout_hoog + goed_hoog).
#' \code{waarde_populatie}, De totale boekwaarde van de hele populatie (waarde_laag + waarde_hoog).
#' @param zekerheid
#' Het zekerheidsniveau waarop we de maximale foutfractie berekenen.
#' @param MC
#' Het aantal Monte Carlo iteraties dat gebruikt wordt.
#' Monte Carlo berekeningen baseren zich op toevalsgetallen.
#' @param start
#' Startwaarde voor de toevalsgenerator.
#' @param vergelijk
#' TRUE of FALSE, als TRUE dan worden wat vergelijkende berekeningen uitgevoerd
#' en de resultaten daarvan toegevoegd aan de uitkomst van de functie.
#' @returns
#' Een lijst, bestaande uit de convolutie-uitkomsten (fracties en geld),
#' eventuele vergelijkingen, en de verrijkte invoergegevens.
#'
#' @export
#' @importFrom dplyr pull mutate
#' @importFrom stats density quantile rbeta qbeta
#' @importFrom tibble add_row is_tibble tribble
eval_stratified <-
  function(steekproeven,
           zekerheid = 0.95,
           MC = 1e7,
           start = 1,
           vergelijk = TRUE) {
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
      stopifnot(length(ihr) == len_naam)
      stopifnot(length(ibr) == len_naam)
      stopifnot(length(car) == len_naam)
      stopifnot(length(materialiteit) == len_naam)
      stopifnot(length(fout_hoog) == len_naam)
      stopifnot(length(goed_hoog) == len_naam)
      stopifnot(length(n_hoog) == len_naam)
      stopifnot(length(n_totaal) == len_naam)
      stopifnot(length(waarde_hoog) == len_naam)
      stopifnot(length(waarde_populatie) == len_naam)

      stopifnot(ihr %in% c("H", "M", "L"))
      stopifnot(ibr %in% c("H", "M", "L"))
      stopifnot(car %in% c("H", "M", "L"))

      stopifnot(is.numeric(waarde_laag))
      stopifnot(is.numeric(n_laag))
      stopifnot(is.numeric(k_laag))
      stopifnot(is.numeric(materialiteit))
      stopifnot(is.numeric(fout_hoog))
      stopifnot(is.numeric(goed_hoog))
      stopifnot(is.numeric(n_hoog))
      stopifnot(is.numeric(n_totaal))
      stopifnot(is.numeric(waarde_hoog))
      stopifnot(is.numeric(waarde_populatie))

      stopifnot(0 <= waarde_laag)
      stopifnot(0 <= n_laag)
      stopifnot(0 <= k_laag)
      stopifnot(k_laag <= n_laag)
      stopifnot(0 < materialiteit)
      stopifnot(materialiteit < 1)
      stopifnot(fout_hoog >= 0)
      stopifnot(goed_hoog >= 0)
      stopifnot(n_hoog >= 0)
      stopifnot(n_totaal > 0)

      stopifnot(is.finite(waarde_laag))
      stopifnot(is.finite(n_laag))
      stopifnot(is.finite(k_laag))

      # Invoercontrole aan de hand van redundantie in parameters.
      # We gebruiken abs(x - y) < 0.01 om onzichtbare afrondingsfouten in kommagetallen te negeren.
      {
        # 1. Check of posten-aantallen kloppen (n_totaal == n_laag + n_hoog)
        if (any(n_totaal != (n_laag + n_hoog))) {
          stop(
            "Inconsistentie gevonden: 'n_totaal' is niet gelijk aan de som van 'n_laag' en 'n_hoog'."
          )
        }

        # 2. Check of het hoogstratum klopt (waarde_hoog == fout_hoog + goed_hoog)
        if (any(abs(waarde_hoog - (fout_hoog + goed_hoog)) > 0.01)) {
          stop(
            "Inconsistentie gevonden: 'waarde_hoog' is niet exact gelijk aan de som van 'fout_hoog' en 'goed_hoog'."
          )
        }

        # 3. Check of de totale populatie klopt (waarde_populatie == waarde_laag + waarde_hoog)
        if (any(abs(waarde_populatie - (waarde_laag + waarde_hoog)) > 0.01)) {
          stop(
            "Inconsistentie gevonden: 'waarde_populatie' is niet gelijk aan de som van het laagstratum ('waarde_laag') en het hoogstratum ('waarde_hoog')."
          )
        }
      }

      stopifnot(length(zekerheid) == 1)
      stopifnot(is.numeric(zekerheid))
      stopifnot(0 <= zekerheid)
      stopifnot(zekerheid <= 1)

      stopifnot(length(MC) == 1)
      stopifnot(is.numeric(MC))
      stopifnot(is.finite(MC))
      stopifnot(rlang::is_integerish((MC)))
      stopifnot(MC >= 1)

      stopifnot(length(start) == 1)
      stopifnot(is.numeric(start))

      stopifnot(length(vergelijk) == 1)
      stopifnot(is.logical(vergelijk))
    }

    # Bepaal totaal geldswaarde, nu inclusief het 100%-getoetste deel
    totaalgeld_laag <- sum(waarde_laag)
    totaalgeld_fout_hoog <- sum(fout_hoog)
    totaalgeld_goed_hoog <- sum(goed_hoog)
    totaalgeld_algeheel <- totaalgeld_laag + totaalgeld_fout_hoog + totaalgeld_goed_hoog

    # Creeer uitvoertibble, t_uit, met regels per steekproef.
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
        ~ max_fout
      )

    # Vul t_uit met invoertibble, en zet andere velden op NA.
    n_steekproeven <- nrow(steekproeven)
    for (i in 1:n_steekproeven) {
      t_uit <-
        add_row(
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
          extra_foutloze_posten = NA,
          toch_fouten = NA,
          mw_fout = NA,
          max_fout = NA
        )
    }

    # Vul extra_foutloze_posten.
    for (i in 1:n_steekproeven) {
      t_uit$extra_foutloze_posten[[i]] <-
        foutloze_posten_equivalent(t_uit$ihr[[i]], t_uit$ibr[[i]], t_uit$car[[i]], t_uit$materialiteit[[i]])
    }

    # Vul toch_fouten.
    for (i in 1:n_steekproeven) {
      if (!(t_uit$ihr[[i]] == "H" &&
            t_uit$ibr[[i]] == "H" &&
            t_uit$car[[i]] == "H") && t_uit$k_laag[[i]] > 0) {
        t_uit$toch_fouten[[i]] <- TRUE
      } else {
        t_uit$toch_fouten[[i]] <- FALSE
      }
    }

    # CONVOLUTIE
    {
      krommen <- matrix(NA, nrow = MC, ncol = n_steekproeven)
      set.seed(start)
      for (i in 1:n_steekproeven) {
        n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
        k_calc <- t_uit$k_laag[[i]]
        krommen[, i] <- rbeta(MC,
                              shape1 = 1 + k_calc,
                              shape2 = 1 + n_calc - k_calc)
      }

      # We zetten de geprojecteerde foutfracties van het laagstratum om naar bedragen,
      # tellen ALLE harde deterministische fouten uit de hoogstrata hierbij op,
      # en delen door het algehele totaal om de nieuwe kanskromme van de *totale* fractie te krijgen.
      convolutie <-
        (krommen %*% t_uit$waarde_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel
      stopifnot(ncol(convolutie) == 1)

      max_fout_convolutie <- unname(quantile(convolutie, probs = zekerheid))
      mediaan_fout_convolutie <- unname(quantile(convolutie, probs = 0.5))

      d <- density(convolutie)
      modus_fout_convolutie <- d$x[which.max(d$y)]
      gemiddelde_fout_convolutie <- mean(convolutie)
    }

    mw_fout_los <- NA
    max_fout_los <- NA
    mw_fout_als1 <- NA
    max_fout_als1 <- NA

    if (vergelijk) {
      # LOS
      {
        for (i in 1:n_steekproeven) {
          n_calc <- t_uit$n_laag[[i]] + t_uit$extra_foutloze_posten[[i]]
          k_calc <- t_uit$k_laag[[i]]

          # Dit blijft de fractie van puur de steekproef (laagstratum) in de tibble output
          t_uit$mw_fout[[i]] <- k_calc / n_calc
          t_uit$max_fout[[i]] <- qbeta(zekerheid, k_calc + 1, n_calc - k_calc + 1)
        }

        # Bij het optellen van LOS nemen we ook per stratum het hoogstratum bedrag mee
        mw_fout_los_geld <- sum(t_uit$mw_fout * t_uit$waarde_laag) + totaalgeld_fout_hoog
        max_fout_los_geld <- sum(t_uit$max_fout * t_uit$waarde_laag) + totaalgeld_fout_hoog

        mw_fout_los <- mw_fout_los_geld / totaalgeld_algeheel
        max_fout_los <- max_fout_los_geld / totaalgeld_algeheel
      }

      # ALS1
      n_calc_als1 <- sum(t_uit$n_laag) + sum(t_uit$extra_foutloze_posten)
      k_calc_als1 <- sum(t_uit$k_laag)

      # Bereken eerst voor het steekproefdeel, tel daarna het totaal hoogstratum erbij op
      mw_fout_als1_laag <- k_calc_als1 / n_calc_als1
      max_fout_als1_laag <- qbeta(zekerheid, k_calc_als1 + 1, n_calc_als1 - k_calc_als1 + 1)

      mw_fout_als1 <- (mw_fout_als1_laag * totaalgeld_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel
      max_fout_als1 <- (max_fout_als1_laag * totaalgeld_laag + totaalgeld_fout_hoog) / totaalgeld_algeheel
    }

    invoer <- list(
      steekproeven = steekproeven,
      zekerheid = zekerheid,
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
