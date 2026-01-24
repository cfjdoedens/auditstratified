#' @title
#' Evalueer samen de resultaten van 1 of meer steekproeven op uitgaand geld
#'
#' @description
#' Het samennemen van de resultaten gebeurt door convolutie van
#' de foutkanskrommes van de afzonderlijke steekproeven tot
#' 1 foutkanskromme.
#'
#' We berekenen de meest waarschijnlijke en de maximale fout als fractie
#' en in geld.
#'
#' De meest waarschijnlijke fout is de modus van de kanskromme.
#' De maximale fout is afhankelijk van de gevraagde zekerheid, en
#' is de fout bij een cumulatieve kans gelijk aan deze zekerheid.
#'
#' @details
#' We gaan uit van de som van de foutfracties, de k-waarde, dus we kijken niet
#' naar
#' de foutfracties per post.
#'
#' De maximale fout wordt bepaald aan de hand van de resulterende kanskromme,
#' op basis
#' van de gewenste zekerheid. Visueel is de maximale fout, pm, te bepalen in een
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
#' De maximale fout is dan 1,83% ofwel ongeveer3.660.000.
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
#' De tibble heeft als kolommen: naam w n k car cir dir.
#' Dit zijn repectievelijk
#' \code{naam}, een aanduiding van de steekproef,
#' \code{w}, de omvang in geld van de massa waaruit getrokken is,
#' \code{n}, het aantal getrokken posten,
#' \code{k}, de som van de foutfracties van de posten,
#' \code{ihr}, inherent risico, te weten H, M of L,
#' \code{ibr}, intern beheersingsrisico, te weten H, M of L,
#' \code{car}, cijferanalyserisico, te weten H, M of L, en
#' \code{materialiteit}, als fractie van de totale massa
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
#' Een lijst, bestaande uit
#' - \code{mw_fout_convolutie}, de meest waarschijnlijke fout als fractie van
#'   de totale massa in geld
#' - \code{max_fout_convolutie}, de maximale fout als fractie van de totale
#'   massa in geld
#' - en zo ook voor modus, mediaan en gemiddelde
#' - als vergelijk == TRUE een lijst
#'   \code{vergelijk_met}, met daarin vergelijkende cijfers voor "los" en voor
#'   "als1".
#'   Hierbij staat "los" voor de samengenomen, gewogen, cijfers der
#'   afzonderlijke steekproeven,
#'   en "als1" voor als alle steken en de resultaten daarvan worden beschouwd
#'   als voor 1 steekproef getrokken uit 1 massa.
#' - \code{steekproeven}, de invoer tibble, verrijkt per steekproef: met
#'   + \code{extra_foutloze_posten},
#'     het aantal foutloze posten dat equivalent is aan ihr+ibr+car volgens
#'     het ARM model en de interpretatie daarvan door de ADR,
#'   + \code{toch_fouten}, TRUE of FALSE,
#'     geeft aan of tenminste 1 van ihr, ibr of car niet op hoog staat
#'     en er wel fouten zijn gevonden
#'   + \code{mw_fout}, als vergelijk TRUE,
#'          de meest waarschijnlijke fout voor die steekproef als
#'          fractie van de massa in geld van de steekproef,
#'   + \code{max_fout}, als vergelijk TRUE,
#'          de maximale fout als fractie van de massa in geld van de steekproef,
#' - \code{invoer}, een lijst bestaande uit de invoerparameters
#'
#' @export
#' @importFrom dplyr pull
#' @importFrom stats density
#' @importFrom stats quantile
#' @importFrom stats rbeta
#' @importFrom stats qbeta
#' @importFrom tibble add_row
#' @importFrom tibble is_tibble
#' @importFrom tibble tribble
#' @examples
#' # Creeer lege invoertibbe.
#' steekproeven <-
#'   tibble::tribble(~naam, ~w, ~n, ~k, ~ihr, ~ibr, ~car, ~materialiteit)
#' naam <- "Steekproef1" # De naam van de 1e steekproef.
#' w <- 35060542 # Het gewicht van de steekproef, als geldomvang van de massa
#' # waarover wordt gestoken.
#' n <- 31 # Het aantal getrokken en geevalueerde posten.
#' k <- 0 # De som van de foutfracties.
#' ihr <- "H"
#' ibr <- "H"
#' car <- "H"
#' materialiteit <- 0.01
#' steekproeven <-
#'   tibble::add_row(steekproeven,
#'     naam = naam,
#'     w = w,
#'     n = n,
#'     k = k,
#'     ihr = ihr,
#'     ibr = ibr,
#'     car = car,
#'     materialiteit = materialiteit
#'   )
#'
#' naam <- "Steekproef2" # De naam van de 2e steekproef.
#' w <- 3044699 # Het gewicht van de steekproef, als geldomvang van de
#' # massa waarover wordt gestoken.
#' n <- 8 # Het aantal getrokken en geevalueerde posten.
#' k <- 0 # De som van de foutfracties.
#' ihr <- "H"
#' ibr <- "H"
#' car <- "H"
#' materialiteit <- 0.1
#' tibble::add_row(steekproeven,
#'   naam = naam,
#'   w = w,
#'   n = n,
#'   k = k,
#'   ihr = ihr,
#'   ibr = ibr,
#'   car = car,
#'   materialiteit = materialiteit
#' )
#'
#' # Kortheidshalve hadden we steekproeven ook kunnen invoeren als:
#' steekproeven <- tibble::tribble(
#'   ~naam, ~w, ~n, ~k, ~ihr, ~ibr, ~car, ~materialiteit,
#'   "Steekproef1", 35060542, 31, 0, "H", "H", "H", 0.01,
#'   "Steekproef2", 3044699, 8, 0, "H", "H", "H", 0.01
#' )
#'
#' # Evalueer steekproef1 en steekproef2 samen.
#' eval_stratified(steekproeven = steekproeven)
eval_stratified <-
  function(steekproeven,
           zekerheid = 0.95,
           MC = 1e7,
           start = 1,
           vergelijk = TRUE) {
    # Controleer de invoer.
    {
      stopifnot(is_tibble(steekproeven))
      stopifnot("naam" %in% colnames(steekproeven))
      stopifnot("w" %in% colnames(steekproeven))
      stopifnot("n" %in% colnames(steekproeven))
      stopifnot("k" %in% colnames(steekproeven))
      stopifnot("ihr" %in% colnames(steekproeven))
      stopifnot("ibr" %in% colnames(steekproeven))
      stopifnot("car" %in% colnames(steekproeven))
      stopifnot("materialiteit" %in% colnames(steekproeven))

      naam <- steekproeven |> pull(naam)
      w <- steekproeven |> pull(w)
      n <- steekproeven |> pull(n)
      k <- steekproeven |> pull(k)
      ihr <- steekproeven |> pull(ihr)
      ibr <- steekproeven |> pull(ibr)
      car <- steekproeven |> pull(car)
      materialiteit <- steekproeven |> pull(materialiteit)
      len_naam <- length(naam)
      stopifnot(len_naam > 0)
      len_w <- length(w)
      len_n <- length(n)
      len_k <- length(k)
      len_ihr <- length(ihr)
      len_ibr <- length(ibr)
      len_car <- length(car)
      len_materialiteit <- length(materialiteit)
      stopifnot(len_naam == len_w)
      stopifnot(len_naam == len_n)
      stopifnot(len_naam == len_k)
      stopifnot(len_naam == len_ihr)
      stopifnot(len_naam == len_ibr)
      stopifnot(len_naam == len_car)
      stopifnot(len_naam == len_materialiteit)
      stopifnot(ihr %in% c("H", "M", "L"))
      stopifnot(ihr %in% c("H", "M", "L"))
      stopifnot(ibr %in% c("H", "M", "L"))
      stopifnot(car %in% c("H", "M", "L"))

      stopifnot(is.numeric(w))
      stopifnot(is.numeric(n))
      stopifnot(is.numeric(k))
      stopifnot(is.numeric(materialiteit))
      stopifnot(0 < w)
      stopifnot(0 < n)
      stopifnot(0 <= k)
      stopifnot(k <= n)
      stopifnot(0 < materialiteit)
      stopifnot(materialiteit < 1)
      stopifnot(is.finite(w))
      stopifnot(is.finite(n))
      stopifnot(is.finite(k))

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

    # Bepaal totaal geldswaarde.
    totaalgeld <- sum(w)

    # Creeer uitvoertibble, t_uit, met regels per steekproef.
    t_uit <-
      tribble(
        ~ naam,
        ~ w,
        ~ n,
        ~ k,
        ~ ihr,
        ~ ibr,
        ~ car,
        ~ materialiteit,
        ~ extra_foutloze_posten,
        ~ toch_fouten,
        ~ mw_fout,
        ~ max_fout
      )

    # Vul t_uit met invoertibble, en zet andere velden op
    # NA (Not Available).
    n_steekproeven <- nrow(steekproeven)
    for (i in 1:n_steekproeven) {
      t_uit <-
        add_row(
          t_uit,
          naam = steekproeven$naam[[i]],
          w = steekproeven$w[[i]],
          n = steekproeven$n[[i]],
          k = steekproeven$k[[i]],
          ihr = steekproeven$ihr[[i]],
          ibr = steekproeven$ibr[[i]],
          car = steekproeven$car[[i]],
          materialiteit = steekproeven$materialiteit[[i]],
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
            t_uit$car[[i]] == "H") && t_uit$k[[i]] > 0) {
        t_uit$toch_fouten[[i]] <- TRUE
      } else {
        t_uit$toch_fouten[[i]] <- FALSE
      }
    }

    # CONVOLUTIE
    # Neem de convolutie van de kanskrommen en bepaal
    # daar de meest waarschijnlijke en de maximale fout van.
    {
      # Maak de afzonderlijke kanskrommen van elk van de steekproeven door
      # middel van Monte Carlo simulatie.
      krommen <- matrix(NA, nrow = MC, ncol = n_steekproeven)
      set.seed(start)
      for (i in 1:n_steekproeven) {
        n <- t_uit$n[[i]] + t_uit$extra_foutloze_posten[[i]]
        k <- t_uit$k[[i]]
        krommen[, i] <-
          # Hier maken we kanskromme i.
          # Let op: de 'kromme' wordt gerepresenteerd als een 1-dimensionale
          # vector van waarden.
          # De dichtheid van de waarden geeft de hoogte van de kromme aan.
          # Dus bijvoorbeeld als de waarden rondom 0.45 veel voorkomen, dan
          # is daar de kanskromme hoog.
          rbeta(MC, shape1 = 1 + k, shape2 = 1 + n - k)
      }

      # Voeg de kanskrommen samen tot 1 kanskromme: "convolutie" geheten.
      # "convolutie" wordt gerepresenteerd als hierboven,
      # dus als een 1-dimensionale vector.
      convolutie <-
        krommen %*% t_uit$w / totaalgeld # Gewogen samengevoegde kanskrommen.
      stopifnot(ncol(convolutie) == 1) # 1 kanskromme dus.

      # We bepalen de maximale fout door het kwantiel van de samengevoegde
      # kanskrommen te nemen
      # op de gegeven zekerheid, bijvoorbeeld op een zekerheid van 95%.
      # Max gebaseerd op 1-zijdige significantie.
      max_fout_convolutie <-
        unname(quantile(convolutie, probs = zekerheid))

      # We bepalen de mediaan van de samengevoegde kanskrommen.
      # De mediaan is de middelste waarde.
      mediaan_fout_convolutie <-
        unname(quantile(convolutie, probs = 0.5))

      # We bepalen de modus van de samengevoegde kanskrommen.
      # De modus is de piek van de kanskromme.
      d <- density(convolutie)
      modus_fout_convolutie <- d$x[which.max(d$y)]

      # We bepalen het gemiddelde van de samengevoegde kanskrommen.
      gemiddelde_fout_convolutie <- mean(convolutie)
    }

    # Zet de volgende velden op NA.
    # Als vergelijk == TRUE, worden ze alsnog ingevuld.
    mw_fout_los <- NA
    max_fout_los <- NA
    mw_fout_als1 <- NA
    max_fout_als1 <- NA

    # Als daarom wordt gevraagd, dus vergelijk == TRUE,
    # berekenen we ter vergelijking ook een aantal extra zaken:
    if (vergelijk) {
      # LOS
      {
        # Evaluaties van de afzonderlijke steekproeven.
        for (i in 1:n_steekproeven) {
          n <- t_uit$n[[i]] + t_uit$extra_foutloze_posten[[i]]
          k <- t_uit$k[[i]]

          # De modale fout, ook wel genoemd de meest waarschijnlijke fout.
          # Deze wordt frequentistisch en Bayesiaans op dezelfde wijze berekend.
          t_uit$mw_fout[[i]] <- k / n

          # De maximale fout gegeven deze zekerheid.
          t_uit$max_fout[[i]] <- qbeta(zekerheid, k + 1, n - k + 1)
        }

        # Voeg modale fouten, respectievelijk maximale fouten, bij elkaar.
        mw_fout_los <- sum(t_uit$mw_fout * t_uit$w) / totaalgeld
        max_fout_los <- sum(t_uit$max_fout * t_uit$w) / totaalgeld
      }

      # ALS1
      # Evalueer alle steekproeven samen, alsof het
      # 1 steekproef is op 1 massa.
      n <- sum(t_uit$n) + sum(t_uit$extra_foutloze_posten)
      k <- sum(t_uit$k)
      mw_fout_als1 <- k / n # De modale fout, ook wel genoemd de meest waarschijnlijke fout.
      max_fout_als1 <- qbeta(zekerheid, k + 1, n - k + 1)
    }

    # Maak een lijst van de invoerparameters.
    invoer <- list(
      steekproeven = steekproeven,
      zekerheid = zekerheid,
      MC = MC,
      start = start,
      vergelijk = vergelijk
    )

    list(
      modus_fout_convolutie = modus_fout_convolutie,
      modus_fout_convolutie_geld = modus_fout_convolutie * totaalgeld,
      mediaan_fout_convolutie = mediaan_fout_convolutie,
      mediaan_fout_convolutie_geld = mediaan_fout_convolutie * totaalgeld,
      gemiddelde_fout_convolutie = gemiddelde_fout_convolutie,
      gemiddelde_fout_convolutie_geld = gemiddelde_fout_convolutie * totaalgeld,
      mw_fout_convolutie = modus_fout_convolutie,
      mw_fout_convolutie_geld = modus_fout_convolutie * totaalgeld,
      max_fout_convolutie = max_fout_convolutie,
      max_fout_convolutie_geld = max_fout_convolutie * totaalgeld,
      vergelijk_met = list(
        mw_fout_los = mw_fout_los,
        mw_fout_los_geld = mw_fout_los * totaalgeld,
        max_fout_los = max_fout_los,
        max_fout_los_geld = max_fout_los * totaalgeld,
        mw_fout_als1 = mw_fout_als1,
        mw_fout_als1_geld = mw_fout_als1 * totaalgeld,
        max_fout_als1 = max_fout_als1,
        max_fout_als1_geld = max_fout_als1 * totaalgeld
      ),
      steekproeven = t_uit,
      invoer = invoer
    )
  }
