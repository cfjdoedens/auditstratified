#' @title Evalueer steekproeven (Python Logic Port)
#' @description
#' Een R-implementatie van de logica uit 'pythoncode evalstratified'.
#' Ondersteunt modellen: 'beta', 'poisson', en 'mus'.
#'
#' @param steekproeven Een data.frame of tibble met kolommen: naam, w, n, k, ihr, ibr, car, materialiteit.
#' @param model Het te gebruiken model: "beta", "poisson", of "mus".
#' @param zekerheid De gewenste zekerheid (bijv. 0.95).
#' @param MC Aantal Monte Carlo simulaties (default uit Python code is 100000).
#' @param start Seed voor reproduceerbaarheid.
#' @param vergelijk Boolean. Voer vergelijkende berekeningen (Los/Als1) uit?
#'
#' @return Een lijst met resultaten en de verrijkte steekproefdata.
#' @export
eval_stratifiedp <- function(steekproeven,
                                         model = "beta",
                                         zekerheid = 0.95,
                                         MC = 1e7,
                                         start = 1,
                                         vergelijk = TRUE) {

  # Benodigde packages laden (indien nog niet geladen)
  if (!requireNamespace("stats", quietly = TRUE)) stop("Package 'stats' nodig.")

  # ---------------------------------------------------------
  # Hulpfuncties (Equivalent aan Python functies)
  # ---------------------------------------------------------

  # [cite: 2-4] haro_nog_nodige_zekerheid
  haro_nog_nodige_zekerheid <- function(ihr = "H", ibr = "H", car = "H") {
    valid_risks <- c("H", "M", "L")
    if (!all(c(ihr, ibr, car) %in% valid_risks)) {
      stop("Risico's moeten Hoog, Gemiddeld of Laag zijn")
    }

    # Mapping waarden [cite: 3]
    ihr_val <- switch(ihr, "H" = 1.00, "M" = 0.63, "L" = 0.40)
    ibr_val <- switch(ibr, "H" = 1.00, "M" = 0.52, "L" = 0.34)
    car_val <- switch(car, "H" = 1.00, "M" = 0.50, "L" = 0.25) # [cite: 4]

    auditrisico <- 0.05
    detectierisico <- auditrisico / (ihr_val * ibr_val * car_val)
    nog_nodige_zekerheid <- max(0.0, 1.0 - detectierisico)

    # HARo-correctie [cite: 4]
    if (nog_nodige_zekerheid <= 0.07) {
      nog_nodige_zekerheid <- nog_nodige_zekerheid + 0.05
    }
    return(nog_nodige_zekerheid)
  }

  # [cite: 5] drawsneeded
  drawsneeded <- function(k, materialiteit, cert) {
    if (k != 0) stop("Alleen k=0 ondersteund")
    if (materialiteit <= 0 || materialiteit >= 1) stop("materialiteit moet tussen 0 en 1 liggen")
    if (cert <= 0 || cert >= 1) stop("cert moet tussen 0 en 1 liggen")

    n <- -log(1 - cert) / materialiteit
    return(ceiling(n))
  }

  # [cite: 6-7] foutloze_posten_equivalent
  foutloze_posten_equivalent <- function(ihr, ibr, car, materialiteit) {
    hoogste_zekerheid <- 0.95
    benodigde_zekerheid <- haro_nog_nodige_zekerheid(ihr, ibr, car)

    if (benodigde_zekerheid > hoogste_zekerheid) {
      stop("Benodigde zekerheid > 95%")
    }

    posten_alles_hoog <- drawsneeded(0, materialiteit, hoogste_zekerheid)
    posten_niet_alles_hoog <- drawsneeded(0, materialiteit, benodigde_zekerheid)

    return(max(0, posten_alles_hoog - posten_niet_alles_hoog))
  }

  # ---------------------------------------------------------
  # Validatie en Setup [cite: 16-17]
  # ---------------------------------------------------------
  if (!model %in% c("beta", "poisson", "mus")) stop("Model moet 'beta', 'poisson' of 'mus' zijn")
  if (zekerheid < 0 || zekerheid > 1) stop("Zekerheid moet tussen 0 en 1 liggen")

  # Dataframe voorbereiden
  t_uit <- as.data.frame(steekproeven)
  totaalgeld <- sum(t_uit$w) # [cite: 17]

  # Kolommen initialiseren
  t_uit$extra_foutloze_posten <- 0
  t_uit$toch_fouten <- FALSE
  t_uit$mw_fout <- NA
  t_uit$max_fout <- NA

  # ---------------------------------------------------------
  # Extra foutloze posten berekenen [cite: 17-18]
  # ---------------------------------------------------------
  for (i in 1:nrow(t_uit)) {
    t_uit$extra_foutloze_posten[i] <- foutloze_posten_equivalent(
      t_uit$ihr[i], t_uit$ibr[i], t_uit$car[i], t_uit$materialiteit[i]
    )

    # Check: Risico niet hoog maar wel fouten gevonden?
    if (!(t_uit$ihr[i] == "H" && t_uit$ibr[i] == "H" && t_uit$car[i] == "H") && t_uit$k[i] > 0) {
      t_uit$toch_fouten[i] <- TRUE
    }
  }

  # ---------------------------------------------------------
  # Monte Carlo Convolutie [cite: 18-19]
  # ---------------------------------------------------------
  set.seed(start)

  # Matrix voor krommen (rijen = MC iteraties, kolommen = strata)
  krommen <- matrix(0, nrow = MC, ncol = nrow(t_uit))

  for (i in 1:nrow(t_uit)) {
    row <- t_uit[i, ]
    k <- row$k
    n_eff <- row$n + row$extra_foutloze_posten
    w <- row$w

    if (model == "beta") {
      # [cite: 7-8] np.random.beta
      krommen[, i] <- rbeta(MC, shape1 = k + 1, shape2 = 1 + n_eff - k)

    } else if (model == "poisson") {
      # [cite: 8-9] Gamma-Poisson (frequentie)
      # Python: np.random.gamma(shape=k+1, scale=1/n_eff) / w
      # R rgamma gebruikt scale (of rate=1/scale).
      lambda_samples <- rgamma(MC, shape = k + 1, scale = 1 / n_eff)
      krommen[, i] <- lambda_samples / w

    } else if (model == "mus") {
      # [cite: 9-10] MUS: Gamma op geldinterval
      # Python: interval = w / n_eff; gamma(shape=k+1, scale=interval) / w
      interval <- w / n_eff
      lambda_samples <- rgamma(MC, shape = k + 1, scale = interval)
      krommen[, i] <- lambda_samples / w
    }
  }

  # Convolutie (Samenvoegen) [cite: 19]
  convolutie <- numeric(MC)

  if (model == "beta") {
    # Matrixvermenigvuldiging: (krommen @ w) / totaalgeld
    # In R: krommen %*% w
    convolutie <- (krommen %*% t_uit$w) / totaalgeld
  } else {
    # Else: convolutie = krommen.sum(axis=1) (voor Poisson/MUS)
    convolutie <- rowSums(krommen)
  }

  # Zorg dat convolutie een vector is (geen matrix van 1 kolom)
  convolutie <- as.vector(convolutie)

  # ---------------------------------------------------------
  # Statistieken [cite: 27-28]
  # ---------------------------------------------------------
  max_fout <- quantile(convolutie, probs = zekerheid, names = FALSE)
  mediaan <- quantile(convolutie, probs = 0.5, names = FALSE)
  gemiddelde <- mean(convolutie)

  # Modus bepalen via Density (Equivalent aan Gaussian KDE in Python)
  # Python gebruikt linspace min/max over 1000 punten [cite: 28]
  d <- density(convolutie, n = 1000, from = min(convolutie), to = max(convolutie))
  modus <- d$x[which.max(d$y)]

  # ---------------------------------------------------------
  # Vergelijkingen (Los en Als1) [cite: 29-34]
  # ---------------------------------------------------------
  mw_fout_los <- NA
  max_fout_los <- NA
  mw_fout_als1 <- NA
  max_fout_als1 <- NA

  if (vergelijk) {
    # --- LOS ---
    for (i in 1:nrow(t_uit)) {
      row <- t_uit[i, ]
      n_eff <- row$n + row$extra_foutloze_posten
      k <- row$k

      if (model == "beta") {
        # [cite: 30]
        t_uit$mw_fout[i] <- k / n_eff
        t_uit$max_fout[i] <- qbeta(zekerheid, shape1 = k + 1, shape2 = n_eff - k + 1)
      } else {
        # [cite: 31-32]
        interval <- row$w / n_eff
        t_uit$mw_fout[i] <- (k * interval) / row$w
        # Python: gamma.ppf(zekerheid, k+1, scale=interval) / row.w
        t_uit$max_fout[i] <- qgamma(zekerheid, shape = k + 1, scale = interval) / row$w
      }
    }

    mw_fout_los <- sum(t_uit$mw_fout * t_uit$w) / totaalgeld
    max_fout_los <- sum(t_uit$max_fout * t_uit$w) / totaalgeld

    # --- ALS1 (Pooled) --- [cite: 32-34]
    n_tot <- sum(t_uit$n) + sum(t_uit$extra_foutloze_posten)
    k_tot <- sum(t_uit$k)

    if (model == "beta") {
      mw_fout_als1 <- k_tot / n_tot
      max_fout_als1 <- qbeta(zekerheid, shape1 = k_tot + 1, shape2 = n_tot - k_tot + 1)
    } else {
      interval_tot <- totaalgeld / n_tot
      mw_fout_als1 <- (k_tot * interval_tot) / totaalgeld
      max_fout_als1 <- qgamma(zekerheid, shape = k_tot + 1, scale = interval_tot) / totaalgeld
    }
  }

  # ---------------------------------------------------------
  # Resultaten Formatteren
  # ---------------------------------------------------------

  # Lijst met resultaten (gelijk aan de Python output structuur)
  resultaten <- list(
    modus_fout_convolutie = modus,
    modus_fout_convolutie_geld = modus * totaalgeld,
    mediaan_fout_convolutie = mediaan,
    mediaan_fout_convolutie_geld = mediaan * totaalgeld,
    gemiddelde_fout_convolutie = gemiddelde,
    gemiddelde_fout_convolutie_geld = gemiddelde * totaalgeld,
    mw_fout_convolutie = modus, # Python code gebruikt modus als 'mw' in output
    mw_fout_convolutie_geld = modus * totaalgeld,
    max_fout_convolutie = max_fout,
    max_fout_convolutie_geld = max_fout * totaalgeld,

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
    invoer = list(model=model, zekerheid=zekerheid, MC=MC, start=start)
  )

  return(resultaten)
}
