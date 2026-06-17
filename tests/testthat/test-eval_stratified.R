test_that("voorbeeld Paul van Batenburg", {
  vb_paul_van_batenburg <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ n_laag,
    ~ k_laag,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ waarde_hoog,
    ~ fout_hoog,
    "populatie1",
    1000000,
    512,
    1,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "populatie2",
    1000000,
    106,
    2,
    "H",
    "H",
    "H",
    0.01,
    0,
    0
  )
  r <- eval_stratified(
    steekproeven = vb_paul_van_batenburg,
    zekerheid = 0.95,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.03100)
})

test_that(
  "Uitleg over samennemen van steekproeven met
   verschillende risicoinschatting in beschrijving van eval_stratified()",
  {
    example_in_description <- tribble(
      ~ naam,
      ~ waarde_laag,
      ~ n_laag,
      ~ k_laag,
      ~ ihr,
      ~ ibr,
      ~ car,
      ~ materialiteit,
      ~ waarde_hoog,
      ~ fout_hoog,
      "populatie1",
      100000000,
      148,
      1,
      "H",
      "H",
      "H",
      0.01,
      0,
      0,
      "populatie2",
      100000000,
      50,
      0,
      "L",
      "L",
      "H",
      0.01,
      0,
      0
    )
    r <- eval_stratified(
      steekproeven = example_in_description,
      zekerheid = 0.95,
      methode = "Monte Carlo",
      granulariteit = 1e5
    )
    expect_equal(round(r[["max_fout_convolutie"]], 4), 0.01840)
  }
)

test_that("Voorbeelden voor Niels van Leeuwen.", {
  sniels <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ n_laag,
    ~ k_laag,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ waarde_hoog,
    ~ fout_hoog,
    "x",
    100,
    300,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "y",
    200,
    160,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0
  )

  # Evalueer x en y samen (Monte Carlo met originele brute rekenkracht).
  r <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.95,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0136)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0156)

  r <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.90,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0108)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0120)

  r <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.85,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.009110)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 3), 0.0100)

  r <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.80,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00791)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0084)

  r <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.55,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.004510)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00418)

  r <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.51,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.004150)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00374)

  r <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.49,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.003980)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00353)

  # Hier is max_fout_convolutie > max_fout_los bij een zekerheid van 10%.
  r <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.10,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00118)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 6), 0.0005530)
})

test_that("Drie dezelfde steekproeven.", {
  dezelfde_drie <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ n_laag,
    ~ k_laag,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ waarde_hoog,
    ~ fout_hoog,
    "s1",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s2",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s3",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0
  )

  r <- eval_stratified(
    steekproeven = dezelfde_drie,
    zekerheid = 0.60,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.088)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0799)
})


test_that("32 dezelfde steekproeven.", {
  dezelfde_32 <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ n_laag,
    ~ k_laag,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ waarde_hoog,
    ~ fout_hoog,
    "s1",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s2",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s3",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s4",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s5",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s6",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s7",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s8",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s9",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s10",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s11",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s12",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s13",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s14",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s15",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s16",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s17",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s18",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s19",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s20",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s21",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s22",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s23",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s24",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s25",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s26",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s27",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s28",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s29",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s30",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s31",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "s32",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0
  )

  r <- eval_stratified(
    steekproeven = dezelfde_32,
    zekerheid = 0.51,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.08300)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0628)

  r <- eval_stratified(
    steekproeven = dezelfde_32,
    zekerheid = 0.95,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.106)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 3), 0.238)

  r <- eval_stratified(
    steekproeven = dezelfde_32,
    zekerheid = 0.70,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0899)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 3), 0.104)

  r <- eval_stratified(
    steekproeven = dezelfde_32,
    zekerheid = 0.05,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0622)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00465)
})

test_that("LNV 2023 (Wim Slot)", {
  lnv_2023_art21 <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ n_laag,
    ~ k_laag,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ waarde_hoog,
    ~ fout_hoog,
    "kd_beleid",
    69600741,
    8,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "lbv",
    223532422,
    22,
    0.0331905,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "inkopen",
    12146914,
    1,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0
  )

  r <- eval_stratified(
    steekproeven = lnv_2023_art21,
    zekerheid = 0.95,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.139)
  expect_equal(round(r[["max_fout_convolutie_geld"]], 0), 42394460)

  r <- eval_stratified(
    steekproeven = lnv_2023_art21,
    zekerheid = 0.88,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.112)
  expect_equal(round(r[["max_fout_convolutie_geld"]], 0), 34175149)
})

test_that("Evaluatie met een 100%-getoetst topstratum inc. redundantie", {
  test_topstratum <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ n_laag,
    ~ k_laag,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ waarde_hoog,
    ~ fout_hoog,
    "stratum_met_top",
    500000,
    100,
    1,
    "H",
    "H",
    "H",
    0.01,
    100000,
    10000
  )

  r_mc <- eval_stratified(
    steekproeven = test_topstratum,
    zekerheid = 0.95,
    methode = "Monte Carlo",
    granulariteit = 1e5
  )

  # Omdat de Monte Carlo 'convolutie' de modus via een density curve benadert,
  # zit daar een minieme ruis op. Om de wiskunde van het topstratum zuiver te bewijzen,
  # testten we voorheen op de analytisch berekende 'los' variant.
  expect_equal(round(r_mc[["vergelijk_met"]][["mw_fout_los"]], 3), 0.025)
  expect_equal(round(r_mc[["vergelijk_met"]][["mw_fout_los_geld"]], 0), 15000)

  # NIEUW: Omdat FFT wiskundig exact rekent, kunnen we nu wel rechtstreeks
  # de convolutie-uitkomst verifiëren zonder last te hebben van ruis!
  r_fft <- eval_stratified(steekproeven = test_topstratum,
                           zekerheid = 0.95,
                           methode = "FFT paarsgewijs")
  expect_equal(round(r_fft[["mw_fout_convolutie_geld"]], 0), 15000)
})

test_that("FFT methode geeft vergelijkbare resultaten als Monte Carlo", {
  sniels <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ n_laag,
    ~ k_laag,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ waarde_hoog,
    ~ fout_hoog,
    "x",
    100,
    300,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0,
    "y",
    200,
    160,
    0,
    "H",
    "H",
    "H",
    0.01,
    0,
    0
  )

  # Draai beide methodes (Monte Carlo krijgt weer de oude 1e5 iteraties voor betrouwbaarheid)
  r_mc <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.95,
    methode = "Monte Carlo",
    granulariteit = 1e5,
    start = 1
  )
  r_fft <- eval_stratified(
    steekproeven = sniels,
    zekerheid = 0.95,
    methode = "FFT paarsgewijs",
    granulariteit = 10000
  )

  # De maximale fout (convolutie) zou bij beide methodes in de basis hetzelfde
  # moeten zijn, we tolereren hier een afrondingsverschil.
  expect_equal(
    r_fft$max_fout_convolutie,
    r_mc$max_fout_convolutie,
    tolerance = 0.015
  )
})

test_that("Controleer effect van toevoegen van posten uit het hoogstratum", {
  a <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ n_laag,
    ~ k_laag,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ waarde_hoog,
    ~ fout_hoog,
    "x",
    1000,
    300,
    0,
    "H",
    "H",
    "H",
    0.01,
    1000,
    0
  )

  b <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ n_laag,
    ~ k_laag,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ waarde_hoog,
    ~ fout_hoog,
    "x",
    1000,
    300,
    0,
    "H",
    "H",
    "H",
    0.01,
    1000,
    1000
  )

  ra <- eval_stratified(steekproeven = a, methode = "FFT samen", vergelijk = FALSE)
  rb <- eval_stratified(steekproeven = b, methode = "FFT samen", vergelijk = FALSE)

  # De max_fout_convolutie moet hoger zijn wanneer fout_hoog groter is.
  expect_lt(ra$max_fout_convolutie, rb$max_fout_convolutie)

  # Het verschil moet gelijk zijn aan fout_hoog / totale_populatie.
  verwacht_verschil <- (b$fout_hoog - a$fout_hoog) / (a$waarde_laag + a$waarde_hoog)
  werkelijk_verschil <- rb$max_fout_convolutie - ra$max_fout_convolutie
  expect_equal(werkelijk_verschil, verwacht_verschil, tolerance = 1e-3)
})

# Helper-tibble voor de invoercontrole-testen.
geldige_steekproef <- tribble(
  ~ naam,
  ~ waarde_laag,
  ~ n_laag,
  ~ k_laag,
  ~ ihr,
  ~ ibr,
  ~ car,
  ~ materialiteit,
  ~ waarde_hoog,
  ~ fout_hoog,
  "test",
  1000,
  50,
  1,
  "H",
  "H",
  "H",
  0.05,
  0,
  0
)

test_that("Invoercontrole: geen tibble wordt afgewezen", {
  expect_error(eval_stratified(steekproeven = as.data.frame(geldige_steekproef)))
})

test_that("Invoercontrole: lege tibble wordt afgewezen", {
  expect_error(eval_stratified(steekproeven = geldige_steekproef[0, ]))
})

test_that("Invoercontrole: ontbrekende kolom wordt afgewezen", {
  expect_error(
    eval_stratified(steekproeven = geldige_steekproef |> dplyr::select(-k_laag)),
    "Ontbrekende kolommen.*k_laag"
  )
})

test_that("Invoercontrole: NA-waarden worden afgewezen", {
  # NA in een numerieke kolom.
  slecht <- geldige_steekproef
  slecht$waarde_laag <- NA_real_
  expect_error(eval_stratified(steekproeven = slecht))

  # NA in een tekstkolom.
  slecht2 <- geldige_steekproef
  slecht2$ihr <- NA_character_
  expect_error(eval_stratified(steekproeven = slecht2))
})

test_that("Invoercontrole: verkeerd kolomtype wordt afgewezen", {
  # Numeriek in plaats van tekst voor ihr.
  slecht <- geldige_steekproef
  slecht$ihr <- 1
  expect_error(eval_stratified(steekproeven = slecht))

  # Tekst in plaats van numeriek voor waarde_laag.
  slecht2 <- geldige_steekproef
  slecht2$waarde_laag <- "duizend"
  expect_error(eval_stratified(steekproeven = slecht2))
})

test_that("Invoercontrole: ongeldige risico-inschatting wordt afgewezen", {
  slecht <- geldige_steekproef
  slecht$ibr <- "X"
  expect_error(eval_stratified(steekproeven = slecht))
})

test_that("Invoercontrole: negatieve waarden worden afgewezen", {
  # Negatieve waarde_laag.
  slecht <- geldige_steekproef
  slecht$waarde_laag <- -100
  expect_error(eval_stratified(steekproeven = slecht))

  # Negatieve n_laag.
  slecht2 <- geldige_steekproef
  slecht2$n_laag <- -1
  expect_error(eval_stratified(steekproeven = slecht2))

  # Negatieve fout_hoog.
  slecht3 <- geldige_steekproef
  slecht3$fout_hoog <- -5
  expect_error(eval_stratified(steekproeven = slecht3))
})

test_that("Invoercontrole: k_laag groter dan n_laag wordt afgewezen", {
  slecht <- geldige_steekproef
  slecht$k_laag <- 100
  slecht$n_laag <- 50
  expect_error(eval_stratified(steekproeven = slecht))
})

test_that("Invoercontrole: fout_hoog groter dan waarde_hoog wordt afgewezen", {
  slecht <- geldige_steekproef
  slecht$fout_hoog <- 500
  slecht$waarde_hoog <- 100
  expect_error(eval_stratified(steekproeven = slecht))
})

test_that("Invoercontrole: materialiteit buiten bereik wordt afgewezen", {
  # Materialiteit van 0.
  slecht <- geldige_steekproef
  slecht$materialiteit <- 0
  expect_error(eval_stratified(steekproeven = slecht))

  # Materialiteit groter dan 1.
  slecht2 <- geldige_steekproef
  slecht2$materialiteit <- 1.5
  expect_error(eval_stratified(steekproeven = slecht2))
})

test_that("Invoercontrole: dubbele stratumnamen worden afgewezen", {
  dubbel <- dplyr::bind_rows(geldige_steekproef, geldige_steekproef)
  expect_error(
    eval_stratified(steekproeven = dubbel),
    "stratumnamen"
  )
})

test_that("Invoercontrole: lege stratumnaam wordt afgewezen", {
  slecht <- geldige_steekproef
  slecht$naam <- ""
  expect_error(eval_stratified(steekproeven = slecht))
})
