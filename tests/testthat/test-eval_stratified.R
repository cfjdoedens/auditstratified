test_that("voorbeeld Paul van Batenburg", {
  vb_paul_van_batenburg <- tribble(
    ~naam, ~waarde_laag, ~n_laag, ~k_laag, ~ihr, ~ibr, ~car, ~materialiteit, ~fout_hoog, ~goed_hoog, ~n_hoog, ~n_totaal, ~waarde_hoog, ~waarde_populatie,
    "populatie1", 1000000, 512, 1, "H", "H", "H", 0.01, 0, 0, 0, 512, 0, 1000000,
    "populatie2", 1000000, 106, 2, "H", "H", "H", 0.01, 0, 0, 0, 106, 0, 1000000
  )
  r <- eval_stratified(steekproeven = vb_paul_van_batenburg, zekerheid = 0.95)

  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0309)
})

test_that(
  "Uitleg over samennemen van steekproeven met
   verschillende risicoinschatting in beschrijving van eval_stratified()",
  {
    example_in_description <- tribble(
      ~naam, ~waarde_laag, ~n_laag, ~k_laag, ~ihr, ~ibr, ~car, ~materialiteit, ~fout_hoog, ~goed_hoog, ~n_hoog, ~n_totaal, ~waarde_hoog, ~waarde_populatie,
      "populatie1", 100000000, 148, 1, "H", "H", "H", 0.01, 0, 0, 0, 148, 0, 100000000,
      "populatie2", 100000000, 50, 0, "L", "L", "H", 0.01, 0, 0, 0, 50, 0, 100000000
    )
    r <-
      eval_stratified(steekproeven = example_in_description, zekerheid = 0.95)
    expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0183)
  }
)

test_that("Voorbeelden voor Niels van Leeuwen.", {
  sniels <- tribble(
    ~naam, ~waarde_laag, ~n_laag, ~k_laag, ~ihr, ~ibr, ~car, ~materialiteit, ~fout_hoog, ~goed_hoog, ~n_hoog, ~n_totaal, ~waarde_hoog, ~waarde_populatie,
    "x", 100, 300, 0, "H", "H", "H", 0.01, 0, 0, 0, 300, 0, 100,
    "y", 200, 160, 0, "H", "H", "H", 0.01, 0, 0, 0, 160, 0, 200
  )

  # Evalueer x en y samen.
  r <- eval_stratified(steekproeven = sniels, zekerheid = 0.95)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0136)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0156)

  r <- eval_stratified(steekproeven = sniels, zekerheid = 0.90)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0107)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0120)

  r <- eval_stratified(steekproeven = sniels, zekerheid = 0.85)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00909)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 3), 0.0100)

  r <- eval_stratified(steekproeven = sniels, zekerheid = 0.80)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00791)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0084)

  r <- eval_stratified(steekproeven = sniels, zekerheid = 0.55)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00453)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00418)

  r <- eval_stratified(steekproeven = sniels, zekerheid = 0.51)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00416)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00374)

  r <- eval_stratified(steekproeven = sniels, zekerheid = 0.49)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00399)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00353)

  # Hier is max_fout_convolutie > max_fout_los bij een zekerheid van 10%.
  r <- eval_stratified(steekproeven = sniels, zekerheid = 0.10)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00118)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 6), 0.0005530)
})

test_that("Drie dezelfde steekproeven.", {
  dezelfde_drie <- tribble(
    ~naam, ~waarde_laag, ~n_laag, ~k_laag, ~ihr, ~ibr, ~car, ~materialiteit, ~fout_hoog, ~goed_hoog, ~n_hoog, ~n_totaal, ~waarde_hoog, ~waarde_populatie,
    "s1", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s2", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s3", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10
  )

  # Hier is max_fout_convolutie > max_fout_los bij een zekerheid van 60%.
  r <- eval_stratified(steekproeven = dezelfde_drie, zekerheid = 0.60)
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.088)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0799)
})


test_that("32 dezelfde steekproeven.", {
  dezelfde_32 <- tribble(
    ~naam, ~waarde_laag, ~n_laag, ~k_laag, ~ihr, ~ibr, ~car, ~materialiteit, ~fout_hoog, ~goed_hoog, ~n_hoog, ~n_totaal, ~waarde_hoog, ~waarde_populatie,
    "s1", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s2", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s3", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s4", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s5", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s6", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s7", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s8", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s9", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s10", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s11", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s12", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s13", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s14", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s15", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s16", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s17", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s18", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s19", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s20", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s21", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s22", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s2",  10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10, # Hier is het extra rijtje weer terug!
    "s23", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s24", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s25", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s26", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s27", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s28", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s29", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s30", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s31", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10,
    "s32", 10, 10, 0, "H", "H", "H", 0.01, 0, 0, 0, 10, 0, 10
  )

  r <- eval_stratified(steekproeven = dezelfde_32, zekerheid = 0.51)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0831)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0628)

  r <- eval_stratified(steekproeven = dezelfde_32, zekerheid = 0.95)
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.106)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 3), 0.238)

  r <- eval_stratified(steekproeven = dezelfde_32, zekerheid = 0.70)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0899)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 3), 0.104)

  r <- eval_stratified(steekproeven = dezelfde_32, zekerheid = 0.05)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0625)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00465)
})

test_that("LNV 2023 (Wim Slot)", {
  lnv_2023_art21 <- tribble(
    ~naam, ~waarde_laag, ~n_laag, ~k_laag, ~ihr, ~ibr, ~car, ~materialiteit, ~fout_hoog, ~goed_hoog, ~n_hoog, ~n_totaal, ~waarde_hoog, ~waarde_populatie,
    "kd_beleid", 69600741, 8, 0, "H", "H", "H", 0.01, 0, 0, 0, 8, 0, 69600741,
    "lbv", 223532422, 22, 0.0331905, "H", "H", "H", 0.01, 0, 0, 0, 22, 0, 223532422,
    "inkopen", 12146914, 1, 0, "H", "H", "H", 0.01, 0, 0, 0, 1, 0, 12146914
  )

  r <- eval_stratified(steekproeven = lnv_2023_art21, zekerheid = 0.95)
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.139)
  expect_equal(round(r[["max_fout_convolutie_geld"]], 0), 42325667)

  r <- eval_stratified(steekproeven = lnv_2023_art21, zekerheid = 0.88)
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.112)
  expect_equal(round(r[["max_fout_convolutie_geld"]], 0), 34194014)
})

# --- NIEUWE TEST VOOR DE TOPSTRATUM FEATURE ---
test_that("Evaluatie met een 100%-getoetst topstratum inc. redundantie", {
  test_topstratum <- tribble(
    ~naam, ~waarde_laag, ~n_laag, ~k_laag, ~ihr, ~ibr, ~car, ~materialiteit, ~fout_hoog, ~goed_hoog, ~n_hoog, ~n_totaal, ~waarde_hoog, ~waarde_populatie,
    "stratum_met_top", 500000, 100, 1, "H", "H", "H", 0.01, 10000, 90000, 15, 115, 100000, 600000
  )

  r <- eval_stratified(steekproeven = test_topstratum, zekerheid = 0.95)

  # Omdat de Monte Carlo 'convolutie' de modus via een density curve benadert,
  # zit daar een minieme ruis op. Om de wiskunde van het topstratum zuiver te bewijzen,
  # testen we op de exact analytisch berekende 'los' variant.
  expect_equal(round(r[["vergelijk_met"]][["mw_fout_los"]], 3), 0.025)
  expect_equal(round(r[["vergelijk_met"]][["mw_fout_los_geld"]], 0), 15000)
})

