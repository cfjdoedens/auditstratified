test_that("voorbeeld Paul van Batenburg", {
  vb_paul_van_batenburg <- tribble(
    ~naam,
    ~w,
    ~n,
    ~k,
    ~ihr,
    ~ibr,
    ~car,
    ~materialiteit,
    "populatie1",
    1000000,
    512,
    1,
    "H",
    "H",
    "H",
    0.01,
    "populatie2",
    1000000,
    106,
    2,
    "H",
    "H",
    "H",
    0.01
  )
  r <- eval_stratifiedp(steekproeven = vb_paul_van_batenburg, zekerheid = 0.95)

  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0309)
})

test_that(
  "Uitleg over samennemen van steekproeven met
   verschillende risicoinschatting in beschrijving van eval_stratifiedp()",
  {
    # Bijvoorbeeld:
    # Steekproef1 is gebaseerd op een zekerheid van 95% omdat
    # ihr, ibr en car alledrie op hoog (H) staan.
    # De materialiteit is 2%.
    # Het betreft 100 miljoen euro.
    # Voor steekproef1 trekken we 148 posten, waarbij 1 fout blijkt.
    # Steekproef2 is gebaseerd op een zekerheid van 64% omdat
    # ihr en ibr allebei op laag staan en alleen car op hoog.
    # Het betreft ook 100 miljoen euro en een materialiteit van 2%.
    # Voor steekproef2 trekken we 50 posten waarvan er 0 fout blijken.
    #
    # Bij verschil in risicoinschatting van de massa's waarover
    # wordt gestoken worden de lagere risicoinschattingen vertaald naar
    # extra getrokken foutloze posten.
    # In ons voorbeeld bepalen we voor steekproef2 het aantal foutloze
    # posten nodig om een positieve uitspraak te doen bij 64% en bij 95%.
    # Dit zijn respectievelijk 50 en 148.
    # Het verschil is 148-50 = 98 posten.
    # Daarna berekenen we totale maximale fout op basis van
    # een zekerheid van 95%,
    # en steekproef1, 148 posten waarvan 1 fout,
    # en steekproef2, met 50 + 98 posten, waarvan 0 fout.
    # De maximale fout is dan 1,83% ofwel ongeveer3.660.000.
    # De meest waarschijnlijke fout 0,52% ofwel ongeveer 1.160.000 euro.
    example_in_description <- tribble(
      ~naam,
      ~w,
      ~n,
      ~k,
      ~ihr,
      ~ibr,
      ~car,
      ~materialiteit,
      "populatie1",
      100000000,
      148,
      1,
      "H",
      "H",
      "H",
      0.01,
      "populatie2",
      100000000,
      50,
      0,
      "L",
      "L",
      "H",
      0.01
    )
    r <-
      eval_stratifiedp(steekproeven = example_in_description, zekerheid = 0.95)
    expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0183)
  }
)

test_that("Voorbeelden voor Niels van Leeuwen.", {
  sniels <- tribble(
    ~naam,
    ~w,
    ~n,
    ~k,
    ~ihr,
    ~ibr,
    ~car,
    ~materialiteit,
    "x",
    100,
    300,
    0,
    "H",
    "H",
    "H",
    0.01,
    "y",
    200,
    160,
    0,
    "H",
    "H",
    "H",
    0.01
  )

  # Evalueer x en y samen.
  r <- eval_stratifiedp(steekproeven = sniels, zekerheid = 0.95)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0136)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0156)

  r <- eval_stratifiedp(steekproeven = sniels, zekerheid = 0.90)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0107)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0120)

  r <- eval_stratifiedp(steekproeven = sniels, zekerheid = 0.85)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00909)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 3), 0.0100)

  r <- eval_stratifiedp(steekproeven = sniels, zekerheid = 0.80)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00791)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0084)

  r <- eval_stratifiedp(steekproeven = sniels, zekerheid = 0.55)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00453)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00418)

  r <- eval_stratifiedp(steekproeven = sniels, zekerheid = 0.51)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00416)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00374)

  r <- eval_stratifiedp(steekproeven = sniels, zekerheid = 0.49)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00399)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00353)

  # Hier is max_fout_convolutie > max_fout_los bij een zekerheid van 10%.
  r <- eval_stratifiedp(steekproeven = sniels, zekerheid = 0.10)
  expect_equal(round(r[["max_fout_convolutie"]], 5), 0.00118)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 6), 0.0005530)
})

test_that("Drie dezelfde steekproeven.", {
  dezelfde_drie <- tribble(
    ~naam,
    ~w,
    ~n,
    ~k,
    ~ihr,
    ~ibr,
    ~car,
    ~materialiteit,
    "s1",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s2",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s3",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01
  )

  # Hier is max_fout_convolutie > max_fout_los bij een zekerheid van 60%.
  r <- eval_stratifiedp(steekproeven = dezelfde_drie, zekerheid = 0.60)
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.088)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0799)
})

test_that("32 dezelfde steekproeven.", {
  # skip("Costs too much time. Works fine :-)")
  dezelfde_32 <- tribble(
    ~naam,
    ~w,
    ~n,
    ~k,
    ~ihr,
    ~ibr,
    ~car,
    ~materialiteit,
    "s1",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s2",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s3",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s4",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s5",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s6",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s7",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s8",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s9",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s10",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s11",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s12",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s13",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s14",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s15",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s16",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s17",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s18",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s19",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s20",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s21",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s22",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s2",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s23",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s24",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s25",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s26",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s27",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s28",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s29",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s30",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s31",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01,
    "s32",
    10,
    10,
    0,
    "H",
    "H",
    "H",
    0.01
  )

  # Hier is max_fout_convolutie > max_fout_los bij een zekerheid van 51%.
  r <- eval_stratifiedp(steekproeven = dezelfde_32, zekerheid = 0.51)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0831)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 4), 0.0628)

  r <- eval_stratifiedp(steekproeven = dezelfde_32, zekerheid = 0.95)
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.106)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 3), 0.238)

  r <- eval_stratifiedp(steekproeven = dezelfde_32, zekerheid = 0.70)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0899)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 3), 0.104)

  # Hier is max_fout_convolutie > max_fout_los bij een zekerheid van 5%.
  r <- eval_stratifiedp(steekproeven = dezelfde_32, zekerheid = 0.05)
  expect_equal(round(r[["max_fout_convolutie"]], 4), 0.0625)
  expect_equal(round(r[["vergelijk_met"]][["max_fout_los"]], 5), 0.00465)
})

test_that("LNV 2023 (Wim Slot)", {
  lnv_2023_art21 <- tribble(
    ~naam,
    ~w,
    ~n,
    ~k,
    ~ihr,
    ~ibr,
    ~car,
    ~materialiteit,
    "kd_beleid",
    69600741,
    8,
    0,
    "H",
    "H",
    "H",
    0.01,
    "lbv",
    223532422,
    22,
    0.0331905,
    "H",
    "H",
    "H",
    0.01,
    "inkopen",
    12146914,
    1,
    0,
    "H",
    "H",
    "H",
    0.01
  )

  r <- eval_stratifiedp(steekproeven = lnv_2023_art21, zekerheid = 0.95)
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.139)
  expect_equal(round(r[["max_fout_convolutie_geld"]], 0), 42325667)

  r <- eval_stratifiedp(steekproeven = lnv_2023_art21, zekerheid = 0.88)
  expect_equal(round(r[["max_fout_convolutie"]], 3), 0.112)
  expect_equal(round(r[["max_fout_convolutie_geld"]], 0), 34194014)
})
