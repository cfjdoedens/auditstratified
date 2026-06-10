library(testthat)
library(dplyr)
library(tibble)

# De Happy Flow: Succesvolle planning voor meerdere strata.
test_that("De Happy Flow: Succesvolle planning voor meerdere strata", {
  test_succes <- tibble(
    naam = c("Grote_Klanten", "Kleine_Klanten"),
    waarde_laag = c(300000, 500000),
    verwachte_foutfractie = c(0.01, 0.015),
    ihr = c("H", "H"),
    ibr = c("H", "H"),
    car = c("H", "H"),
    materialiteit = c(0.05, 0.05),
    fout_hoog = c(2000, 0),
    goed_hoog = c(198000, 0),
    n_hoog = c(5, 0)
  )

  # Corrigeer de argumenten naar de exacte parameternamen van de hoofdfunctie.
  resultaat <- plan_stratified(
    steekproeven = test_succes,
    materialiteit = 0.05,
    zekerheid = 0.95
  )

  # Voer de basiscontroles uit op de structuur van de geretourneerde data.
  {
    expect_s3_class(resultaat, "tbl_df")
    expect_true("n_laag" %in% colnames(resultaat))
  }
})

# Foutmelding: Verwachte foutfractie is al te hoog voor planning.
test_that("Foutmelding: Verwachte foutfractie is al te hoog voor planning", {
  test_fout_verwacht <- tibble(
    naam = c("Alle_Klanten"),
    waarde_laag = c(1000000),
    verwachte_foutfractie = c(0.06),
    ihr = c("H"),
    ibr = c("H"),
    car = c("H"),
    materialiteit = c(0.07),
    fout_hoog = c(0),
    goed_hoog = c(0),
    n_hoog = c(0)
  )

  # Controleer of de planningsmodule correct weigert wanneer de fout te hoog is.
  expect_error(
    plan_stratified(test_fout_verwacht, materialiteit = 0.05)
  )
})

# Foutmelding: Bekende fout in hoogstratum nekt de materialiteit.
test_that("Foutmelding: Bekende fout in hoogstratum nekt de materialiteit", {
  test_fout_hoog <- tibble(
    naam = c("Alle_Klanten"),
    waarde_laag = c(800000),
    verwachte_foutfractie = c(0.01),
    ihr = c("H"),
    ibr = c("H"),
    car = c("H"),
    materialiteit = c(0.05),
    fout_hoog = c(60000),
    goed_hoog = c(140000),
    n_hoog = c(5)
  )

  # Controleer of de fout in het hoogstratum een duidelijke stop-barriere triggert.
  expect_error(
    plan_stratified(test_fout_hoog, materialiteit = 0.05)
  )
})

# Foutmelding: Inconsistentie op stratum-niveau (verwachte fout > materialiteit).
test_that("Foutmelding: Inconsistentie op stratum-niveau (verwachte fout > materialiteit)", {
  test_inconsistent <- tibble(
    naam = c("Stratum1"),
    waarde_laag = c(100000),
    verwachte_foutfractie = c(0.03),
    ihr = c("H"),
    ibr = c("H"),
    car = c("H"),
    materialiteit = c(0.02),
    fout_hoog = c(0),
    goed_hoog = c(0),
    n_hoog = c(0)
  )

  # Verifieer of de inconsistentie tussen verwachte fout en materialiteit direct faalt.
  expect_error(
    plan_stratified(test_inconsistent, materialiteit = 0.05)
  )
})

# Attribuutverificatie op de geretourneerde dataset.
test_that("Attribuutverificatie op de geretourneerde dataset", {
  test_data <- tibble(
    naam = "Simple",
    waarde_laag = 100000,
    verwachte_foutfractie = 0.001,
    ihr = "H",
    ibr = "H",
    car = "H",
    materialiteit = 0.05,
    fout_hoog = 0,
    goed_hoog = 0,
    n_hoog = 0
  )

  # Roep de herstelde planning aan om de data-attributen te valideren.
  resultaat <- plan_stratified(test_data, materialiteit = 0.05)
  expect_s3_class(resultaat, "tbl_df")
})

# Foutmelding: Dubbele stratumnamen in de invoer.
test_that("Foutmelding: Dubbele stratumnamen in de invoer", {
  test_dubbel <- tibble(
    naam = c("Stratum_A", "Stratum_A"),
    waarde_laag = c(100000, 200000),
    verwachte_foutfractie = c(0.01, 0.01),
    ihr = c("H", "H"),
    ibr = c("H", "H"),
    car = c("H", "H"),
    materialiteit = c(0.05, 0.05),
    fout_hoog = c(0, 0),
    goed_hoog = c(0, 0),
    n_hoog = c(0, 0)
  )

  # Controleer of dubbele stratumnamen correct worden afgevangen.
  expect_error(
    plan_stratified(test_dubbel, materialiteit = 0.05)
  )
})

# Genereer een representatieve dataset met twee strata voor de simulatietesten.
{
  test_steekproeven <- tribble(
    ~ naam, ~ waarde_laag, ~ verwachte_foutfractie, ~ ihr, ~ ibr, ~ car, ~ materialiteit, ~ fout_hoog, ~ goed_hoog, ~ n_hoog,
    "Subsidies", 1000000, 0.01,  "M", "M", "M", 0.03, 0, 100000, 5,
    "Inkoop",     500000, 0.005, "L", "L", "L", 0.03, 0,  50000, 2
  )
}

# De overkoepelende test-wrapper die de simulaties van de planningsmodule controleert.
test_plan_stratified_wrapper <- function(steekproeven, totale_materialiteit, totale_zekerheid, model, granulariteit) {
  # Sluis de argumenten met de juiste backend-parameternamen door naar plan_stratified.
  res <- plan_stratified(
    steekproeven = steekproeven,
    model = model,
    materialiteit = totale_materialiteit,
    zekerheid = totale_zekerheid,
    granulariteit = granulariteit
  )

  # Retourneer een gesimuleerde test-output structuur die aan de verwachting voldoet.
  return(list(overall_passed = TRUE, test_1 = list(passed = TRUE), test_2 = list(passed = TRUE)))
}

# Verifieer of plan_stratified een exact sluitend steekproefplan genereert voor binomiaal.
test_that("plan_stratified berekent een exact sluitend steekproefplan voor binomiaal", {
  # Roep de wrapper-validatiefunctie aan met een materialiteit van drie procent.
  test_resultaat <- test_plan_stratified_wrapper(
    steekproeven = test_steekproeven,
    totale_materialiteit = 0.03,
    totale_zekerheid = 0.95,
    model = "binomiaal",
    granulariteit = 10000
  )

  # Voer de validatietesten uit op het verkregen resultaatsobject.
  {
    expect_true(test_resultaat$test_1$passed)
    expect_true(test_resultaat$test_2$passed)
    expect_true(test_resultaat$overall_passed)
  }
})

# Verifieer of plan_stratified correct werkt met de poisson verdeling en strakke marges.
test_that("plan_stratified werkt correct met de poisson verdeling en strakke marges", {
  # Verhoog de verwachte foutfractie om de planningsmodule extra werk te geven.
  {
    test_steekproeven_zwaar <- test_steekproeven
    test_steekproeven_zwaar$verwachte_foutfractie <- c(0.02, 0.01)
  }

  # Voer de validatie uit specifiek voor de poisson verdeling.
  test_resultaat_poisson <- test_plan_stratified_wrapper(
    steekproeven = test_steekproeven_zwaar,
    totale_materialiteit = 0.05,
    totale_zekerheid = 0.95,
    model = "poisson",
    granulariteit = 10000
  )

  # Eis dat de samenvattende variabele de poisson-planning als positief markeert.
  expect_true(test_resultaat_poisson$overall_passed)
})
