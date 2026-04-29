library(testthat)
library(dplyr)
library(tibble)

# De functies worden normaal gesproken geladen via devtools::test() of devtools::load_all().
# Als je dit script los draait, uncomment dan:
# source("R/plan_stratified.R")

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

  resultaat <- plan_stratified(
    steekproeven = test_succes,
    totale_materialiteit = 0.05,
    totale_zekerheid = 0.95
  )

  # Basiscontroles op de output structuur
  expect_s3_class(resultaat, "tbl_df")
  expect_true("n_definitief" %in% colnames(resultaat))

  # Controleer of de geplande fout onder de materialiteit ligt
  max_fout <- attr(resultaat, "geplande_max_fout_totaal")
  expect_lt(max_fout, 0.05)

  # Specifieke n_definitief verificatie (indicatief)
  expect_true(all(resultaat$n_definitief >= resultaat$n_basis))
})

test_that("Foutmelding: Verwachte foutfractie is al te hoog voor planning", {
  test_fout_verwacht <- tibble(
    naam = c("Alle_Klanten"),
    waarde_laag = c(1000000),
    verwachte_foutfractie = c(0.06),
    # Hoger dan totale materialiteit
    ihr = c("H"),
    ibr = c("H"),
    car = c("H"),
    materialiteit = c(0.07),
    # Stratum-materialiteit hoog genoeg, maar totale niet
    fout_hoog = c(0),
    goed_hoog = c(0),
    n_hoog = c(0)
  )

  expect_error(plan_stratified(test_fout_verwacht, totale_materialiteit = 0.05),
               regexp = "al groter dan of gelijk aan de totale materialiteit")
})

test_that("Foutmelding: Bekende fout in hoogstratum nekt de materialiteit",
          {
            test_fout_hoog <- tibble(
              naam = c("Alle_Klanten"),
              waarde_laag = c(800000),
              verwachte_foutfractie = c(0.01),
              ihr = c("H"),
              ibr = c("H"),
              car = c("H"),
              materialiteit = c(0.05),
              fout_hoog = c(60000),
              # 60.000 / 1.000.000 = 0.06 (nekt de 0.05 materialiteit)
              goed_hoog = c(140000),
              n_hoog = c(5)
            )

            expect_error(plan_stratified(test_fout_hoog, totale_materialiteit = 0.05),
                         regexp = "reeds bekende fout in de hoogstrata")
          })

test_that("Foutmelding: Inconsistentie op stratum-niveau (verwachte fout > materialiteit)",
          {
            test_inconsistent <- tibble(
              naam = c("Stratum1"),
              waarde_laag = c(100000),
              verwachte_foutfractie = c(0.03),
              ihr = c("H"),
              ibr = c("H"),
              car = c("H"),
              materialiteit = c(0.02),
              # Lager dan verwachte fout
              fout_hoog = c(0),
              goed_hoog = c(0),
              n_hoog = c(0)
            )

            expect_error(plan_stratified(test_inconsistent, totale_materialiteit = 0.05),
                         regexp = "verwachte foutfractie groter dan of gelijk aan de stratum-materialiteit")
          })

test_that("Attribuut 'geplande_max_fout_totaal' is aanwezig en numeriek",
          {
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

            resultaat <- plan_stratified(test_data, totale_materialiteit = 0.05)

            expect_false(is.null(attr(resultaat, "geplande_max_fout_totaal")))
            expect_true(is.numeric(attr(resultaat, "geplande_max_fout_totaal")))
          })

test_that("Foutmelding: Dubbele stratumnamen in de invoer", {
  test_dubbel <- tibble(
    naam = c("Stratum_A", "Stratum_A"),
    # Dubbele naam
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

  expect_error(plan_stratified(test_dubbel, totale_materialiteit = 0.05),
               regexp = "komen vaker dan.*n keer voor: Stratum_A")
})

# maak een representatieve dataset aan met twee strata voor de planningsmodule.
{
  test_steekproeven <- tribble(
    ~ naam,
    ~ waarde_laag,
    ~ verwachte_foutfractie,
    ~ ihr,
    ~ ibr,
    ~ car,
    ~ materialiteit,
    ~ fout_hoog,
    ~ goed_hoog,
    ~ n_hoog,
    "Subsidies",
    1000000,
    0.01,
    "M",
    "M",
    "M",
    0.03,
    0,
    100000,
    5,
    "Inkoop",
    500000,
    0.005,
    "L",
    "L",
    "L",
    0.03,
    0,
    50000,
    2
  )
}

test_that("plan_stratified berekent een exact sluitend steekproefplan voor binomiaal",
          {
            # roep de testfunctie aan met 3 procent totale materialiteit.
            test_resultaat <- test_plan_stratified(
              steekproeven = test_steekproeven,
              totale_materialiteit = 0.03,
              totale_zekerheid = 0.95,
              model = "binomiaal",
              methode = "FFT"
            )

            # verifieer of de berekende steekproef netjes onder de materialiteitsgrens blijft.
            expect_true(test_resultaat$test_1$passed)

            # controleer of de greedy loop strak genoeg is door te eisen dat 1 post minder tot falen leidt.
            expect_true(test_resultaat$test_2$passed)

            # bevestig dat de samenvattende variabele beide testen als positief markeert.
            expect_true(test_resultaat$overall_passed)
          })

test_that("plan_stratified werkt correct met de poisson verdeling en strakke marges",
          {
            # verhoog de verwachte foutfractie om de planningsmodule extra werk te geven.
            test_steekproeven_zwaar <- test_steekproeven
            test_steekproeven_zwaar$verwachte_foutfractie <- c(0.02, 0.01)

            # voer de validatie uit specifiek voor poisson.
            test_resultaat_poisson <- test_plan_stratified(
              steekproeven = test_steekproeven_zwaar,
              totale_materialiteit = 0.05,
              totale_zekerheid = 0.95,
              model = "poisson",
              methode = "FFT"
            )

            # eis dat de uiteindelijke planning robuust genoeg is voor poisson zonder de marge te overschrijden.
            expect_true(test_resultaat_poisson$overall_passed)
          })
