library(dplyr)
library(tibble)

# Zorg dat plan_stratified en je andere functies zijn ingeladen!
# source("R/plan_stratified.R")

# =========================================================================
# TEST 1: De Happy Flow (Succesvolle planning)
# =========================================================================
cat("\n--- TEST 1: De Happy Flow ---\n")

test_succes <- tibble(
  naam = c("Grote_Klanten", "Kleine_Klanten"),
  waarde_laag = c(300000, 500000),
  verwachte_foutfractie = c(0.01, 0.015),
  # Aangepast naar H, M of L:
  ihr = c("H", "H"),
  ibr = c("H", "H"),
  car = c("H", "H"),
  materialiteit = c(0.05, 0.05),
  fout_hoog = c(2000, 0),
  goed_hoog = c(198000, 0),
  n_hoog = c(5, 0)
)

resultaat_succes <- plan_stratified(
  steekproeven = test_succes,
  totale_materialiteit = 0.05,
  totale_zekerheid = 0.95
)

print(resultaat_succes %>% select(naam, n_basis, n_definitief, k_laag, n_totaal))
cat("Bereikte totale maximale fout:", attr(resultaat_succes, "geplande_max_fout_totaal"), "\n")


# =========================================================================
# TEST 2: Foutmelding - Verwachte fout al te hoog
# =========================================================================
cat("\n--- TEST 2: Verwachte fout te hoog ---\n")

test_fout_verwacht <- tibble(
  naam = c("Alle_Klanten"),
  waarde_laag = c(1000000),
  verwachte_foutfractie = c(0.06),
  # Aangepast naar H, M of L:
  ihr = c("H"), ibr = c("H"), car = c("H"),
  materialiteit = c(0.05)
)

tryCatch({
  plan_stratified(test_fout_verwacht, totale_materialiteit = 0.05)
}, error = function(e) {
  cat("GELUKT! Verwachte error afgevangen:\n", e$message, "\n")
})


# =========================================================================
# TEST 3: Foutmelding - Hoogstratum nekt de materialiteit
# =========================================================================
cat("\n--- TEST 3: Hoogstratum te veel fouten ---\n")

test_fout_hoog <- tibble(
  naam = c("Alle_Klanten"),
  waarde_laag = c(800000),
  verwachte_foutfractie = c(0.01),
  # Aangepast naar H, M of L:
  ihr = c("H"), ibr = c("H"), car = c("H"),
  materialiteit = c(0.05),
  fout_hoog = c(60000),
  goed_hoog = c(140000),
  n_hoog = c(5)
)

tryCatch({
  plan_stratified(test_fout_hoog, totale_materialiteit = 0.05)
}, error = function(e) {
  cat("GELUKT! Verwachte error afgevangen:\n", e$message, "\n")
})
