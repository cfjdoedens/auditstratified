# Plan de steekproefomvang voor meerdere strata (gestratificeerde steekproef)

Deze functie berekent de optimale steekproefomvang (n) en verwachte fout
(k) voor afzonderlijke strata, zodat deze bij gezamenlijke evaluatie
onder de algehele materialiteit blijven.

## Usage

``` r
plan_stratified(
  steekproeven,
  totale_materialiteit,
  totale_zekerheid = 0.95,
  model = c("binomiaal", "poisson"),
  methode = c("FFT", "MonteCarlo"),
  max_iteraties = 1000
)
```

## Arguments

- steekproeven:

  Een tibble met de planningsgegevens.

- totale_materialiteit:

  De maximaal toegestane foutfractie voor de gehele populatie.

- totale_zekerheid:

  Het algehele zekerheidsniveau (bijv. 0.95).

- model:

  Keuze uit `"binomiaal"` (standaard) of `"poisson"`.

- methode:

  Rekenmethode voor evaluatie. `"FFT"` is sterk aanbevolen voor
  snelheid.

- max_iteraties:

  Veiligheidslimiet voor de greedy loop om vastlopen te voorkomen.

## Value

Een verrijkte tibble met de berekende `n_basis`, `n_definitief`,
`k_laag`, `n_totaal` en de uiteindelijke `geplande_max_fout_totaal` als
attribuut.
