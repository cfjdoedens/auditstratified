# Plan de initiële basissteekproefomvang (Stap 1)

Deze functie berekent de minimale uitgangssituatie per stratum om
afzonderlijk onder de eigen stratum-materialiteit te blijven.

## Usage

``` r
plan_stratified_basis(steekproeven, model = c("binomiaal", "poisson"))
```

## Arguments

- steekproeven:

  Een tibble met de planningsgegevens.

- model:

  Het statistische model ("binomiaal" of "poisson").

## Value

De tibble verrijkt met de initiële n_basis en n_laag.
