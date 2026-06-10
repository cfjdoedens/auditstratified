# Plan de volledige optimale steekproefverdeling via parallelle convolutie-optimalisatie

Deze functie voert de volledige planningscyclus uit: het start met de
basisomvang en verhoogt daarna stapsgewijs de omvang van de meest
effectieve strata totdat de gecombineerde convolutiefout onder de
algehele materialiteit zakt.

## Usage

``` r
plan_stratified(
  steekproeven,
  model = c("binomiaal", "poisson"),
  materialiteit = NULL,
  zekerheid = 0.95,
  granulariteit = 10000,
  ...
)
```

## Arguments

- steekproeven:

  Een tibble met de planningsgegevens per stratum.

- model:

  Het statistische model ("binomiaal" of "poisson").

- materialiteit:

  De algehele materialiteitsgrens.

- zekerheid:

  Het gewenste algehele zekerheidsniveau.

- granulariteit:

  De resolutie voor de FFT-convolutieberekeningen.

- ...:

  Extra argumenten om flexibel om te gaan met 'totale_materialiteit' en
  'totale_zekerheid'.

## Value

De tibble met de definitieve, geoptimaliseerde n_laag en n_totaal.
