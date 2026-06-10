# Evalueer en selecteer de optimale strata voor de volgende parallelle klimstap

Deze functie berekent via exacte convolutie welke strata bij ophoging de
grootste foutreductie opleveren.

## Usage

``` r
vind_beste_strata_groep(
  huidige_strata,
  model,
  klim_granulariteit,
  totale_zekerheid
)
```

## Arguments

- huidige_strata:

  De actuele tibble met de status van de strata.

- model:

  Het statistische model ("binomiaal" of "poisson").

- klim_granulariteit:

  De resolutie van de kanskromme.

- totale_zekerheid:

  Het algehele zekerheidsniveau.

## Value

Een vector met indices van de strata die moeten worden opgehoogd.
