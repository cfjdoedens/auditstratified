# Evalueer en selecteer de optimale strata voor de volgende parallelle klimstap

Deze functie berekent via exacte convolutie welke strata bij een
ophoging van exact 1 post de grootste foutreductie opleveren. Omdat
max_fout continu is, geeft de kleinste ophoogstap direct het optimale
sturingssignaal.

## Usage

``` r
vind_beste_strata_groep(
  huidige_strata,
  model,
  klim_granulariteit = 1e+06,
  totale_zekerheid = 0.95
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
