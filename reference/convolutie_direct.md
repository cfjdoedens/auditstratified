# Convolutie via directe, lineaire vectorberekening.

Dit is het tragere, iteratieve alternatief voor de Fast Fourier
Transform methode.

## Usage

``` r
convolutie_direct(
  t_uit,
  model,
  zekerheid,
  granulariteit,
  totaalgeld_laag,
  totaalgeld_fout_hoog,
  totaalgeld_algeheel
)
```

## Arguments

- t_uit:

  Verrijkte steekproef-tibble met n_laag, k_laag, extra_foutloze_posten,
  waarde_laag.

- model:

  "binomiaal" of "poisson".

- zekerheid:

  Zekerheidsniveau (0-1).

- granulariteit:

  Aantal toevalsiteraties.

- totaalgeld_laag:

  Totale geldswaarde van het laagstratum.

- totaalgeld_fout_hoog:

  Totale fout in het hoogstratum.

- totaalgeld_algeheel:

  Totale geldswaarde van de gehele populatie.

## Value

Een lijst met d, min_fout, max_fout, mediaan_fout, modus_fout,
gemiddelde_fout.
