# Vergelijkingsmethoden: los (per stratum) en als1 (alles samengevoegd).

Vergelijkingsmethoden: los (per stratum) en als1 (alles samengevoegd).

## Usage

``` r
vergelijk_los_en_als1(
  t_uit,
  model,
  zekerheid,
  totaalgeld_laag,
  totaalgeld_fout_hoog,
  totaalgeld_algeheel
)
```

## Arguments

- t_uit:

  Verrijkte steekproef-tibble.

- model:

  "binomiaal" of "poisson".

- zekerheid:

  Zekerheidsniveau (0-1).

- totaalgeld_laag:

  Totale geldswaarde van het laagstratum.

- totaalgeld_fout_hoog:

  Totale fout in het hoogstratum.

- totaalgeld_algeheel:

  Totale geldswaarde van de gehele populatie.

## Value

Een lijst met t_uit, mw_fout_los, min_fout_los, max_fout_los,
mw_fout_als1, min_fout_als1, max_fout_als1.

## Details

Vult ook de kolommen mw_fout, min_fout en max_fout in t_uit per stratum.
