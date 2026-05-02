# test plan_stratified()

Test uitgaande van de invoervelden van plan_stratified plus de
uitvoervelden of plan_stratified() juist heeft gewerkt.

## Usage

``` r
test_plan_stratified(
  steekproeven,
  totale_materialiteit,
  totale_zekerheid = 0.95,
  model = c("binomiaal", "poisson"),
  methode = c("FFT", "Monte Carlo"),
  granulariteit = 10000
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

  Het statistische model dat gebruikt wordt voor de extrapolatie. Keuze
  uit `"binomiaal"` (standaard) of `"poisson"`.

- methode:

  Rekenmethode voor evaluatie. Keuze uit `"FFT"` (standaard) of
  `"Monte Carlo"`. `"FFT"` wordt aanbevolen. Deels omdat dat ietsje
  sneller is, deels omdat dat dezelfde resultaten geeft ongeacht de
  startwaarde van de toevalsgenerator.

- granulariteit:

  Bepaalt de nauwkeurigheid van de berekening. Bij `"FFT"` is dit het
  aantal stappen op de kanskromme-as. Bij `"Monte Carlo"` is dit het
  aantal toevalsiteraties.

## Value

Lijst met antwoorden op vraag 1 en 2.

## Details

De functie test of:

1.  De max_fout inderdaad onder de materialiteit ligt

2.  Of voor elk van de strata waarvoor n_laag_extra \> 0 geldt, dat als
    je deze waarde met 1 verlaagt, en je dan eval_stratified() toepast,
    je dan boven de materialiteit uitkomt.
