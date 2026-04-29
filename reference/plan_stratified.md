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
  granulariteit = 10000,
  max_iteraties = 10000
)
```

## Arguments

- steekproeven:

  Een tibble met de planningsgegevens.Deze bestaat uit de volgende
  kolommen:

  - naam

  - waarde_laag

  - verwachte_foutfractie

  - ihr

  - ibr

  - car

  - materialiteit

  - fout_hoog

  - goed_hoog

  - n_hoog

- totale_materialiteit:

  De maximaal toegestane foutfractie voor de gehele populatie.

- totale_zekerheid:

  Het algehele zekerheidsniveau (bijv. 0.95).

- model:

  Het statistische model dat gebruikt wordt voor de extrapolatie. Keuze
  uit `"binomiaal"` (standaard) of `"poisson"`.

- methode:

  Rekenmethode voor evaluatie. Keuze uit `"FFT"` (standaard) of
  `"MonteCarlo"`. `"FFT"` wordt aanbevolen. Deels omdat dat ietsje
  sneller is, deels omdat dat dezelfde resultaten geeft ongeacht de
  startwaarde van de toevalsgenerator.

- granulariteit:

  Bepaalt de nauwkeurigheid van de berekening. Bij `"FFT"` is dit het
  aantal stappen op de kanskromme-as. Bij `"MonteCarlo"` is dit het
  aantal toevalsiteraties.

- max_iteraties:

  Limiet voor hoeveel extra steekproefposten we willen trekken. Dit
  beschermt ook tegen een eventuele eindeloze lus.

## Value

De tibble steekproeven verrijkt met de berekende `waarde_hoog`, \# \<-
fout_hoog + goed_hoog `waarde_populatie`, \# \<- waarde_laag +
waarde_hoog `n_basis`, \# De n nodig om per steekproef om onder de \#
materialiteit voor die steekproef te blijven. `n_definitief`, \# n_basis
plus de extra nodige steken voor \# het lage stratum om onder de totale
\# materialiteit te blijven. `k_laag`, `n_totaal` en met als attribuut
de uiteindelijke `geplande_max_fout_totaal`.
