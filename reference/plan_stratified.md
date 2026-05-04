# Plan de steekproefomvang voor meerdere strata (gestratificeerde steekproef)

Deze functie tracht de optimale, dat wil zeggen minimale,
steekproefomvang (n) en verwachte fout (k) voor afzonderlijke strata te
berekenen, zodat deze bij gezamenlijke evaluatie onder de algehele
materialiteit blijven.

## Usage

``` r
plan_stratified(
  steekproeven,
  totale_materialiteit,
  totale_zekerheid = 0.95,
  model = c("binomiaal", "poisson"),
  klim_methode = c("FFT", "FFT gelijktijdig", "direct", "Monte Carlo"),
  klim_granulariteit = NULL,
  validatie_methode = c("FFT", "FFT gelijktijdig", "direct", "Monte Carlo"),
  validatie_granulariteit = NULL,
  start = 1,
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

- klim_methode:

  Methode voor klimdeel van de optimalisatie. Keuze uit:

  - `"direct"`

  - `"FFT"`

  - `"FFT gelijktijdig"` (standaard)

  - `"Monte Carlo"`.

  `"direct"`, `"FFT"` en `"FFT gelijktijdig"` zijn deterministische
  algoritmen. en zijn opvolgend meer efficiente vormen van hetzelfde
  convolutie-algoritme. `"Monte Carlo"` is niet-deterministisch, dus
  gebaseerd op toeval. Dat betekent dat het resultaat ervan afhangt van
  de startwaarde van de R toevalsgenerator, die je kunt opgeven via de
  parameter `start`.

- klim_granulariteit:

  Bepaalt de nauwkeurigheid van de berekening. Bij `"direct"`, `"FFT"`
  en `"FFT gelijktijdig"`, is dit het aantal stappen op de
  kanskromme-as. Alledrie hebben ze als verstekwaarde 10.000. Bij
  `"Monte Carlo"` is dit het aantal toevalsiteraties. De verstekwaarde
  hiervoor is 1.000.000.

- validatie_methode:

  Methode voor validatie. Heeft dezelfde keuzes als bij klim_methode.

- validatie_granulariteit:

  Granulariteit voor validatie. Heeft dezelfde keuzes als bij
  klim_granulariteit. Als verstekwaarden geldt voor `"direct"`, `"FFT"`
  en `"FFT gelijktijdig"` 25.000, en voor `"Monte Carlo"` 10.000.000.

- start:

  De vaste startwaarde voor de toevalsgenerator (alleen voor Monte
  Carlo). Een startwaarde van 0 betekent dat de startwaarde op de
  systeemklok, is gebaseerd, dus min of meer 'echt' op toeval is
  gebaseerd.

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

## Details

Dat optimaliseren gaat via een heuvelklimalgoritme. En wel in drie
stappen.

Stap 1. Construeren uitgangssituatie. We bepalen per stratum het minimum
aantal steekproevenposten dat er nodig is om gezien de verwachte
foutfractie van dat stratum onder de materialiteit voor dat stratum te
komen.

Stap 2. Het eigenlijke klimmen. We verhogen steeds voor elk van de
strata de steekproefomvang met 1. We kijken dan welke verhoging de
meeste winst oplevert in de vorm van verkleining van de maximale fout.
Dat kijken doen we door de maximale fout te berekenen via een algoritme
A. Die verhoging kiezen we dan. Daar gaan we mee door tot de maximale
fout onder de materialiteit is.

Stap 3. Valideren. We valideren tenslotte eventueel de zo gevonden
verdeling van steken met een algoritme B.

Voor A en B zijn verschillende keuzen en parameters beschikbaar.

De gebruiker kan hier zijn voordeel mee doen door A zo te kiezen dat het
optimaal (snel en betrouwbaar) is voor het eigenlijke heuvelklimmen en
dat B optimaal (juist, maar mogelijk trager) is voor het bepalen dat bij
de gekozen verdeling van steken we inderdaad onder de materialiteit
uitkomen.

Achtergrond van deze architectuur is dat gedurende het heuvelklimmen de
verkeerde afslag genomen kan worden. Dat nemen van de verkeerde afslag
komt dan door numerieke ruis. De kunst is om die kans zo klein mogelijk
te houden terwijl de computationele kosten tegelijk niet de pan uit
rijzen.

Er is geprobeerd om redelijk bruikbare verstekwaarden voor A en B te
kiezen.
