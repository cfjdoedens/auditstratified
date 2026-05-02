# Evalueer samen de resultaten van 1 of meer steekproeven op uitgaand geld

Het samennemen van de resultaten gebeurt door convolutie van de
foutkanskrommes van de afzonderlijke steekproeven tot 1 foutkanskromme.

Waar van toepassing worden 100%-getoetste posten (het hoogstratum of
topstratum) als deterministische factoren opgeteld bij de resulterende
statistische verdeling.

We berekenen de meest waarschijnlijke en de maximale fout als fractie en
in geld.

De meest waarschijnlijke fout is de modus van de kanskromme. De maximale
fout is afhankelijk van de gevraagde zekerheid, en is de fout bij een
cumulatieve kans gelijk aan deze zekerheid.

De statistische interpretatie van de risico waarden hoog, midden en laag
voor IHR, IBR en CAR, die deze module hanteert is volgens het HARO, het
Handboek Auditing Rijksoverheid. Het HARo wordt beheerd door Auditdienst
Rijk, de ADR.

## Usage

``` r
eval_stratified(
  steekproeven,
  model = c("binomiaal", "poisson"),
  zekerheid = 0.95,
  methode = c("FFT", "FFT gelijktijdig", "direct", "Monte Carlo"),
  granulariteit = 10000,
  start = 1,
  vergelijk = TRUE
)
```

## Arguments

- steekproeven:

  Een tibble met de steekproefgegevens. Deze bestaat uit de volgende
  kolommen:

  - naam

  - waarde_laag

  - n_laag

  - k_laag

  - ihr

  - ibr

  - car

  - materialiteit

  - fout_hoog

  - goed_hoog

  - n_hoog

  - n_totaal

  - waarde_hoog

  - waarde_populatie

- model:

  Het statistische model dat gebruikt wordt. Keuze uit `"binomiaal"`
  (standaard) of `"poisson"`.

- zekerheid:

  Het zekerheidsniveau waarop we de maximale foutfractie berekenen.

- methode:

  Methode voor de berekening. Keuze uit `"FFT"` (standaard) `"direct"`,
  of `"Monte Carlo"`. `"Monte Carlo"` is bij grotere granulariteit (zeg
  boven 10.000) (veel !) sneller dan `"FFT"`. `"FFT"` is altijd (veel)
  sneller dan `"direct"` De resultaten van `"FFT"` em `"direct"`, zijn
  niet afhankelijk van de startwaarde van de randomgenerator.
  `"Monte Carlo"` is dat wel. Dit mogeljke verschil in uitkomst wordt
  echter steeds kleiner bij grotere waarden van de `granulariteit`.
  `"FFT"` wordt aanbevolen omdat dat resultaten geeft die niet
  afhankelijk zijn van de startwaarde van de toevalsgenerator.

- granulariteit:

  Bepaalt de nauwkeurigheid van de berekening. Bij `"FFT"` en `"direct"`
  is dit het aantal stappen op de kanskromme-as. Een goede waarde voor
  `"FFT"` en `"direct"` is 10.000. Bij `"Monte Carlo"` is dit het aantal
  toevalsiteraties. Voor `"Monte Carlo"` kun je makkelijk 1.000.000
  gebruiken.

- start:

  De vaste startwaarde voor de toevalsgenerator (alleen voor
  MonteCarlo). Een startwaarde van 0 betekent dat de startwaarde op de
  systeemklok, is gebaseerd, dus min of meer 'echt' op toeval is
  gebaseerd.

- vergelijk:

  TRUE of FALSE, als TRUE dan worden wat vergelijkende berekeningen
  uitgevoerd.

## Value

Een lijst, bestaande uit de convolutie-uitkomsten (fracties en geld),
eventuele vergelijkingen, en de verrijkte invoergegevens.

## Details

We gaan uit van de som van de foutfracties, de k-waarde, dus we kijken
niet naar de foutfracties per post.

De maximale fout wordt bepaald aan de hand van de resulterende
kanskromme, op basis van de gewenste zekerheid. Visueel is de maximale
fout, pm, te bepalen in een tweedimensionaal, haaks, assenstelsel. De
horizontale as, de p-as, loopt van 0 tot 1. De waarden langs die as
geven de mogeljke foutfracties weer, lopend van 0 (geen fouten) tot 1
(alles fout). De verticale as, de c-as, loopt van 0 to oneindig. Deze as
geeft de kanswaarden van de foutfracties aan. In dit assenstelsel kunnen
we de kanskromme afbeelden. Het oppervlak onder de kanskromme is 1.
Hierbij praten we over het oppervlak begrenst door de p-as, aan de
onderkant, en de verticale lijnen p = 0, en p = 1. pm is het punt op de
p-as waarbij de verticale lijn p = pm, het oppervlak onder de kanskromme
begrenst zodat links van deze lijn het oppervlak gelijk is aan de
zekerheid, bijvoorbeeld 0,95.

Aggregatie is puur op statistische gronden: namelijk risico's op fouten
boven de meest waarschijnlijke fout en op onder de meest waarschijnlijke
fout vlakken elkaar enigszins uit genomen over de meerdere steekproeven.
Dus, bij het aggregeren van de resultaten van de verschillende
steekproeven wordt geen enkele aanname gedaan over gelijkenis tussen de
eigenschappen van de afzonderlijke administraties waaruit is getrokken.
