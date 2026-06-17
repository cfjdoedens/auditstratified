# Evalueer samen de resultaten van 1 of meer steekproeven op uitgaand geld

Het samennemen van de resultaten gebeurt door convolutie van de
foutkanskrommes van de afzonderlijke steekproeven tot 1 foutkanskromme.

Waar van toepassing worden 100%-getoetste posten (het hoogstratum of
topstratum) als deterministische factoren opgeteld bij de resulterende
statistische verdeling.

We berekenen de meest waarschijnlijke en de minimale en maximale fout
als fractie en in geld.

De meest waarschijnlijke fout is de modus van de kanskromme. De minimale
en maximale fout zijn afhankelijk van de gevraagde zekerheid. De
minimale fout is de fout bij een cumulatieve kans gelijk aan 1 - deze
zekerheid. De maximale fout is de fout bij een cumulatieve kans gelijk
aan deze zekerheid.

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
  methode = c("FFT paarsgewijs", "FFT samen", "direct", "Monte Carlo"),
  granulariteit = NULL,
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

  - waarde_hoog

  - fout_hoog

- model:

  Het statistische model dat gebruikt wordt. Keuze uit `"binomiaal"`
  (standaard) of `"poisson"`.

- zekerheid:

  Het zekerheidsniveau waarop we de maximale foutfractie berekenen.

- methode:

  Methode voor de berekening. Keuze uit:

  - `"direct"`

  - `"FFT paarsgewijs"`

  - `"FFT samen"` (standaard)

  - `"Monte Carlo"`.

  `"direct"`, `"FFT paarsgewijs"` en `"FFT samen"` zijn deterministische
  algoritmen. en zijn opvolgend meer efficiente vormen van hetzelfde
  convolutie-algoritme. `"Monte Carlo"` is niet-deterministisch, dus
  gebaseerd op toeval. Dat betekent dat het resultaat ervan afhangt van
  de startwaarde van de R toevalsgenerator, die je kunt opgeven via de
  parameter `start`.

- granulariteit:

  Bepaalt de nauwkeurigheid van de berekening. Bij `"direct"`,
  `"FFT paarsgewijs"` en `"FFT samen"`, is dit het aantal stappen op de
  kanskromme-as. Als verstekwaarden geldt voor `"direct"`,
  `"FFT paarsgewijs"` en `"FFT samen"` 100.000, en voor `"Monte Carlo"`
  10.000.000.

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

De minimale en maximale fout worden bepaald aan de hand van de
resulterende kanskromme, op basis van de gewenste zekerheid. Visueel
zijn de minimale fout, pmin, en de maximale fout, pmax, te bepalen in
een tweedimensionaal, haaks, assenstelsel. De horizontale as, de p-as,
loopt van 0 tot 1. De waarden langs die as geven de mogeljke
foutfracties weer, lopend van 0 (geen fouten) tot 1 (alles fout). De
verticale as, de c-as, loopt van 0 to oneindig. Deze as geeft de
kanswaarden van de foutfracties aan. In dit assenstelsel kunnen we de
kanskromme afbeelden. Het oppervlak onder de kanskromme is 1. Hierbij
praten we over het oppervlak begrenst door de p-as, aan de onderkant, en
de verticale lijnen p = 0, en p = 1. pmin is het punt op de p-as waarbij
de verticale lijn p = pmin, het oppervlak onder de kanskromme begrenst
zodat *rechts* van deze lijn het oppervlak gelijk is aan de zekerheid,
bijvoorbeeld 0,95. pmax is het punt op de p-as waarbij de verticale lijn
p = pmax, het oppervlak onder de kanskromme begrenst zodat *links* van
deze lijn het oppervlak gelijk is aan de zekerheid, bijvoorbeeld 0,95.

Aggregatie is puur op statistische gronden: namelijk risico's op fouten
boven de meest waarschijnlijke fout en op onder de meest waarschijnlijke
fout vlakken elkaar enigszins uit genomen over de meerdere steekproeven.
Dus, bij het aggregeren van de resultaten van de verschillende
steekproeven wordt geen enkele aanname gedaan over gelijkenis tussen de
eigenschappen van de afzonderlijke administraties waaruit is getrokken.
