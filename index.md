# auditstratified

- [auditstratified](#auditstratified)
  - [Installatie](#installatie)
  - [Planning en Evaluatie van gestratificeerde monetaire
    steekproeven](#planning-en-evaluatie-van-gestratificeerde-monetaire-steekproeven)
  - [Twee statistische modellen: binomiaal en
    Poisson](#twee-statistische-modellen-binomiaal-en-poisson)
  - [Keuze voor convolutie](#keuze-voor-convolutie)
  - [Vier rekenmethoden voor
    convolutie](#vier-rekenmethoden-voor-convolutie)
  - [`eval_stratified()`: Evaluatie van meerdere
    steekproeven](#eval_stratified-evaluatie-van-meerdere-steekproeven)
    - [Verfijning: Hoogstratum en
      Laagstratum](#verfijning-hoogstratum-en-laagstratum)
    - [Voorbeelden van verschillende
      evaluatie-scenario’s](#voorbeelden-van-verschillende-evaluatie-scenarios)
    - [De rol van risicofactoren, materialiteit en “virtuele”
      posten](#de-rol-van-risicofactoren-materialiteit-en-virtuele-posten)
    - [De data-input: Het opbouwen van de
      steekproeven-tibble](#de-data-input-het-opbouwen-van-de-steekproeven-tibble)
    - [Verklaring van de verplichte
      kolommen](#verklaring-van-de-verplichte-kolommen)
    - [In de praktijk: Rekenen met risico’s en virtuele
      posten](#in-de-praktijk-rekenen-met-risicos-en-virtuele-posten)
  - [`plan_stratified()`: Plannen van meerdere
    steekproeven](#plan_stratified-plannen-van-meerdere-steekproeven)
    - [Algoritme](#algoritme)
  - [Webversie](#webversie)
  - [Referenties](#referenties)
    - [Wikipedia: convolutie](#wikipedia-convolutie)
    - [JCGM 101:2008. Propagation of distributions using a Monte Carlo
      method.](#jcgm-1012008-propagation-of-distributions-using-a-monte-carlo-method)
    - [Wikipedia: Monte Carlo methode](#wikipedia-monte-carlo-methode)
    - [Stevens 2024](#stevens-2024)
    - [MathWorks: Fast Fourier Transform
      (FFT)](#mathworks-fast-fourier-transform-fft)
    - [Abate & Whitt 1992](#abate--whitt-1992)
    - [Wang 1988](#wang-1988)
  - [Ideeën voor verdere
    ontwikkeling](#idee%C3%ABn-voor-verdere-ontwikkeling)

## Installatie

Je kunt de ontwikkelversie van `auditstratified` laden van
[GitHub](https://github.com/):

``` r

if (!requireNamespace("devtools", quietly = TRUE)) {
install.packages("devtools")
}
devtools::install_github("cfjdoedens/auditstratified")
```

## Planning en Evaluatie van gestratificeerde monetaire steekproeven

Dit R-pakket ondersteunt een audit waarbij meerdere steekproeven in
combinatie worden gebruikt. We noemen zo’n combinatie van meerdere
steekproeven ook wel een gestratificeerde steekproef. Het idee is dat er
een aantal afzonderlijke monetaire massa’s zijn die we om wat voor reden
dan ook ieder afzonderlijk controleren door middel van een monetaire
steekproef. De aanname is dat de er geen samenhang in foutkans is tussen
de afzonderlijke steekproeven. Elke afzonderlijk gecontroleerde massa
noemen we een stratum. Bij elkaar gaat het dan om meerdere strata.

Voor het plannen van de steekproeven is er de functie
`plan_stratified()`. Voor het evalueren is er de functie
[`eval_stratified()`](https://cfjdoedens.github.io/auditstratified/reference/eval_stratified.md).

Hoewel in de auditpraktijk je in het ideale scenario eerst plant, en dan
pas evalueert, zullen we hieronder eerst
[`eval_stratified()`](https://cfjdoedens.github.io/auditstratified/reference/eval_stratified.md)
bespreken, en daarna `plan_stratified()`. Dit omdat `plan_stratified()`
het best uitgelegd kan worden in termen van
[`eval_stratified()`](https://cfjdoedens.github.io/auditstratified/reference/eval_stratified.md).

## Twee statistische modellen: binomiaal en Poisson

`auditstratified` biedt de mogelijkheid om twee verschillende
statistische modellen toe te passen. De keuze is tussen:

- binomiaal: Dit is het meest nauwkeurige model.
- Poisson: Dit wordt traditioneel veel gebruikt in de audit. Maar het is
  minder nauwkeurig, en bij grote foutfracties, wordt het erg
  onnauwkeurig.

## Keuze voor convolutie

Voor het samen evalueren van meerdere steekproeven bestaan diverse
methoden. De meeste exacte daarvan is convolutie van de kanskromme
gevolgd door het bepalen van de maximale fout daarvan. Daarom gebruikt
dit pakket convolutie.

In de literatuur zijn verschillende algoritmen voor convolutie
beschreven. Vier van deze algoritmen zijn geimplementeerd in
`auditstratified`.

## Vier rekenmethoden voor convolutie

`auditstratified` biedt vier verschillende numerieke benaderingen om de
convolutie (het wiskundig samenvoegen van de kanskrommes van de
afzonderlijke steekproeven) uit te voeren.

Waarom worden er verschillende methoden aangeboden? Dit is vooral om
meer vetrouwen te geven in het resultaat van de convolutie: elk van de
methode levert - afgezien van kleine rekenkundige verschillen, en
statistische ruis bij de Monte Carlo methode (zie hieronder) - hetzelfde
resultaat op, alleen de snelheid van de berekening is verschillend.

- **direct** Een numerieke benadering die de as met mogelijke fouten in
  euro’s opdeelt in een eindig, discreet raster (grid). De kanskrommen
  worden vervolgens via de stappen op dit raster met elkaar
  gecombineerd. Dit gebeurt in paren van kanskrommen. De methode is een
  directe implementatie van wat convolutie wiskundig betekent. Zie
  <https://nl.wikipedia.org/wiki/Convolutie>.
- **Fast Fourier Transformatie (FFT) paarsgewijs** Ook een numerieke
  benadering die de as met mogelijke fouten in euro’s opdeelt in een
  eindig, discreet raster (grid). De kanskrommen worden hierbij omgezet
  in een frequentieverdeling. Dit gebeurt in paren van kanskrommen,
  totdat alle kanskrommen zijn samengenomen. Zie
  <https://nl.wikipedia.org/wiki/Fast_Fourier_transform>.
- **Fast Fourier Transformatie (FFT) samen** Zelfde als vorige methode,
  maar nu worden alle kanskrommen achter elkaar op 1 raster geplaatst,
  en als 1 geheel gecombineerd. Dit is meer efficient en betrouwbaarder.
- **Monte Carlo:** Een numerieke benadering die de resulterende
  verdeling opbouwt door het trekken van een groot aantal
  (pseudo-)willekeurige waarden uit de kanskrommen. Dit gaat niet
  paarsgewijs, maar samen over alle kanskrommen. Zie
  <https://nl.wikipedia.org/wiki/Monte-Carlosimulatie>.

De parameter **granulariteit** slaat bij de eerste drie methoden op het
aantal elementen in het raster: een hogere granulariteit betekent een
fijner raster. Bij de vierde methode staat de granulariteit voor het
aantal getrokken toevalsgetallen.

**Vergelijking:** De eerste drie methoden kunnen beschouwd worden als
opvolgend steeds efficientere algortmen. Dus FFT samen is hiervan het
meest efficient. De Monet Carlo methode is heel grofweg gesproken even
efficient. Echter, omdat deze methode gebaseerd is op generatie van
toevalsgetallen, en de uitkomst er van dus statistische ruis bevat, is
FFT samen de beste keuze.

**Toepassing:**
[`eval_stratified()`](https://cfjdoedens.github.io/auditstratified/reference/eval_stratified.md)
biedt deze keuze tussen de vier verschillende convolutieberekeningen.
Nogmaals, dit is vooral om het vertrouwen te bevorderen in de convolutie
zoals berekend door het pakket auditstratified.

Bij `plan-startified()` is er geen keuze. Daar wordt altijd de meest
efficiente en stabiele methode, namelijk FFT samen gebruikt.

## `eval_stratified()`: Evaluatie van meerdere steekproeven

[`eval_stratified()`](https://cfjdoedens.github.io/auditstratified/reference/eval_stratified.md)
evalueert samen meerdere, afzonderlijke, steekproeven. Dit is nodig in
de auditpraktijk wanneer een totale populatie bestaat uit verschillende
deelpopulaties (bijvoorbeeld verschillende locaties, systemen of
processen) waaruit separate steekproeven zijn getrokken.

In het meest basale scenario bevatten deze steekproeven geen
uitzonderlijk grote posten (alle gecontroleerde posten zijn kleiner dan
het steekproefinterval). In dit geval zijn er dus geen posten die
integraal (100%) gecontroleerd hoeven te worden; de zogenaamde
‘hoogstrata’ zijn leeg (ofwel € 0).

Wanneer je de resultaten van deze losse steekproeven simpelweg bij
elkaar op zou tellen, zou je de statistische onzekerheid verkeerd
inschatten. `eval_stratified` lost dit op door de afzonderlijke
kansverdelingen van de verschillende steekproeven te combineren door
middel van **convolutie**. Het resultaat is één gecombineerde
kanskromme. Hieruit kan een minimale en een maximale fout(fractie)
worden berekend.

### Verfijning: Hoogstratum en Laagstratum

In de praktijk komt het voor dat bestanden wél enkele zeer grote posten
bevatten die integraal gecontroleerd worden. Om hier wiskundig correct
mee om te gaan, biedt `auditstratified` een verfijning op bovenstaand
basisprincipe: het onderscheid tussen een **hoogstratum** en een
**laagstratum** binnen de steekproeven. Het hoogstratum bevat de
integraal gecontroleerde posten, deze zijn niet geextrapoleerd bij de
steekproefevaluatie. Het laagstrum bevat de posten kleiner dan het
steekproefinterval. Deze posten zijn geextrapoleerd en leveren samen een
kanskromme die beschrijft wat de kans is op de mogeljke foutfractie in
de massa van posten kleiner dan het steekproefinterval.

### Voorbeelden van verschillende evaluatie-scenario’s

Om de flexibiliteit van `auditstratified` te demonstreren, schetsen we
hieronder drie veelvoorkomende praktijksituaties en hoe het package
hiermee omgaat. Het is hierbij belangrijk om te onthouden dat de
uiteindelijke evaluatie een *foutfractie* is: de totale (geprojecteerde)
fout gedeeld door de totale populatieomvang.

**Scenario 1: Geen fouten in de integraal gecontroleerde posten (Het
verdunningseffect)** We evalueren twee steekproeven die beide bestaan
uit een laagstratum (de getrokken steekproef) en een hoogstratum (de
100% gecontroleerde grote posten). In de steekproeven (laagstrata)
zitten enkele fouten, maar in de hoogstrata van beide bestanden worden
*geen* fouten aangetroffen (absolute fout = € 0). *Hoe `auditstratified`
dit verwerkt:* Het package berekent via convolutie de gecombineerde
onzekerheid en geprojecteerde fout van de twee laagstrata. Omdat de fout
in de hoogstrata € 0 is, wordt de teller (de fout) niet verhoogd, maar
de noemer (de totale populatieomvang) wél. Het foutloze hoogstratum
‘verdunt’ hiermee de fout, waardoor de uiteindelijke maximale
foutfractie voor het geheel significant daalt.

**Scenario 2: Wel fouten in de integraal gecontroleerde posten** We
nemen dezelfde opzet als hierboven, maar nu worden er wél fouten
gevonden in de grote posten. In het hoogstratum van steekproef 1 vinden
we een werkelijke fout van € 15.000, en in steekproef 2 een fout van €
5.000. *Hoe `auditstratified` dit verwerkt:* De statistische onzekerheid
van de laagstrata wordt opnieuw zuiver via convolutie gecombineerd.
Vervolgens telt het package de absolute, vastgestelde fouten uit de
hoogstrata (samen € 20.000) direct op bij de uitkomst van de convolutie
(de teller), en deelt dit door de totale omvang van alle strata (de
noemer). De foutfractie stijgt hierdoor exact in verhouding met de
gevonden harde fouten, zonder onterechte extra statistische opslag.

**Scenario 3: Een volledige populatie wordt integraal gecontroleerd**
Het package kan ook overweg met asymmetrische controlesituaties. Stel,
bestand 1 wordt regulier gecontroleerd via een steekproef (laagstratum).
Bestand 2 wordt door de auditor echter *volledig* (100%) gecontroleerd.
Hierbij wordt in totaal voor € 12.500 aan fouten gevonden in bestand 2.
*Hoe `auditstratified` dit verwerkt:* Bestand 2 heeft door de 100%
controle geen statistische onzekerheid meer. De gebruiker van het
package geeft daarom voor het laagstratum een omvang van 0 euro op, en
de 100% populatie wordt door de gebruiker in de parameters fout_hoog en
goed_hoog geplaatst. Dus het integraal gecontroleerde bestand wordt dan
door het package in zijn geheel behandeld als één groot ‘hoogstratum’.
`auditstratified` berekent de verdeling van bestand 1, slaat de
convolutie voor bestand 2 over, en voegt de omvang én de exacte fout van
€ 12.500 van bestand 2 direct toe aan de breuk voor de algehele
eindevaluatie.

### De rol van risicofactoren, materialiteit en “virtuele” posten

Wanneer bestanden uit verschillende bedrijfsonderdelen of processen
komen, worden ze zelden met exact hetzelfde risicoprofiel gecontroleerd.
`auditstratified` is speciaal ontworpen om steekproeven met
verschillende risico-inschattingen toch wiskundig zuiver met elkaar te
kunnen combineren. Om te begrijpen hoe het package dit doet, is het
belangrijk te kijken naar de wisselwerking tussen risicofactoren,
materialiteit en zogenaamde “virtuele foutloze posten”.

**De basis: Risico en Materialiteit** In de auditpraktijk wordt de
benodigde statistische zekerheid voor een steekproef bepaald door de
inschatting van het Inherent Risico (IHR), het Interne Beheersingsrisico
(IBR) en het risico van Cijferbeoordelingen (CAR).

- Staan al deze risicofactoren op “Hoog”, dan leunt de accountant
  volledig op de steekproef en is er bijvoorbeeld 95% zekerheid nodig.
- Is het IBR “Laag” (bijvoorbeeld door een sterke interne controle), dan
  mag de vereiste zekerheid uit de steekproef omlaag (bijv. naar 64%).

**HARo** De statistische interpretatie van de risico waarden hoog,
midden en laag voor IHR, IBR en CAR, die deze module hanteert is volgens
het HARO, het Handboek Auditing Rijksoverheid. Het HARo wordt beheerd
door Auditdienst Rijk, de ADR.

De vereiste zekerheid bepaalt, in combinatie met de vastgestelde
**materialiteit** (de maximaal acceptabele fout in de populatie),
hoeveel posten er fysiek getrokken moeten worden. Bij een lager risico
is de steekproefomvang kleiner.

**Het concept van “Virtuele Foutloze Posten”** Wiskundig gezien kun je
een verlaagd risico (bijv. steunen op interne controles) op een zeer
elegante manier vertalen. Het feit dat de interne controle effectief is
en een deel van het risico afdekt, fungeert als statistisch bewijs. Je
kunt dit zien alsof de interne systemen vooraf al een aantal posten
foutloos voor je hebben gecontroleerd.

Dit noemen we **virtuele foutloze steekproefposten**. Je fysieke
steekproefomvang wordt dan weliswaar kleiner, maar wiskundig gezien
wordt jouw bewijslast ‘aangevuld’ door de virtuele foutloze posten die
voortkomen uit je positieve risicoanalyse.

**Verschillende steekproeven combineren via harmonisatie** Dit concept
vormt het wiskundige fundament onder `auditstratified` bij het
combineren van verschillende steekproeven:

- **Steekproef A (Hoog risico):** Vereist 95% zekerheid. Er is geen
  steun op interne controles, dus er zijn **0** virtuele posten. Het
  benodigde bewijs moet 100% uit de getrokken posten komen.
- **Steekproef B (Laag risico):** Vereist slechts 64% zekerheid. De
  sterke interne controle levert een flinke buffer aan **virtuele
  foutloze posten** op. Er hoeven fysiek veel minder posten getrokken te
  worden.

Als je de resultaten van Steekproef A en B wilt samenvoegen, kun je niet
zomaar de gevonden fouten en getrokken aantallen bij elkaar optellen;
het statistische vertrekpunt is immers ongelijk.

`auditstratified` lost dit op door bij de evaluatie voor élke
afzonderlijke steekproef het totale bewijs te harmoniseren (fysiek
getrokken posten + virtuele foutloze posten op basis van de opgegeven
IHR/IBR/CAR-parameters en materialiteit). Pas nadat de kansverdelingen
via deze virtuele posten op hetzelfde fundamentele vertrekpunt zijn
gebracht, past het package de convolutie toe. Hierdoor weegt de vooraf
verkregen zekerheid per bestand zuiver en proportioneel mee in de
berekende maximale fout voor de gehele populatie.

### De data-input: Het opbouwen van de steekproeven-tibble

Om de functie
[`eval_stratified()`](https://cfjdoedens.github.io/auditstratified/reference/eval_stratified.md)
te kunnen gebruiken, moet je de resultaten van je steekproeven
aanleveren in een specifieke dataframe, een zogenaamde `tibble`. Elke
rij in deze tabel representeert één afzonderlijk te evalueren bestand of
steekproef.

Het package is streng op de invoer om wiskundige fouten te voorkomen: er
wordt gecontroleerd op redundante totaalvelden. Zo moet `n_totaal`
bijvoorbeeld exact gelijk zijn aan `n_laag + n_hoog`.

Hier is een voorbeeld van hoe je deze invoer-tibble opbouwt in R,
gebaseerd op de twee eerder genoemde steekproeven:

``` r

library(tibble)
library(auditstratified)

mijn_audit_data <- tribble(
  ~naam,          ~waarde_laag, ~n_laag, ~k_laag, ~ihr, ~ibr, ~car, ~materialiteit, ~fout_hoog, ~goed_hoog, ~n_hoog, ~n_totaal, ~waarde_hoog, ~waarde_populatie,
  # Steekproef 1: Hoog risico (HHH), 1 fout in de steekproef, en € 15.000 fout in het hoogstratum
  "Steekproef 1", 85000000,     148,     1,       "H",  "H",  "H",  0.02,           15000,      14985000,   25,      173,       15000000,     100000000,
  
  # Steekproef 2: Laag risico (LLH), 0 fouten in de steekproef, en € 5.000 fout in het hoogstratum
  "Steekproef 2", 90000000,     50,      0,       "L",  "L",  "H",  0.02,           5000,       9995000,    10,      60,        10000000,     100000000
)

# Voer de gezamenlijke evaluatie uit
resultaat <- eval_stratified(steekproeven = mijn_audit_data, zekerheid = 0.95)
```

### Verklaring van de verplichte kolommen

- naam: Een herkenbare naam voor het bestand of de steekproef.

- Laagstratum (de steekproef):

  ``` R
    waarde_laag: De totale boekwaarde in euro's waaruit de steekproef is getrokken.
    n_laag: Het aantal fysiek gecontroleerde posten in deze steekproef.
    k_laag: De som van de gevonden foutfracties (bijv. 1 post volledig fout = 1.0, 1 post voor de helft fout = 0.5).
  ```

- Risico & Zekerheid:

  ``` R
    ihr, ibr, car: De risico-inschattingen (kies uit "H", "M", of "L").
    materialiteit: De gehanteerde materialiteit als fractie (bijv. 0.02 voor 2%).
  ```

- Hoogstratum (100% controle):

  ``` R
    fout_hoog: Het in euro's gevonden foutbedrag in de integraal gecontroleerde posten.
    goed_hoog: Het in euro's goedgekeurde bedrag in dit stratum.
    n_hoog: Het aantal posten dat in dit stratum is gecontroleerd.
  ```

- Totaalcontroles (Redundantie voor veiligheid):

  ``` R
    n_totaal: Totale aantal gecontroleerde posten (n_laag + n_hoog).
    waarde_hoog: Totale boekwaarde van het hoogstratum (fout_hoog + goed_hoog).
    waarde_populatie: De absolute totale boekwaarde (waarde_laag + waarde_hoog).
  ```

### In de praktijk: Rekenen met risico’s en virtuele posten

`auditstratified` bevat twee hulpfuncties waarmee je deze theoretische
stappen zelf inzichtelijk kunt maken en kunt narekenen:
[`haro_nog_nodige_zekerheid()`](https://cfjdoedens.github.io/auditstratified/reference/haro_nog_nodige_zekerheid.md)
en
[`foutloze_posten_equivalent()`](https://cfjdoedens.github.io/auditstratified/reference/foutloze_posten_equivalent.md).

**1. De benodigde zekerheid berekenen** Stel, je hebt een deelpopulatie
met een Laag Inherent Risico (L), een Midden Intern Beheersingsrisico
(M) en een Hoog Cijferanalyserisico (H). Je kunt de exact vereiste
statistische zekerheid volgens het HARo-model direct opvragen met de
volgende functie:

``` r

library(auditstratified)

# Bereken de benodigde zekerheid voor IHR=L, IBR=M, CAR=H
zekerheid <- haro_nog_nodige_zekerheid(ihr = "L", ibr = "M", car = "H")

print(zekerheid)
#> [1] 0.7596154
# Output: 0.8015873 (ofwel ~80,2% vereiste zekerheid in plaats van de standaard 95%)
```

**2. Virtuele foutloze posten berekenen** Omdat de vereiste zekerheid
door de risicoanalyse is gedaald van 95% naar ~80,2%, hoef je fysiek
aanzienlijk minder posten te trekken. Het verschil in benodigd bewijs
tussen een reguliere 95%-steekproef en deze 80%-steekproef wordt
wiskundig opgevuld door virtuele foutloze posten. Bij een materialiteit
van bijvoorbeeld 2% (0.02) bereken je dit als volgt:

``` r


# Bereken het equivalente aantal foutloze posten
virtuele_posten <- foutloze_posten_equivalent(ihr = "L", ibr = "M", car = "H", materialiteit = 0.02)

print(virtuele_posten)
#> [1] 78
# Output: Geeft het exacte aantal posten terug dat de effectieve interne controle 
# "virtueel" voor je heeft afgevinkt.
```

Wanneer je de hoofdfunctie eval_stratified() gebruikt om je volledige
audit met meerdere steekproeven door te rekenen, hoef je deze stappen
niet zelf uit te voeren. Het package roept deze hulpfuncties op de
achtergrond per geüploade steekproefregel automatisch aan. Zo wordt elke
steekproef feilloos en transparant geharmoniseerd voordat de convolutie
(het samenvoegen) plaatsvindt.

## `plan_stratified()`: Plannen van meerdere steekproeven

Deze functie berekent de optimale, dit wil zeggen minimale
steekproefomvang (n) voor elk van de afzonderlijke strata zodat bij
gezamenlijke evaluatie de maximale fout onder de algehele materialiteit
blijft.

### Algoritme

Doel van het planningsalgoritme is het vinden van een optimale verdeling
van zo min mogelijk steekproefposten over de strata. Optimaal: zodanig
dat gegeven het verwachte aantal fouten voor elk afzonderlijk stratum de
maximale fout van alle strata samen net onder de materialiteit ligt.

We baseren het planningsalgoritme op het evaluatie-algoritme. Met andere
woorden we baseren plan_stratified() op eval_stratified().

We gebruiken een heuvelklimalgoritme, dat stap voor stap
steekproefposten toevoegd aan de strata, waarbij steeds koers wordt
gehouden door het aanroepen van eval_stratified(). In pseudocode
uitgedrukt: while eval_stratified(strata) \> totale_materialiteit {
bepaal voor welk stratum de maximale fout het meest afneemt, als we de n
voor dat stratum ophogen; hoog de n1 voor dat stratum 1 op }

Mogelijk probleem: als we de n van alle strata met 1 ophogen dan heeft
dat geen invloed op de maximale fout. Dus de maximale fout neemt
helemaal niet af daardoor! Oplossing: we moeten met hogere aantallen n
verhogen.

Afgeleid probleem: met hoeveel moeten we de n dan verhogen? Oplossing:
We verhogen met de ruimte die we nog hebben gezien het maximaal aantal
steken dat we ueberhaupt willen trekken.

Mogelijke probleem: 2 of meer strata blijken ex equo te zijn. Oplossing:
we verhogen de n voor ieder van die strata.

## Webversie

Zie <https://cfjdoedens.shinyapps.io/auditstratified/>.

## Referenties

### Wikipedia: convolutie

<https://en.wikipedia.org/wiki/Convolution>

### JCGM 101:2008. Propagation of distributions using a Monte Carlo method.

<https://www.bipm.org/documents/20126/2071204/JCGM_101_2008_E.pdf>

Dit BIPM-document stelt dat de onzekerheid over verschillende bronnen
(strata) moet worden berekend door de volledige kansverdelingen te
combineren (convolutie). Alleen puntschattingen gebruiken is
onvoldoende. BIPM, Bureau International des Poids et Mesures, wordt
wereldwijd als gezaghebbend gezien. Het publiceert publiceert onder meer
de GUM-richtlijnen (Guide to the Expression of Uncertainty in
Measurement).

### Wikipedia: Monte Carlo methode

<https://en.wikipedia.org/wiki/Monte_Carlo_method>

Dit Wikipedia-artikel beschrijft de Monte Carlo-simulatie.

### Stevens 2024

Alan Stevens, Monte-Carlo Simulation An Introduction for Engineers and
Scientists, ISBN 9781032280806, Published October 4, 2024 by CRC Press

<https://www.routledge.com/Monte-Carlo-Simulation-An-Introduction-for-Engineers-and-Scientists/Stevens/p/book/9781032280806>

Dit boek geeft de basisprincipes van Monte Carlo-simulatie plus wat
toepassingen.

### MathWorks: Fast Fourier Transform (FFT)

<https://nl.mathworks.com/discovery/fft.html>

Dit korte artikel beschijft de Fast Fourier transformatie.

### Abate & Whitt 1992

Abate, J., & Whitt, W. (1992). The Fourier-series method for inverting
transforms of probability distributions.

<https://www.columbia.edu/~ww2040/FourierSeries1992.pdf>

Dit artikel gaat dieper in op de FFT.

### Wang 1988

<https://www.casact.org/sites/default/files/database/proceed_proceed98_980848.pdf>

Dit artikel beschrijft hoe meerdere risico’s samen te nemen met onder
meer Monte Carlo-simulatie en FFT.

## Ideeën voor verdere ontwikkeling

1.  Voeg aan de live-versie de mogelijkheid toe om te kiezen tussen:

- Nederlandse tekst + Europese getalnotatie, of
- Engelse tekst + Europese getalnotatie, of
- Engelse tekst + Engelse getalnotatie
