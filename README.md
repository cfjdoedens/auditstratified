
# evalstratified

<!-- badges: start -->
[![R-CMD-check](https://github.com/cfjdoedens/evalstratified/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cfjdoedens/evalstratified/actions/workflows/R-CMD-check.yaml)
<!-- badges: end --> Het doel van evalstratified is om een schatting te
maken van de foutfractie van een aantal monetaire bestanden. Dit gebeurt
op basis van een algeheel betrouwbaarheidsniveau, een
betrouwbaarheidsniveau per bestand, en de foutfracties die zijn gevonden
in de steekproefposten van elk van deze bestanden.

## Installatie

Je kunt de ontwikkelversie van evalstratified laden van
[GitHub](https://github.com/):

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
install.packages("devtools")
}
devtools::install_github("cfjdoedens/evalstratified")
```

## De basis: meerdere steekproeven combineren

De functie van `evalstratified` is het statistisch samenvoegen van de
evaluaties van meerdere, afzonderlijke steekproeven. Dit is nodig in de
auditpraktijk wanneer een totale populatie bestaat uit verschillende
deelpopulaties (bijvoorbeeld verschillende locaties, systemen of
processen) waaruit separate steekproeven zijn getrokken.

In het meest basale scenario bevatten deze steekproeven geen
uitzonderlijk grote posten (alle gecontroleerde posten zijn kleiner dan
het steekproefinterval). In dit geval zijn er dus geen posten die
integraal (100%) gecontroleerd hoeven te worden; de zogenaamde
‘hoogstrata’ zijn leeg (ofwel € 0).

Wanneer je de resultaten van deze losse steekproeven simpelweg bij
elkaar op zou tellen, zou je de statistische onzekerheid verkeerd
inschatten. `evalstratified` lost dit op door de afzonderlijke
kansverdelingen van de verschillende steekproeven wiskundig te
combineren door middel van **convolutie**. Het resultaat is één zuivere,
gecombineerde kanskromme met één correct berekende maximale
fout(fractie) en precisie voor de totale organisatie.

### Verfijning: Hoogstratum en Laagstratum

In de praktijk komt het voor dat bestanden wél enkele zeer grote posten
bevatten die integraal gecontroleerd worden. Om hier wiskundig correct
mee om te gaan, biedt `evalstratified` een verfijning op bovenstaand
basisprincipe: het onderscheid tussen een **hoogstratum** en een
**laagstratum** binnen de steekproeven. Het hoogstratum bevat de
integraal gecontroleerde posten, deze zijn niet geextrapoleerd bij de
steekproefevaluatie. Het laagstrum bevat de posten kleiner dan het
steekproefinterval. Deze posten zijn geextrapoleerd en leveren samen een
kanskromme die beschrijft wat de kans is op de mogeljke foutfractie in
de massa van posten kleiner dan het steekproefinterval.

### Voorbeelden van verschillende evaluatie-scenario’s

Om de flexibiliteit van `evalstratified` te demonstreren, schetsen we
hieronder drie veelvoorkomende praktijksituaties en hoe het package
hiermee omgaat. Het is hierbij belangrijk om te onthouden dat de
uiteindelijke evaluatie een *foutfractie* is: de totale (geprojecteerde)
fout gedeeld door de totale populatieomvang.

**Scenario 1: Geen fouten in de integraal gecontroleerde posten (Het
verdunningseffect)** We evalueren twee steekproeven die beide bestaan
uit een laagstratum (de getrokken steekproef) en een hoogstratum (de
100% gecontroleerde grote posten). In de steekproeven (laagstrata)
zitten enkele fouten, maar in de hoogstrata van beide bestanden worden
*geen* fouten aangetroffen (absolute fout = € 0). *Hoe `evalstratified`
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
5.000. *Hoe `evalstratified` dit verwerkt:* De statistische onzekerheid
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
*Hoe `evalstratified` dit verwerkt:* Bestand 2 heeft door de 100%
controle geen statistische onzekerheid meer. De gebruiker van het
package geeft daarom voor het laagstratum een omvang van 0 euro op, en
de 100% populatie wordt door de gebruiker in de parameters fout_hoog en
goed_hoog geplaatst. Dus het integraal gecontroleerde bestand wordt dan
door het package in zijn geheel behandeld als één groot ‘hoogstratum’.
`evalstratified` berekent de verdeling van bestand 1, slaat de
convolutie voor bestand 2 over, en voegt de omvang én de exacte fout van
€ 12.500 van bestand 2 direct toe aan de breuk voor de algehele
eindevaluatie.

## De rol van risicofactoren, materialiteit en “virtuele” posten

Wanneer bestanden uit verschillende bedrijfsonderdelen of processen
komen, worden ze zelden met exact hetzelfde risicoprofiel gecontroleerd.
`evalstratified` is speciaal ontworpen om steekproeven met verschillende
risico-inschattingen toch wiskundig zuiver met elkaar te kunnen
combineren. Om te begrijpen hoe het package dit doet, is het belangrijk
te kijken naar de wisselwerking tussen risicofactoren, materialiteit en
zogenaamde “virtuele foutloze posten”.

**De basis: Risico en Materialiteit** In de auditpraktijk wordt de
benodigde statistische zekerheid voor een steekproef bepaald door de
inschatting van het Inherent Risico (IHR), het Interne Beheersingsrisico
(IBR) en het risico van Cijferbeoordelingen (CAR).

- Staan al deze risicofactoren op “Hoog”, dan leunt de accountant
  volledig op de steekproef en is er bijvoorbeeld 95% zekerheid nodig.
- Is het IBR “Laag” (bijvoorbeeld door een sterke interne controle), dan
  mag de vereiste zekerheid uit de steekproef omlaag (bijv. naar 64%).

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
vormt het wiskundige fundament onder `evalstratified` bij het combineren
van verschillende steekproeven:

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

`evalstratified` lost dit op door bij de evaluatie voor élke
afzonderlijke steekproef het totale bewijs te harmoniseren (fysiek
getrokken posten + virtuele foutloze posten op basis van de opgegeven
IHR/IBR/CAR-parameters en materialiteit). Pas nadat de kansverdelingen
via deze virtuele posten op hetzelfde fundamentele vertrekpunt zijn
gebracht, past het package de convolutie toe. Hierdoor weegt de vooraf
verkregen zekerheid per bestand zuiver en proportioneel mee in de
berekende maximale fout voor de gehele populatie.

## De data-input: Het opbouwen van de steekproeven-tibble

Om de hoofdfunctie `eval_stratified()` te kunnen gebruiken, moet je de
resultaten van je steekproeven aanleveren in een specifieke dataframe,
een zogenaamde `tibble`. Elke rij in deze tabel representeert één
afzonderlijk te evalueren bestand of steekproef.

Het package is streng op de invoer om wiskundige fouten te voorkomen: er
wordt gecontroleerd op redundante totaalvelden. Zo moet `n_totaal`
bijvoorbeeld exact gelijk zijn aan `n_laag + n_hoog`.

Hier is een voorbeeld van hoe je deze invoer-tibble opbouwt in R,
gebaseerd op de twee eerder genoemde steekproeven:

``` r
library(tibble)
library(evalstratified)

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

### Verklaring van de verplichte kolommen:

- naam: Een herkenbare naam voor het bestand of de steekproef.

- Laagstratum (de steekproef):

        waarde_laag: De totale boekwaarde in euro's waaruit de steekproef is getrokken.
        n_laag: Het aantal fysiek gecontroleerde posten in deze steekproef.
        k_laag: De som van de gevonden foutfracties (bijv. 1 post volledig fout = 1.0, 1 post voor de helft fout = 0.5).

- Risico & Zekerheid:

        ihr, ibr, car: De risico-inschattingen (kies uit "H", "M", of "L").
        materialiteit: De gehanteerde materialiteit als fractie (bijv. 0.02 voor 2%).

- Hoogstratum (100% controle):

        fout_hoog: Het in euro's gevonden foutbedrag in de integraal gecontroleerde posten.
        goed_hoog: Het in euro's goedgekeurde bedrag in dit stratum.
        n_hoog: Het aantal posten dat in dit stratum is gecontroleerd.

- Totaalcontroles (Redundantie voor veiligheid):

        n_totaal: Totale aantal gecontroleerde posten (n_laag + n_hoog).
        waarde_hoog: Totale boekwaarde van het hoogstratum (fout_hoog + goed_hoog).
        waarde_populatie: De absolute totale boekwaarde (waarde_laag + waarde_hoog).

### In de praktijk: Rekenen met risico’s en virtuele posten

`evalstratified` bevat twee ingebouwde hulpfuncties waarmee je deze
theoretische stappen zelf inzichtelijk kunt maken en kunt narekenen:
`haro_nog_nodige_zekerheid()` en `foutloze_posten_equivalent()`.

**1. De benodigde zekerheid berekenen** Stel, je hebt een deelpopulatie
met een Laag Inherent Risico (L), een Midden Intern Beheersingsrisico
(M) en een Hoog Cijferanalyserisico (H). Je kunt de exact vereiste
statistische zekerheid volgens het HARo-model direct opvragen met de
volgende functie:

``` r
library(evalstratified)

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

## Webversie

Zie <https://cfjdoedens.shinyapps.io/evalstratified/>.

## Ideeën voor verdere ontwikkeling

1.  Voeg aan de live-versie de mogelijkheid toe om te kiezen tussen:

- Nederlandse tekst + Europese getalnotatie, of
- Engelse tekst + Europese getalnotatie, of
- Engelse tekst + Engelse getalnotatie
