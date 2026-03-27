# Bereken de nog benodigde zekerheid te verkrijgen uit detailcontrole

In het Handboek Auditing Rijksoverheid, paragraaf B7.3.4 staat
beschreven welke zekerheid de auditor moet bereiken met detailcontroles,
gegeven ihr, ibr en car. Deze functie maakt de betreffende berekening.

## Usage

``` r
haro_nog_nodige_zekerheid(ihr = "H", ibr = "H", car = "H")
```

## Arguments

- ihr:

  Het door de auditor ingeschatte inherente risico. Te weten `"H"`,
  `"M"`, of `"L"`, ofwel hoog, midden of laag.

- ibr:

  Het door de auditor ingeschatte interne beheersingsrisico. Te weten
  `"H"`, `"M"`, of `"L"`, ofwel hoog, midden of laag.

- car:

  Het door de auditor ingeschatte cijferanalyserisico. Te weten `"H"`,
  `"M"`, of `"L"`, ofwel hoog, midden of laag.

## Value

Een getal tussen 0 en 1 (inclusief).

## Details

Een detailcontrole kan bijvoorbeeld een steekproef zijn.

Het rekenmodel dat wordt gehanteerd staat bekend als het ARM, het Audit
Risk Model. Deze term wordt echter niet gebruikt in het HARo. Het model
is gebaseerd op inherent risico, ihr, intern beheersings risico, ibr,
cijferanalyse risico, car. De mogelijke waarden zijn: H, M en L, ofwel,
hoog, midden en laag. In B7.3.4 wordt een getalswaarde (uit `[0, 1]`)
gekoppeld aan deze H, M en L, die deels verschilt per type risico: ihr,
ibr of car. Zie hiervoor de tabel op de tweede bladzijde van B7.3.4.
Vanuit deze tabel wordt dan de tabel die begint op de eerste bladzijde
van B7.3.4 berekend. De berekening hiervoor staat niet in B7.3.4, maar
is wel bekend. Deze berekening wordt in de programmacode van
`haro_nog_nodige_zekerheid()` toegepast.

In de tabel die begint op de eerste bladzijde van B7.3.4 komen niet
alleen getalswaarden voor als resultaat van een combinatie van ihr, ibr,
en car, maar ook "-". Dit komt voor in alle gevallen waar de gebruikte
berekeningen tot een getalswaarde lager dan .55 leiden. Ik citeer uit
B7.3.4:

    "Als in de tabel een "-" is opgenomen betekent dat niet dat er geen
     gegevensgerichte werkzaamheden nodig zijn, maar alleen dat de benodigde
     zekerheid niet effectief met een (statistische) steekproef
     kan worden verkregen."

Dat je die lagere zekerheden niet met een statistische steekproef kunt
bereiken lijkt me onjuist. Bovendien eist bijvoorbeeld Standaard 330 van
de NBA:

     "Ongeacht de inschatting van de risico's op een afwijking van
      materieel belang dient de accountant gegevensgerichte controles op
      te zetten en uit te voeren voor elk van de transactiestromen,
      rekeningsaldi en in de financiële overzichten opgenomen toelichtingen
      die van materieel belang zijn."

En deze getallen zijn ook nodig om de berekening te kunnen uitvoeren
door de functie foutloze_posten_equivalent() in deze module. Die
functie, foutloze_posten_equivalent(), is weer nodig voor
eval_stratified() om om te kunnen gaan met steekproeven die zijn
uitgevoerd gebaseerd op verschillende zekerheid.

De functie `haro_nog_nodige_zekerheid()` geeft daarom ook waarden onder
de .55 terug.

Een bijzonderheid is dat toepassing van de formules kan leiden tot een
niet-positieve zekerheid. Dit laatste heeft geen betekenisvolle
interpretatie, en geeft ook aan dat het model een benadering is. Om deze
niet-positieve waarden uit te sluiten, worden in
`haro_nog_nodige_zekerheid()` potentiele negatieve waarden van de
functie omgezet naar 0.05. Dit komt alleen voor als ihr, ibr en car
alledrie op laag staan (LLL) Omdat daardoor de drie laagste waarden
dicht op elkaar komen, worden de twee waarden voor LML en MLL verhoogd
met 0.05 tot respectievelijk 0.09 en 0.12.

Ik heb een voorstel gedaan binnen de ADR werkgroep steekproeven om de
HARo-tabel dienovereenkomstig aan te passen.

## Examples

``` r
nnz <- haro_nog_nodige_zekerheid()
```
