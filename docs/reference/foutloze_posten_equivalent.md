# Geef equivalent in getrokken foutloze posten voor verlaagde risicoschattingen.

Volgens het HARo is de benodigde zekerheid bij een gegevensgerichte
controle 95%, namelijk 100% minus het accountantsrisico (5%).

Ook volgens het HARo, definieren ihr, ibr en car samen de nog benodigde
zekerheid voor een gegevensgerichte controle. Bereken het aantal
foutloze posten dat daar mee overeenkomt, rekeninghoudend met de
materialiteit.

## Usage

``` r
foutloze_posten_equivalent(
  ihr = "H",
  ibr = "H",
  car = "H",
  materialiteit = 0.01
)
```

## Arguments

- ihr:

  inherente risico, te weten H, M of L

- ibr:

  internebeheersingsrisico, te weten H, M of L

- car:

  cijferanalyserisico, te weten H, M of L

- materialiteit:

  de maximale foutfractie in de te onderzoeken geldmassa

## Value

Het equivalent in getrokken foutloze posten. Een integer \>= 0.

## Examples

``` r
fpe <- foutloze_posten_equivalent()
```
