# Programmeerstandaard

## Commentaar bij codeblokken

Blokken samenhangende code worden voorafgegaan door een commentaarregel.

- Als de code **geen witregels** bevat: alleen commentaar ervoor.
- Als de code **wel witregels** bevat: commentaar ervoor, en de code
  omgeven met [`{ }`](https://rdrr.io/r/base/Paren.html).

Witregels tussen de blokken. Nesting is mogelijk.

### Voorbeeld

``` r

# Valideer de argumentkeuzes
model <- match.arg(model)
methode <- match.arg(methode)

# Controleer de invoer
{
  stopifnot(is_tibble(steekproeven))

  # Strikte controle op vereiste kolommen
  stopifnot("naam" %in% colnames(steekproeven))
  stopifnot("waarde_laag" %in% colnames(steekproeven))

  # Basiscontroles
  stopifnot(is.numeric(waarde_laag))
  stopifnot(0 <= waarde_laag)
}

# Bepaal totale geldswaarde
totaalgeld_laag <- sum(waarde_laag)
totaalgeld_hoog <- sum(fout_hoog)
```
