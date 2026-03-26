# ==============================================================================
# PROTOTYPE: Numerieke Convolutie voor Audit Steekproeven
# ==============================================================================

# 1. Definieer de parameters van twee steekproeven
waarde1 <- 1000000; n1 <- 1; k1 <- 0
waarde2 <- 500000;  n2 <- 1; k2 <- 1
totale_waarde <- waarde1 + waarde2

# Bepaal hoe fijnmazig we rekenen (hoe hoger, hoe gladder en trager)
granulariteit <- 10000000

# 2. Bepaal de vaste stapgrootte (dx) in euro's voor de hele populatie
dx <- totale_waarde / granulariteit

# 3. Maak de assen per steekproef (van €0 t/m de maximale waarde van dat stratum)
# Let op: de lengtes van deze assen zijn dus verschillend!
x1_geld <- seq(0, waarde1, by = dx)
x2_geld <- seq(0, waarde2, by = dx)

# 4. Bereken de kansmassa per stapje (we gebruiken het Binomiale/Beta model)
# We delen door 'waarde' om de euro's tijdelijk om te rekenen naar een fractie (0 tot 1)
# omdat de dbeta() functie alleen breuken snapt.
p1 <- dbeta(x1_geld / waarde1, shape1 = k1 + 1, shape2 = n1 - k1 + 1)
p2 <- dbeta(x2_geld / waarde2, shape1 = k2 + 1, shape2 = n2 - k2 + 1)

# Normaliseer de kansmassa's zodat de som van alle rechthoekjes exact 1 (100%) is.
p1 <- p1 / sum(p1)
p2 <- p2 / sum(p2)

# ==============================================================================
# DE CONVOLUTIE
# ==============================================================================
# Let op R's eigenaardigheid: convolve berekent standaard kruiscorrelatie.
# Om wiskundige convolutie te krijgen, MOETEN we de tweede reeks omdraaien met rev().
# type = "open" zorgt ervoor dat de reeksen volledig langs elkaar schuiven (het 2N-1 effect).

p_totaal <- convolve(p1, rev(p2), type = "open")

# ==============================================================================
# RESULTAAT INTERPRETEREN
# ==============================================================================

# Maak de nieuwe x-as voor het resultaat.
# Omdat we in stapjes van 'dx' zijn opgeschoven, is de as simpelweg 0, dx, 2dx, 3dx...
x_totaal_geld <- seq(0, by = dx, length.out = length(p_totaal))

# Bereken de maximale fout bij 95% zekerheid
cumulatieve_kans <- cumsum(p_totaal)
zekerheid <- 0.95
index_max_fout <- which(cumulatieve_kans >= zekerheid)[1]

max_fout_geld <- x_totaal_geld[index_max_fout]
max_fout_fractie <- max_fout_geld / totale_waarde

# Print de resultaten in de console
cat("--- RESULTATEN NUMERIEKE CONVOLUTIE ---\n")
cat("Maximale fout in euro's:  €", format(round(max_fout_geld, 2), big.mark = ".", decimal.mark = ","), "\n")
cat("Maximale fout in fractie:  ", round(max_fout_fractie * 100, 2), "%\n")

# Teken de kanskromme om het te bewijzen
plot(x_totaal_geld, p_totaal, type = "l", col = "#2980b9", lwd = 2,
     main = "Numerieke Convolutie (Zonder Monte Carlo)",
     xlab = "Fout in Euro's", ylab = "Kansmassa (relatief)")
abline(v = max_fout_geld, col = "#e74c3c", lwd = 2, lty = 2)
legend("topright", legend = c("Kanskromme", paste0("Max Fout (", zekerheid*100, "%)")),
       col = c("#2980b9", "#e74c3c"), lwd = 2, lty = c(1, 2))
