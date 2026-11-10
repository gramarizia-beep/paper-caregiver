# =============================================================================
# 16_microsimulazione_scenari.R
# Microsimulazione del contributo ai caregiver (art. 14 A.C. 2789-A) per scenari:
# quanti hanno diritto (platea), quanti fanno domanda (tasso di domanda, cioè
# 1 - non take-up) e quanto riceve ciascuno con il fondo fisso.
# Non usa microdati: parte dai numeri già verificati nel paper (relazione
# tecnica A.C. 2789 p. 24; A.C. 2789-A art. 14; microdati AVQ 2024 elaborati
# dagli script 08-10). È un esercizio per l'orale e per la tesi, non per il paper.
# Uscite: output/grafici/G16a_importo_per_platea_e_domande.png
#         output/grafici/G16b_spesa_per_scenario.png
#         output/tabelle/T21_microsimulazione_scenari.csv
# =============================================================================

TIPO_PNG <- if (Sys.info()[["sysname"]] == "Darwin") "quartz" else "cairo"   # il Mac non ha cairo
SFONDO <- "#f4f4f1"
BLU <- "#1f4e79"; AZZ <- "#6f93bf"; ROSSO <- "#b01c2e"; GRIGIO <- "#8a8a8a"; INK <- "#222222"; MUTED <- "#555555"
it <- function(x, d = 0) format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d, trim = TRUE)

# ---- 1. I numeri di partenza (tutti già nel paper) ---------------------------
FONDO      <- 251.41e6   # limite di spesa del contributo dal 2028, art. 14 c. 5 (euro)
MASSIMO    <- 4800       # 1.200 euro a trimestre, art. 14 c. 3 (euro l'anno)
QUOTA_15K  <- 0.7164     # quota con ISEE fino a 15.000 (relazione tecnica, Osservatorio INPS)
COEFF_1525 <- 0.5        # chi ha ISEE tra 15.000 e 25.000 prende in media la metà (ipotesi: ISEE distribuiti in modo uniforme)
# una persona con ISEE 15-25 mila "pesa" come mezzo contributo pieno
PESO_MEDIO <- QUOTA_15K + (1 - QUOTA_15K) * COEFF_1525     # = 0,858 contributi pieni per avente diritto

# Platee (aventi diritto, cioè chi ha tutti i requisiti compreso l'ISEE fino a 25.000)
# 87.000 è il tetto della relazione tecnica con ISEE fino a 25.000 (di cui 62.333 con ISEE fino a 15.000);
# 93.000 è un sesto dei 557.000 dell'AVQ 2024 (stessa ipotesi "un sesto" della relazione tecnica); gli altri sono scenari.
platee <- c(87000, round(557000 / 6), 150000, 250000)
# Tassi di domanda (1 - non take-up). Negli studi sulle prestazioni sociali il
# mancato ricorso va dal 20 al 60 per cento: qui 40, 60, 80 e 100 per cento.
tassi <- c(0.40, 0.60, 0.80, 1.00)

# ---- 2. La simulazione --------------------------------------------------------
simula <- function(platea, tasso) {
  beneficiari   <- platea * tasso                         # chi fa domanda e la ottiene
  pieni_equiv   <- beneficiari * PESO_MEDIO               # in "contributi pieni equivalenti"
  importo_pieno <- min(MASSIMO, FONDO / pieni_equiv)      # euro l'anno per chi ha ISEE fino a 15.000
  spesa         <- importo_pieno * pieni_equiv            # quanto si spende davvero
  data.frame(platea = platea, tasso = tasso, beneficiari = round(beneficiari),
             importo_pieno = round(importo_pieno), mensile = round(importo_pieno / 12),
             importo_isee20 = round(importo_pieno * 0.5),  # chi ha ISEE 20.000 prende la metà
             spesa_mln = round(spesa / 1e6, 1), non_speso_mln = round((FONDO - spesa) / 1e6, 1),
             quota_massimo = round(100 * importo_pieno / MASSIMO))
}
T21 <- do.call(rbind, lapply(platee, function(p) do.call(rbind, lapply(tassi, function(t) simula(p, t)))))
rownames(T21) <- NULL
dir.create("output/tabelle", showWarnings = FALSE, recursive = TRUE)
dir.create("output/grafici", showWarnings = FALSE, recursive = TRUE)
write.csv2(T21, "output/tabelle/T21_microsimulazione_scenari.csv", row.names = FALSE)
print(T21)

# ---- 3. Grafico a: importo annuo per platea, una linea per tasso di domanda ----
griglia <- seq(40000, 260000, by = 2000)
png("output/grafici/G16a_importo_per_platea_e_domande.png", width = 10, height = 6.6, units = "in", res = 240,
    pointsize = 11, type = TIPO_PNG, bg = SFONDO)
par(family = "sans", mar = c(7, 5.2, 5.8, 7), mgp = c(3, 0.6, 0), las = 1, bty = "n", xpd = NA)
plot(NA, xlim = c(40000, 260000), ylim = c(0, 5200), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
par(xpd = FALSE); abline(h = seq(0, 5000, 1000), col = "#dcdcd7"); par(xpd = NA)
axis(1, at = seq(50000, 250000, 50000), labels = it(seq(50000, 250000, 50000)), col = "#999999")
axis(2, at = seq(0, 5000, 1000), labels = paste0(it(seq(0, 5000, 1000)), " €"), col = "#999999")
abline(h = MASSIMO, col = ROSSO, lty = 2, lwd = 1.5)
text(260000, MASSIMO + 130, "massimo di legge 4.800 € l'anno (400 al mese)", adj = c(1, 0), col = ROSSO, cex = 0.85)
colori <- c(GRIGIO, AZZ, BLU, "#0b2a46"); spess <- c(1.6, 1.8, 2.2, 2.6)
for (i in seq_along(tassi)) {
  y <- sapply(griglia, function(p) simula(p, tassi[i])$importo_pieno)
  lines(griglia, y, col = colori[i], lwd = spess[i])
  text(263000, y[length(y)], paste0("chiede il ", tassi[i] * 100, "%"), adj = c(0, 0.5), col = colori[i], cex = 0.85, font = 2)
}
# i tre punti di riferimento con tutti che chiedono
for (p in c(87000, 93000)) {
  y <- simula(p, 1)$importo_pieno
  points(p, y, pch = 21, bg = "white", col = BLU, lwd = 2, cex = 1.2)
  if (p == 87000) { text(80000, 3300, paste0("87.000 (relazione tecnica): ", it(y), " €"), cex = 0.8, col = INK, adj = c(1, 0.5))
                    segments(80600, 3300, p - 1200, y - 40, col = GRIGIO, lwd = 0.8) }
  else            { text(90000, 2600, paste0("93.000 (un sesto dell'AVQ): ", it(y), " €"), cex = 0.8, col = INK, adj = c(1, 0.5))
                    segments(90600, 2600, p - 700, y - 110, col = GRIGIO, lwd = 0.8) }
}
mtext("Aventi diritto (persone con tutti i requisiti; soglia ISEE a 25.000 euro, testo approvato dalla Camera)", side = 1, line = 2.6, col = MUTED, cex = 0.95)
mtext("Con la soglia ISEE a 25.000, l'importo scende al crescere delle domande",
      side = 3, line = 3.8, adj = 0, font = 2, cex = 1.3, col = INK, at = 40000)
sott <- strwrap(paste0("Euro l'anno a chi ha ISEE fino a 15.000. Fino a circa 75.000 domande il fondo di 251,41 milioni copre il massimo di 4.800 euro; oltre, lo stesso fondo si divide tra pi\u00f9 persone. ",
                       "Ogni linea \u00e8 la quota degli aventi diritto che fa domanda; chi ha ISEE tra 15.000 e 25.000 riceve in media la met\u00e0 (quota con ISEE fino a 15.000: 71,6%, relazione tecnica)."), width = 150)
for (k in seq_along(sott)) mtext(sott[k], side = 3, line = 2.5 - (k - 1) * 1.05, adj = 0, cex = 0.8, col = MUTED, at = 40000)
fonte <- strwrap(paste0("Fonte: A.C. 2789-A, art. 14; relazione tecnica A.C. 2789, p. 24; AVQ 2024 (557.000 = persone con limitazioni gravi conviventi con un adulto ",
                        "non occupato in famiglie con risorse scarse, script 08). Le platee oltre 87.000 sono scenari, non stime. Script 16_microsimulazione_scenari.R."), width = 165)
for (k in seq_along(fonte)) mtext(fonte[k], side = 1, line = 4.3 + (k - 1) * 0.95, adj = 0, cex = 0.72, col = MUTED, at = 40000)
dev.off()

# ---- 4. Grafico b: spesa effettiva per scenario, con la parte del fondo non spesa ----
sel <- T21[T21$platea %in% c(87000, 150000, 250000), ]
sel$etic <- paste0(it(sel$platea), "\nchiede il\n", sel$tasso * 100, "%")
png("output/grafici/G16b_spesa_per_scenario.png", width = 10, height = 6.4, units = "in", res = 240,
    pointsize = 11, type = TIPO_PNG, bg = SFONDO)
par(family = "sans", mar = c(7.5, 5.2, 6.2, 1.5), mgp = c(3, 0.6, 0), las = 1, bty = "n", xpd = NA)
b <- barplot(rbind(sel$spesa_mln, sel$non_speso_mln), col = c(BLU, "#d9d9d4"), border = NA, ylim = c(0, 300),
             yaxt = "n", names.arg = rep("", nrow(sel)), space = c(0.6, rep(c(0.25, 0.25, 0.25, 0.9), 2), 0.25, 0.25, 0.25))
axis(2, at = seq(0, 250, 50), labels = paste0(it(seq(0, 250, 50)), " mln"), col = "#999999")
abline(h = FONDO / 1e6, col = ROSSO, lty = 2, lwd = 1.5)
text(b[1] - 0.55, FONDO / 1e6 + 5, "limite di spesa 251,41 mln", adj = c(0, 0), col = ROSSO, cex = 0.8)
text(b, -8, sel$etic, adj = c(0.5, 1), cex = 0.75, col = INK)
text(b, pmin(sel$spesa_mln, 236) + 6, paste0(it(sel$importo_pieno), " €"), cex = 0.78, font = 2, col = ifelse(sel$spesa_mln > 236, "white", INK))
text(mean(b[1:4]), 288, "87.000: tetto della relazione tecnica", cex = 0.85, col = MUTED)
text(mean(b[5:8]), 288, "150.000: scenario", cex = 0.85, col = MUTED)
text(mean(b[9:12]), 288, "250.000: scenario", cex = 0.85, col = MUTED)
mtext("Dodici scenari: tre platee di aventi diritto, quattro quote di domanda, lo stesso fondo di 251 milioni",
      side = 3, line = 3.9, adj = 0, font = 2, cex = 1.2, col = INK, at = -0.6)
sott2 <- strwrap(paste0("Con la soglia ISEE a 25.000. Per ogni platea (87.000, 150.000, 250.000) la quota che fa domanda va dal 40 al 100 per cento. ",
                        "Barra scura = quanto si spende davvero, barra chiara = parte del fondo che resta non spesa; sopra ogni barra l'importo annuo per chi ha ISEE fino a 15.000: ",
                        "resta 4.800 euro finch\u00e9 il fondo avanza, poi scende."), width = 160)
for (k in seq_along(sott2)) mtext(sott2[k], side = 3, line = 2.7 - (k - 1) * 1.0, adj = 0, cex = 0.8, col = MUTED, at = -0.6)
mtext("Fonte: come il grafico precedente. Nel 2027 il fondo è di 186,08 milioni perché si pagano tre trimestri. Script 16_microsimulazione_scenari.R.",
      side = 1, line = 5.8, adj = 0, cex = 0.72, col = MUTED)
dev.off()
cat("Salvati G16a, G16b e T21\n")
