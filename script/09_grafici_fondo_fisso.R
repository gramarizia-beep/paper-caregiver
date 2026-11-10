# =============================================================================
# 09_grafici_fondo_fisso.R
# Tre grafici per l'Allegato A della tesina, con R di base (nessun pacchetto).
#  G06  importo annuo per beneficiario al variare della platea (fondo fisso)
#  G07  formula dell'ISEE tra 15.000 e 25.000 euro
#  G08  gli ordini di grandezza della platea a confronto
# Gli stessi numeri dello script 08_fondo_fisso_platea.R.
# Se sul Mac le lettere accentate escono male, lancia prima:
#   Sys.setlocale("LC_ALL", "it_IT.UTF-8")
# =============================================================================

# --- Parametri (identici allo script 08) -------------------------------------
# Limite di spesa del contributo (art. 14, comma 5, del testo approvato dalla Camera):
# 186,08 milioni nel 2027 (si pagano tre trimestri), 251,41 milioni nel 2028,
# 251,21 nel 2029-2034, 251,01 dal 2035. Lo stanziamento complessivo della legge
# (257 milioni nel 2027, 260 dal 2028) comprende anche piattaforma e personale INPS.
FONDO_CONTRIBUTO <- 251.41e6          # euro l'anno a regime (2028)
FONDO_2027       <- 186.08e6          # euro nel 2027, tre trimestri
CONTRIBUTO_MAX <- 1200 * 4            # 4.800 euro l'anno
ISEE_PIENO     <- 15000
ISEE_MAX       <- 25000
# Relazione tecnica al disegno di legge (A.C. 2789, 6/2/2026, p. 24 dello stampato):
RT_CONVIVENTI_20H      <- 1096000   # caregiver familiari conviventi non occupati, almeno 20 ore (EHIS 2019)
RT_NON_OCCUPATI        <- 522000    # ...di cui assistono una persona con limitazioni gravi
RT_PLATEA_MAX          <- 87000     # ...di cui con almeno 91 ore: "non piu' di un sesto" (ipotesi del Governo)
RT_BENEFICIARI         <- 62333     # ...di cui con ISEE fino a 15.000 euro (71,64%, dati DSU INPS 2023-24)
AVQ_CONVIVENTI_NON_OCC <- 1345000
AVQ_RISORSE_SCARSE     <- 557000

# --- Aspetto comune -------------------------------------------------------------
# Sul Mac R scrive i PNG con "quartz"; altrove con "cairo" (serve per le lettere accentate).
TIPO_PNG <- if (Sys.info()[["sysname"]] == "Darwin") "quartz" else "cairo"
SFONDO <- "#f4f4f1"   # fondo neutro chiaro di tutte le figure del paper, perché il testo risalti (10/11/2026)
GRIGIO  <- "#4D4D4D"
ROSSO   <- "#B2182B"
BLU     <- "#2166AC"
GRIGLIA <- "#DDDDDD"
TESTO2  <- "#555555"
it <- function(x, dec = 0) format(round(x, dec), big.mark = ".", decimal.mark = ",", trim = TRUE)

dir.create(file.path("output", "grafici"), showWarnings = FALSE, recursive = TRUE)
apri <- function(nome, larghezza = 10, altezza = 6.4) {
  png(file.path("output", "grafici", paste0(nome, ".png")),
      width = larghezza, height = altezza, units = "in", res = 240, type = TIPO_PNG, bg = SFONDO)
  par(family = "sans", mar = c(8.8, 5, 5.5, 1.5), mgp = c(3, 0.6, 0), las = 1,
      cex.axis = 0.85, bty = "n")
}
titoli <- function(titolo, sottotitolo, fonte) {
  mtext(titolo, side = 3, line = 3.2, adj = 0, font = 2, cex = 1.15)
  mtext(sottotitolo, side = 3, line = 1.6, adj = 0, cex = 0.85, col = TESTO2)
  mtext(fonte, side = 1, line = 7.8, adj = 0, cex = 0.68, col = TESTO2)
}

# =============================================================================
# G06 - importo annuo per beneficiario con fondo fisso
# =============================================================================
platea  <- c(FONDO_CONTRIBUTO / CONTRIBUTO_MAX, RT_BENEFICIARI, RT_PLATEA_MAX, 100000,
             150000, 200000, 300000, AVQ_RISORSE_SCARSE)
importo <- pmin(CONTRIBUTO_MAX, FONDO_CONTRIBUTO / platea)
etich   <- c(paste0(it(round(FONDO_CONTRIBUTO / CONTRIBUTO_MAX)), "\ncontributi\npieni"), "62.333\nrelazione\ntecnica", "87.000\ntetto con\nISEE 25.000", "100.000",
             "150.000", "200.000", "300.000", "557.000\nimbuto\nAVQ 2024")
colori  <- rep(GRIGIO, length(platea)); colori[2] <- ROSSO

apri("G06_importo_per_beneficiario_fondo_fisso")
b <- barplot(importo, names.arg = rep("", length(platea)), col = colori, border = NA,
             ylim = c(0, 5400), yaxt = "n", space = 0.35)
abline(h = seq(1000, 5000, 1000), col = GRIGLIA, lwd = 0.6)
barplot(importo, col = colori, border = NA, add = TRUE, axes = FALSE, names.arg = rep("", 8), space = 0.35)
axis(2, at = seq(0, 5000, 1000), labels = paste(it(seq(0, 5000, 1000)), "€"), col = NA, col.ticks = NA)
abline(h = CONTRIBUTO_MAX, col = ROSSO, lty = 2, lwd = 1.2)
text(max(b) + 0.5, CONTRIBUTO_MAX + 170, "massimo di legge 4.800 € (400 al mese)",
     adj = 1, cex = 0.8, col = ROSSO)
text(b, importo + 150, paste0(it(importo), " €"), cex = 0.85, font = 2)
text(b, importo + 420, paste0(round(100 * importo / CONTRIBUTO_MAX), "%"), cex = 0.75, col = TESTO2)
mtext(etich, side = 1, at = b, line = 0.5, cex = 0.78, padj = 0.5)
mtext("Platea dei beneficiari (caregiver prevalenti)", side = 1, line = 4.2, cex = 0.85, col = TESTO2)
titoli("Con un fondo fisso di 251 milioni, più beneficiari vuol dire meno euro a ciascuno",
       "Importo annuo per caregiver prevalente al variare della platea. In percentuale, la quota del massimo di legge.",
       paste0("Fonte: A.C. 2789-A (Camera, 14/10/2026), art. 14: limite di spesa del contributo 251,41 milioni l'anno dal 2028 (186,08 nel 2027, tre trimestri).\n",
              "Relazione tecnica all'A.C. 2789 (p. 24): 62.333 beneficiari con ISEE fino a 15.000 euro, su 87.000 caregiver stimati con almeno 91 ore;\n",
              "87.000 è quindi il tetto anche con l'ISEE a 25.000. 557.000 = persone con limitazioni gravi conviventi con un adulto non occupato,\n",
              "in famiglie con risorse scarse (prima esplorazione dei microdati ISTAT AVQ 2024). Oltre 87.000 sono scenari, non stime."))
dev.off()

# =============================================================================
# G07 - la formula dell'ISEE tra 15.000 e 25.000
# =============================================================================
isee  <- seq(0, 27500, by = 250)
coeff <- ifelse(isee <= ISEE_PIENO, 1,
         ifelse(isee >= ISEE_MAX, 0, (ISEE_MAX - isee) / (ISEE_MAX - ISEE_PIENO)))
mensile <- CONTRIBUTO_MAX * coeff / 12
punti <- c(15000, 17500, 20000, 22500, 25000)
punti_y <- CONTRIBUTO_MAX / 12 * ifelse(punti <= ISEE_PIENO, 1, (ISEE_MAX - punti) / 10000)

apri("G07_formula_isee_15000_25000")
plot(isee, mensile, type = "n", xlim = c(0, 27500), ylim = c(0, 470), xaxt = "n", yaxt = "n",
     xlab = "", ylab = "")
rect(ISEE_PIENO, 0, ISEE_MAX, 470, col = "#F3F3F3", border = NA)
abline(h = seq(100, 400, 100), col = GRIGLIA, lwd = 0.6)
axis(1, at = seq(0, 25000, 5000), labels = paste(it(seq(0, 25000, 5000)), "€"), col = NA, col.ticks = NA)
axis(2, at = seq(0, 400, 100), labels = paste(it(seq(0, 400, 100)), "€"), col = NA, col.ticks = NA)
lines(isee, mensile, col = BLU, lwd = 2.5)
points(punti, punti_y, pch = 21, bg = "white", col = BLU, lwd = 2, cex = 1.3)
text(punti, punti_y, paste0(it(punti_y), " € (", round(100 * punti_y / 400), "%)"),
     cex = 0.8, font = 2, pos = c(2, 4, 4, 4, 2), offset = 0.9)
text(ISEE_PIENO, 452, "vecchia soglia\n15.000 €", cex = 0.75, col = TESTO2, adj = 0.5)
text(ISEE_MAX, 450, "nuova soglia\n25.000 €", cex = 0.75, col = TESTO2, adj = 0.5)
text(7500, 30, "importo pieno", cex = 0.8, col = BLU)
text(22600, 330, "importo ridotto\nin proporzione", cex = 0.8, col = BLU)
mtext("ISEE del nucleo familiare", side = 1, line = 2.6, cex = 0.85, col = TESTO2)
mtext("Importo mensile massimo", side = 2, line = 3.6, cex = 0.85, col = TESTO2, las = 0)
titoli("La soglia ISEE sale a 25.000 euro, ma oltre 15.000 l'importo scende in proporzione",
       "Importo mensile massimo del contributo per ISEE del nucleo. Tra 15.000 e 25.000: 400 € × (25.000 − ISEE) / 10.000.",
       paste0("Fonte: A.C. 2789-A, art. 14, comma 3 (testo approvato dalla Camera il 14/10/2026).\n",
              "Il contributo non è dovuto se, dopo il calcolo, risulta pari o inferiore a 30 € nel trimestre.\n",
              "L'importo effettivo è poi fissato ogni trimestre nei limiti delle risorse (vedi il grafico sul fondo fisso)."))
dev.off()

# =============================================================================
# G08 - gli ordini di grandezza a confronto
# =============================================================================
INPS_ACCOMPAGNAMENTO <- 2361982   # indennità di accompagnamento vigenti al 31/12/2024
voci <- c("Titolari di indennità di accompagnamento\n(INPS, 31 dicembre 2024)",
          "Persone con limitazioni gravi che convivono\ncon un adulto non occupato (AVQ 2024)",
          "...di cui in famiglie con risorse economiche\nscarse o insufficienti (AVQ 2024)",
          "Caregiver familiari conviventi non occupati,\nalmeno 20 ore a settimana (EHIS 2019, rel. tecnica)",
          "...di cui assistono una persona con\nlimitazioni gravi (relazione tecnica)",
          "...di cui con almeno 91 ore: \"non più di un sesto\"\n(ipotesi della relazione tecnica)",
          "...di cui con ISEE fino a 15.000 euro:\nbeneficiari stimati (relazione tecnica)",
          "Contributi pieni da 4.800 € pagabili\ncon 251 milioni l'anno")
valori <- c(INPS_ACCOMPAGNAMENTO, AVQ_CONVIVENTI_NON_OCC, AVQ_RISORSE_SCARSE,
            RT_CONVIVENTI_20H, RT_NON_OCCUPATI, RT_PLATEA_MAX, RT_BENEFICIARI, FONDO_CONTRIBUTO / CONTRIBUTO_MAX)
tipo   <- c("inps", "assistiti", "assistiti", "caregiver", "caregiver", "caregiver", "caregiver", "fondo")
GRIGIO2 <- "#9A9A9A"
col8   <- c(inps = GRIGIO2, assistiti = GRIGIO, caregiver = BLU, fondo = ROSSO)[tipo]

png(file.path("output", "grafici", "G08_ordini_di_grandezza_platea.png"),
    width = 10, height = 7.6, units = "in", res = 240, type = TIPO_PNG, bg = SFONDO)
par(family = "sans", mar = c(7, 18, 5.5, 2), mgp = c(3, 0.6, 0), las = 1, cex.axis = 0.85, bty = "n")
b <- barplot(rev(valori) / 1000, horiz = TRUE, col = rev(col8), border = NA, xlim = c(0, 2700),
             xaxt = "n", names.arg = rep("", length(valori)), space = 0.45)
abline(v = seq(500, 2500, 500), col = GRIGLIA, lwd = 0.6)
barplot(rev(valori) / 1000, horiz = TRUE, col = rev(col8), border = NA, add = TRUE, axes = FALSE,
        names.arg = rep("", length(valori)), space = 0.45)
axis(1, at = seq(0, 2500, 500), labels = it(seq(0, 2500, 500)), col = NA, col.ticks = NA)
mtext(rev(voci), side = 2, at = b, line = 0.6, cex = 0.78, adj = 1, padj = 0.4)
text(rev(valori) / 1000 + 30, b, it(rev(valori)), adj = 0, cex = 0.85, font = 2)
mtext("Migliaia di persone", side = 1, line = 2.4, cex = 0.85, col = TESTO2)
legend("bottomright", inset = c(0, 0.03), bty = "n", cex = 0.8, pch = 15, pt.cex = 1.6,
       col = c(GRIGIO2, GRIGIO, BLU, ROSSO),
       legend = c("persone assistite, dato amministrativo (INPS)", "persone assistite, indagine (ISTAT AVQ)",
                  "persone che assistono (relazione tecnica, su EHIS 2019)", "capienza del fondo"))
xs <- grconvertX(0.02, from = "ndc", to = "user")   # bordo sinistro della figura
mtext("Chi potrebbe ricevere il contributo: gli ordini di grandezza a confronto", side = 3, line = 3.2, at = xs, adj = 0, font = 2, cex = 1.15)
mtext("Dalle indennità di accompagnamento in essere ai contributi che il fondo può pagare per intero.",
      side = 3, line = 1.6, at = xs, adj = 0, cex = 0.85, col = TESTO2)
mtext(paste0("Fonte: INPS, Coordinamento generale statistico attuariale, Analisi delle pensioni di invalidità civile, 27/1/2026 (3.414.007 prestazioni, di cui 2.361.982 indennità);\n",
             "prima esplorazione dei microdati ISTAT AVQ 2024 (persone assistite); relazione tecnica all'A.C. 2789, 6/2/2026, p. 24 (chi assiste, da EHIS 2019 e DSU INPS);\n",
             "A.C. 2789-A, art. 14, per il fondo. Le barre grigie contano chi è assistito, quelle blu chi assiste: ordini di grandezza, non la stessa misura. Le 91 ore non sono osservate da nessuna indagine."),
      side = 1, line = 5.6, at = xs, adj = 0, cex = 0.68, col = TESTO2)
dev.off()

cat("Grafici salvati in output/grafici: G06, G07, G08\n")
