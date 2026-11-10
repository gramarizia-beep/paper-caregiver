# =============================================================================
# figura1_caregiver_per_fonte.R
# Figura 1 del paper: quanti sono i caregiver in Italia secondo le diverse
# fonti ISTAT (EHIS 2015 e 2019, Famiglie e soggetti sociali 2024, AVQ 2019 e
# 2024, Conciliazione 2018). I numeri sono quelli pubblicati dall'ISTAT nelle
# tavole e nei report citati nel paper; i soli valori calcolati da noi sono
# quelli dell'AVQ (microdati, script 90-92) e l'1,3 milioni di Famiglie e
# soggetti sociali (29,3% di 4,4 milioni). Le barre tratteggiate li segnalano.
# Non servono microdati né pacchetti: solo R di base.
# Uscita: output/grafici/Figura1_caregiver_per_fonte.png
#         e, se esiste la cartella figure/, figure/Figura 1 - caregiver per fonte ISTAT.png
# =============================================================================

TIPO_PNG <- if (Sys.info()[["sysname"]] == "Darwin") "quartz" else "cairo"   # il Mac non ha cairo

DARK  <- "#1f4e79"; MID <- "#6f93bf"; INK <- "#222222"; MUTED <- "#555555"
GIALLO <- "#fff7cc"; BORDO_GIALLO <- "#e6d98a"
SFONDO <- "#f4f4f1"   # fondo neutro, chiaro, perché il testo risalti (richiesta del 10/11)

# ---- 1. I dati: per ogni fonte un titolo e le righe -------------------------
# Ogni riga: etichetta, valore in milioni, livello (0 = totale, 1 = "di cui"),
# stimato da noi (TRUE = barra tratteggiata), etichetta del valore.
riga <- function(etichetta, valore, livello, stimato, testo)
  data.frame(etichetta = etichetta, valore = valore, livello = livello,
             stimato = stimato, testo = testo, stringsAsFactors = FALSE)

blocchi <- list(
  list(titolo = paste0("ISTAT, Indagine europea sulla salute (EHIS) 2015 – tavole del 19 ottobre 2017\n",
                       "Persone di 15 anni e più che forniscono cure o assistenza almeno una volta a settimana a persone\n",
                       "con problemi dovuti all'invecchiamento, a patologie croniche o a infermità"),
       righe = rbind(
         riga("totale (16,4% dei 15+)",                              8.555, 0, FALSE, "8,56 mln"),
         riga("   di cui prevalentemente a familiari",               7.293, 1, FALSE, "7,29 mln"),
         riga("   di cui per 20 ore o più a settimana (25,1%)", 2.146, 1, FALSE, "2,15 mln"))),
  list(titolo = paste0("ISTAT, Indagine europea sulla salute (EHIS) 2019 – tavole del 13 gennaio 2022\n",
                       "Stessa definizione del 2015"),
       righe = rbind(
         riga("totale (15,4% dei 15+)",                              7.992, 0, FALSE, "7,99 mln"),
         riga("   di cui prevalentemente a familiari",               7.014, 1, FALSE, "7,01 mln"),
         riga("   di cui per 20 ore o più a settimana (29,6%)", 2.367, 1, FALSE, "2,37 mln"))),
  list(titolo = paste0("ISTAT, Famiglie e soggetti sociali 2024 – report «Aiuti dati, ricevuti e reti di solidarietà», 21 luglio 2026\n",
                       "Persone di 18 anni e più che nelle quattro settimane precedenti l'intervista hanno prestato cura o assistenza"),
       righe = rbind(
         riga("caregiver in complesso, di coabitanti e/o non coabitanti (24,9% dei 18+)",        12.3,  0, FALSE, "12,3 mln"),
         riga("   di cui per 20 ore o più a settimana (31,9%, «circa 4 milioni»)", 4.0,  1, FALSE, "≈4,0 mln"),
         riga("   di cui caregiver di persone non coabitanti (19,4%)",                           9.6,   1, FALSE, "9,6 mln"),
         riga("   di cui «caregiver familiari», di un coabitante con problemi di salute (11,0%)", 4.435, 1, FALSE, "4,4 mln"),
         riga("      di cui per 20 ore o più a settimana (29,3% di 4,4 mln, calcolo nostro)", 1.30, 1, TRUE,  "1,3 mln"))),
  list(titolo = paste0("ISTAT, Aspetti della vita quotidiana (AVQ) 2019 e 2024 – nostra elaborazione sui microdati ad uso pubblico\n",
                       "Persone di 18 anni e più che vivono con un familiare con limitazioni gravi (domanda GALI): «caregiver potenziali»,\n",
                       "perché l'AVQ non chiede se e quanto si assiste"),
       righe = rbind(
         riga("2019: conviventi con un familiare con limitazioni gravi (7,3% dei 18+)", 3.674, 0, TRUE, "3,67 mln"),
         riga("   di cui senza limitazioni gravi loro stessi",                           3.204, 1, TRUE, "3,20 mln"),
         riga("2024: conviventi con un familiare con limitazioni gravi (6,3% dei 18+)", 3.124, 0, TRUE, "3,12 mln"),
         riga("   di cui senza limitazioni gravi loro stessi",                           2.763, 1, TRUE, "2,76 mln"))),
  list(titolo = paste0("ISTAT, Conciliazione tra lavoro e famiglia 2018 – report del 18 novembre 2019 (modulo ad hoc Forze di lavoro)\n",
                       "Persone di 18-64 anni che si prendono cura di parenti malati, disabili o anziani"),
       righe = riga("totale (7,7% dei 18-64)", 2.827, 0, FALSE, "2,83 mln"))
)

# ---- 2. Le posizioni verticali: si parte da 0 e si scende --------------------
y <- 0; intestazioni <- list(); righe <- NULL
for (b in blocchi) {
  intestazioni[[length(intestazioni) + 1]] <- list(y = y, titolo = b$titolo)
  y <- y - 1.75
  r <- b$righe; r$y <- y - (seq_len(nrow(r)) - 1)
  righe <- rbind(righe, r)
  y <- y - nrow(r) - 0.55
}
ymin <- y

# ---- 3. Il disegno ------------------------------------------------------------
disegna <- function() {
  # margini in pollici: sotto, sinistra (spazio per le etichette), sopra, destra
  par(mai = c(1.95, 7.28, 1.54, 0.42), xpd = NA, lheight = 1.25, family = "sans")
  plot.new()
  plot.window(xlim = c(0, 13.5), ylim = c(ymin, 0.6), xaxs = "i", yaxs = "i")

  # griglia verticale leggera, sotto le barre (solo dentro l'area del grafico)
  par(xpd = FALSE); abline(v = seq(0, 12, 2), col = "#dcdcd7", lwd = 1); par(xpd = NA)

  # linee di separazione tra i blocchi e intestazioni, dal bordo sinistro della figura
  x_sin <- grconvertX(0.02, "ndc", "user"); x_des <- grconvertX(0.985, "ndc", "user")
  for (h in intestazioni) {
    segments(x_sin, h$y + 0.45, x_des, h$y + 0.45, col = "#cfcfca", lwd = 1)
    text(x_sin, h$y, h$titolo, adj = c(0, 1), col = DARK, font = 2, cex = 0.95)
  }

  # le barre
  for (i in seq_len(nrow(righe))) {
    r <- righe[i, ]; col <- if (r$livello == 0) DARK else MID
    rect(0, r$y - 0.36, r$valore, r$y + 0.36, col = col, border = "white", lwd = 1)
    if (r$stimato)   # tratteggio bianco sopra la barra piena, come nella versione originale
      rect(0, r$y - 0.36, r$valore, r$y + 0.36, density = 14, angle = 45, col = "white", border = NA)
    text(-0.15, r$y, r$etichetta, adj = c(1, 0.5), col = INK, cex = 0.95)
    text(r$valore + 0.1, r$y, r$testo, adj = c(0, 0.5), col = INK, cex = 1,
         font = if (r$livello == 0) 2 else 1)
  }

  # asse orizzontale
  axis(1, at = seq(0, 12, 2), col = "#999999", col.axis = INK, cex.axis = 1, lwd = 1, padj = -0.3)
  mtext("Milioni di persone", side = 1, line = 1.8, col = MUTED, cex = 1)

  # titolo e sottotitolo
  text(grconvertX(0.02, "ndc", "user"), grconvertY(0.985, "ndc", "user"),
       "Quanti sono i caregiver in Italia? Dipende da chi conta e da come",
       adj = c(0, 1), font = 2, cex = 16 / 11, col = INK)
  text(grconvertX(0.02, "ndc", "user"), grconvertY(0.955, "ndc", "user"),
       "Stime ISTAT a confronto: ogni indagine usa una definizione, un'età minima e un periodo di riferimento diversi",
       adj = c(0, 1), cex = 11.5 / 11, col = MUTED)

  # legenda in alto, sotto il sottotitolo, su fondo giallino
  yl <- grconvertY(0.921, "ndc", "user"); alt <- grconvertY(0.935, "ndc", "user") - grconvertY(0.907, "ndc", "user")
  rect(grconvertX(0.015, "ndc", "user"), yl - alt / 2, grconvertX(0.618, "ndc", "user"), yl + alt / 2,
       col = GIALLO, border = BORDO_GIALLO, lwd = 1)
  chiave <- function(x_ndc, testo, col, tratteggio = FALSE) {
    x0 <- grconvertX(x_ndc, "ndc", "user"); x1 <- grconvertX(x_ndc + 0.018, "ndc", "user")
    rect(x0, yl - alt * 0.18, x1, yl + alt * 0.18, col = col, border = NA)
    if (tratteggio) rect(x0, yl - alt * 0.18, x1, yl + alt * 0.18, density = 14, angle = 45, col = "white", border = NA)
    text(grconvertX(x_ndc + 0.024, "ndc", "user"), yl, testo, adj = c(0, 0.5), cex = 10 / 11, col = INK)
  }
  chiave(0.027, "totale della rilevazione", DARK)
  chiave(0.185, "sottoinsieme («di cui»)", MID)
  chiave(0.340, "valore calcolato da noi (non pubblicato dall'ISTAT)", MID, tratteggio = TRUE)

  # nota in calce, spezzata in righe
  nota <- paste0(
    "Fonti: ISTAT, tavole EHIS 2015 (19/10/2017) e EHIS 2019 (13/01/2022); «Aiuti dati, ricevuti e reti di solidarietà. Anno 2024», Statistiche Report, 21/07/2026; ",
    "«Conciliazione tra lavoro e famiglia. Anno 2018», Statistiche Report, 18/11/2019; microdati AVQ 2019 e 2024 ad uso pubblico (elaborazione propria).\n",
    "Le stime non sono confrontabili fra loro: ogni indagine usa una definizione, un'età minima e un periodo di riferimento diversi; l'AVQ conta chi vive con la persona ",
    "con limitazioni gravi, non chi la assiste. EHIS e Famiglie e soggetti sociali pubblicano le ore di assistenza in classi, l'ultima «20 e più», e l'AVQ non le chiede: ",
    "nessuna indagine permette quindi di individuare chi assiste per le 91 ore settimanali del disegno di legge, e la stessa relazione tecnica le stima per ipotesi.")
  righe_nota <- unlist(lapply(strsplit(nota, "\n")[[1]], strwrap, width = 210))
  text(grconvertX(0.02, "ndc", "user"), grconvertY(0.012, "ndc", "user"),
       paste(righe_nota, collapse = "\n"), adj = c(0, 0), cex = 9.2 / 11, col = MUTED)
}

# ---- 4. Salvataggio -----------------------------------------------------------
dir.create("output/grafici", showWarnings = FALSE, recursive = TRUE)
uscita <- "output/grafici/Figura1_caregiver_per_fonte.png"
png(uscita, width = 14, height = 16.2, units = "in", res = 200, pointsize = 11, type = TIPO_PNG, bg = SFONDO)
disegna()
dev.off()
if (dir.exists("figure")) file.copy(uscita, "figure/Figura 1 - caregiver per fonte ISTAT.png", overwrite = TRUE)
cat("Salvato", uscita, "\n")
