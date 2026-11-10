# 11_ehis_ore_lavoro.R - Figura per il paragrafo 1.1: chi assiste, per ore settimanali
# di assistenza e condizione lavorativa (EHIS 2019). Più ore si fanno, meno si lavora.
# Entrata: dati/grezzi/EHIS/EHIS_2019_IT.zip (stessa lettura dello script 99_ehis_caregiver.R)
# Uscita:  output/tabelle/T20_ehis_ore_e_lavoro.csv
#          output/grafici/G11_ehis_ore_e_lavoro.png
library(data.table)
source("script/00_funzioni_comuni.R")

PERIODO_EHIS <- "anno 2019 (Indagine europea sulla salute, persone di 15 anni e più)"
FONTE_EHIS   <- "elaborazione su microdati ISTAT, Indagine europea sulla salute (EHIS) 2019, file ad uso pubblico"
TIPO_PNG <- if (Sys.info()[["sysname"]] == "Darwin") "quartz" else "cairo"   # il Mac non ha cairo
SFONDO <- "#f4f4f1"   # fondo neutro chiaro di tutte le figure del paper, perché il testo risalti (10/11/2026)

# --- 1. Lettura del file dentro lo zip (come nello script 99) -----------------
zip_ehis <- "dati/grezzi/EHIS/EHIS_2019_IT.zip"
if (!file.exists(zip_ehis)) stop("Manca ", zip_ehis, ": copia EHIS_2019_IT.zip in dati/grezzi/EHIS/")
nome_txt <- grep("MICRODATI/.*\\.txt$", unzip(zip_ehis, list = TRUE)$Name, value = TRUE)[1]
cartella_tmp <- tempdir()
unzip(zip_ehis, files = nome_txt, exdir = cartella_tmp)
ehis <- fread(file.path(cartella_tmp, nome_txt), sep = "\t", colClasses = "character", encoding = "Latin-1",
              select = c("WGT", "MAINSTAT", "IC1", "IC3"))
setnames(ehis, c("peso", "condizione", "assiste", "ore"))
ehis[, peso := as.numeric(sub(",", ".", peso, fixed = TRUE))]
for (v in c("condizione", "assiste", "ore")) set(ehis, j = v, value = as.integer(trimws(ehis[[v]])))

# --- 2. Tabella: caregiver per ore e condizione (migliaia) --------------------
ehis[, condizione_nome := factor(condizione, c(10, 20, 30, 80),
                                 c("Occupati", "Disoccupati", "Ritirati dal lavoro", "Altra condizione"))]
ehis[, ore_nome := factor(ore, 1:3, c("Meno di 10 ore", "Da 10 a 19 ore", "20 ore o più"))]
tab <- ehis[assiste %in% 1 & !is.na(ore_nome) & !is.na(condizione_nome),
            .(migliaia = round(sum(peso) / 1000)), by = .(ore_nome, condizione_nome)]
T20 <- dcast(tab, ore_nome ~ condizione_nome, value.var = "migliaia", fill = 0)
T20[, Totale := rowSums(.SD), .SDcols = -1]
T20[, occupati_perc := round(100 * Occupati / Totale, 1)]
print(T20)
write.csv2(nota_periodo(T20, PERIODO_EHIS, FONTE_EHIS), "output/tabelle/T20_ehis_ore_e_lavoro.csv",
           row.names = FALSE, fileEncoding = "UTF-8")

# --- 3. Grafico: barre orizzontali al 100% -----------------------------------
m <- as.matrix(T20[, .(Occupati, Disoccupati, `Ritirati dal lavoro`, `Altra condizione`)])
rownames(m) <- paste0(T20$ore_nome, "\n(", formatC(T20$Totale, big.mark = ".", format = "d"), " mila)")
quote <- 100 * m / rowSums(m)
colori <- c("#2166AC", "#92C5DE", "#B2182B", "#BDBDBD")

png("output/grafici/G11_ehis_ore_e_lavoro.png", width = 10, height = 5.8, units = "in", res = 240, type = TIPO_PNG, bg = SFONDO)
par(family = "sans", mar = c(7.5, 11, 6, 1.5), las = 1, bty = "n", cex.axis = 0.85)
b <- barplot(t(quote), horiz = TRUE, col = colori, border = NA, xlim = c(0, 100),
             xlab = "Quota di chi assiste, per condizione lavorativa (%)", space = 0.5)
# etichetta della quota di occupati dentro la barra
text(quote[, 1] / 2, b, paste0(round(quote[, 1]), "%"), col = "white", font = 2, cex = 0.95)
legend("bottom", inset = c(0, -0.42), horiz = TRUE, bty = "n", fill = colori, border = NA, cex = 0.8,
       legend = colnames(m), xpd = TRUE)
mtext("Più ore di assistenza, meno lavoro: chi assiste 20 ore o più è occupato in un caso su tre",
      side = 3, line = 3.8, adj = 0, font = 2, cex = 1.1)
mtext("Persone di 15 anni e più che prestano cure o assistenza almeno una volta a settimana,",
      side = 3, line = 2.0, adj = 0, cex = 0.85, col = "#555555")
mtext("per ore settimanali dichiarate. Italia, 2019. Tra parentesi il numero di persone.",
      side = 3, line = 0.9, adj = 0, cex = 0.85, col = "#555555")
mtext(paste0("Fonte: ", FONTE_EHIS, ". Esclusi i casi con ore non indicate."),
      side = 1, line = 6.3, adj = 0, cex = 0.68, col = "#555555")
dev.off()
cat("Scritti output/tabelle/T20_ehis_ore_e_lavoro.csv e output/grafici/G11_ehis_ore_e_lavoro.png\n")
