# =============================================================================
# 10_platea_regioni.R
# Dove sta la platea potenziale del contributo per il caregiver prevalente?
# Due indicatori per regione, messi su una mappa e in una classifica:
#   1) persone con limitazioni gravi che convivono con un adulto senza lavoro,
#      per 1.000 residenti (microdati ISTAT AVQ, anni 2022-2024 uniti per avere
#      campioni regionali meno ballerini; stesse definizioni degli script 02 e 05);
#   2) pensionati di invalidità civile per 100 residenti (ISTAT, Casellario
#      centrale dei pensionati, anno 2022: dato amministrativo, conteggio completo).
# Il contributo è una somma uguale in tutta Italia, pagata dall'INPS a chi ha i
# requisiti: la mappa mostra quindi dove sono concentrati i potenziali aventi
# diritto, non una ripartizione decisa per regione.
#
# Serve: data.table, sf, ggplot2 (già usati dagli script 01-05 e 92).
# File: dati/grezzi/AVQ/AVQ_2022_IT.zip ... AVQ_2024_IT.zip
#       dati/grezzi/geo/regioni_italia.geojson (openpolis, da ISTAT)
#       dati/grezzi/pensioni/pensionati_invalidita_civile_per_100_residenti.csv
#         (ISTAT, dataflow 46_812_DF_DCAR_PENSIONATI2_4, scaricato con lo script 96)
# Output: output/tabelle/T19_platea_potenziale_per_regione.csv
#         output/grafici/G09_mappa_platea_potenziale.png
#         output/grafici/G10_classifica_platea_potenziale.png
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(sf); library(ggplot2); library(grid) })
source("script/00_funzioni_comuni.R")

ANNI_MAPPA <- 2022:2024
TIPO_PNG   <- if (Sys.info()[["sysname"]] == "Darwin") "quartz" else "cairo"
SFONDO <- "#f4f4f1"   # fondo neutro chiaro di tutte le figure del paper, perché il testo risalti (10/11/2026)

# ---- 1. Lettura dei microdati AVQ 2022-2024 (stesse regole dello script 01) ---
VARIABILI <- list(famiglia = "PROFAM", parentela = "RELPAR", eta_cl = "ETAMi",
                  condizione = "CONDMi", regione = c("REGMf", "REGMF", "REGMiF", "REGMif"),
                  peso = "COEFIN", limitazioni = "LIMITA", risorse = "RISEC")

leggi_anno_mappa <- function(anno) {
  zip <- list.files("dati/grezzi/AVQ", pattern = paste0("^AVQ_", anno, "_IT\\.zip$"),
                    recursive = TRUE, full.names = TRUE)[1]
  if (is.na(zip)) stop("Non trovo AVQ_", anno, "_IT.zip in dati/grezzi/AVQ")
  interno <- grep("^MICRODATI/.*\\.txt$", unzip(zip, list = TRUE)$Name, value = TRUE)[1]
  tmp <- tempfile("avq_"); unzip(zip, files = interno, exdir = tmp)
  f <- file.path(tmp, interno)
  colonne <- names(fread(f, nrows = 0))
  scelta <- vapply(VARIABILI, function(alt) { t <- colonne[toupper(colonne) %in% toupper(alt)]
                                              if (length(t)) t[1] else NA_character_ }, character(1))
  d <- fread(f, select = unname(scelta), colClasses = "character", encoding = "Latin-1", showProgress = FALSE)
  setnames(d, unname(scelta), names(scelta)); unlink(tmp, recursive = TRUE)
  for (v in setdiff(names(d), "famiglia")) d[[v]] <- suppressWarnings(as.numeric(trimws(d[[v]])))
  d[, peso := peso / 10000]
  d[, famiglia := paste(anno, famiglia, sep = "_")]
  d[, anno := anno]
  message(anno, ": ", format(nrow(d), big.mark = "."), " persone, popolazione stimata ",
          format(round(sum(d$peso) / 1e6, 2), decimal.mark = ","), " milioni")
  d
}
avq <- rbindlist(lapply(ANNI_MAPPA, leggi_anno_mappa), use.names = TRUE, fill = TRUE)

# ---- 2. Le stesse definizioni degli script 02 e 05 ----------------------------
avq[, grave     := grave1(limitazioni)]
avq[, minorenne := eta_cl <= 6]                                   # ETAMi 1-6 = fino a 17 anni
avq[, occupato  := fifelse(condizione %in% 1:3, as.numeric(condizione == 1), NA_real_)]
avq[, adulto_ng := !grave & !minorenne & !is.na(eta_cl)]           # adulto senza limitazioni gravi
avq[, `:=`(fam_adulto_ng        = any(adulto_ng),
           fam_adulto_ng_nonocc = any(adulto_ng & occupato %in% 0),
           risorse_scarse       = any(risorse %in% 3:4)), by = famiglia]
# codice regione ISTAT: in AVQ è moltiplicato per 10 (10 Piemonte ... 200 Sardegna; 41 e 42 = Trentino-Alto Adige)
avq[, cod_reg := ifelse(regione %in% c(41, 42), 4, regione %/% 10)]
avq <- avq[cod_reg %in% 1:20]

reg_avq <- avq[, .(
  residenti_migliaia      = sum(peso) / length(ANNI_MAPPA) / 1000,
  gravi_migliaia          = sum(peso * grave) / length(ANNI_MAPPA) / 1000,
  platea_migliaia         = sum(peso * (grave & fam_adulto_ng_nonocc)) / length(ANNI_MAPPA) / 1000,
  platea_scarse_migliaia  = sum(peso * (grave & fam_adulto_ng_nonocc & risorse_scarse)) / length(ANNI_MAPPA) / 1000,
  gravi_per_100           = 100 * sum(peso * grave) / sum(peso),
  platea_per_1000         = 1000 * sum(peso * (grave & fam_adulto_ng_nonocc)) / sum(peso),
  n_campione_platea       = sum(grave & fam_adulto_ng_nonocc)
), by = cod_reg]

# ---- 3. Pensionati di invalidità civile per 100 residenti (Casellario, 2022) -----
# Il Casellario usa i codici europei NUTS; li traduco nel codice regione ISTAT.
nuts <- data.table(
  REF_AREA = c("ITC1","ITC2","ITC4","ITD1","ITDA","ITD3","ITD4","ITC3","ITD5","ITE1","ITE2","ITE3","ITE4",
               "ITF1","ITF2","ITF3","ITF4","ITF5","ITF6","ITG1","ITG2"),
  cod_reg  = c(1, 2, 3, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
cas <- fread("dati/grezzi/pensioni/pensionati_invalidita_civile_per_100_residenti.csv")
cas <- cas[DATA_TYPE == "P_RSPER" & PENSION_TYPE == "CIVDIS" & SEX == 9 & AGE == "TOTAL"]
ANNO_CASELLARIO <- max(cas$TIME_PERIOD)
cas <- merge(cas[TIME_PERIOD == ANNO_CASELLARIO, .(REF_AREA, invciv_per_100 = OBS_VALUE)], nuts, by = "REF_AREA")
# La Valle d'Aosta e le province autonome di Trento e Bolzano erogano in proprio le prestazioni
# di invalidita' civile (non l'INPS, che vi svolge solo l'accertamento sanitario): nel Casellario
# risultano sottostimate. Per il Trentino-Alto Adige faccio la media delle due province e lo segnalo.
# Le altre regioni a statuto speciale (Friuli-Venezia Giulia, Sicilia, Sardegna) sono gestite dall'INPS.
cas <- cas[, .(invciv_per_100 = mean(invciv_per_100)), by = cod_reg]
italia_invciv <- fread("dati/grezzi/pensioni/pensionati_invalidita_civile_per_100_residenti.csv")[
  REF_AREA == "IT" & DATA_TYPE == "P_RSPER" & PENSION_TYPE == "CIVDIS" & SEX == 9 & AGE == "TOTAL" &
  TIME_PERIOD == ANNO_CASELLARIO, OBS_VALUE]

# ---- 4. Tabella T19 ---------------------------------------------------------------
mappa <- st_read("dati/grezzi/geo/regioni_italia.geojson", quiet = TRUE)
mappa$cod_reg <- mappa$reg_istat_code_num
mappa$regione <- sub("/.*", "", mappa$reg_name)
nomi <- as.data.table(st_drop_geometry(mappa))[, .(cod_reg, regione)]
ripartizione <- data.table(cod_reg = 1:20,
  ripartizione = c(rep("Nord", 8), rep("Centro", 4), rep("Mezzogiorno", 8)))
# 1 Piemonte, 2 Valle d'Aosta, 3 Lombardia, 4 Trentino-A.A., 5 Veneto, 6 Friuli-V.G., 7 Liguria, 8 Emilia-Romagna,
# 9 Toscana, 10 Umbria, 11 Marche, 12 Lazio, 13-20 Abruzzo, Molise, Campania, Puglia, Basilicata, Calabria, Sicilia, Sardegna

T19 <- Reduce(function(a, b) merge(a, b, by = "cod_reg"), list(nomi, ripartizione, reg_avq, cas))
T19[, quota_residenti_pct := 100 * residenti_migliaia / sum(residenti_migliaia)]
T19[, quota_platea_pct    := 100 * platea_migliaia / sum(platea_migliaia)]
T19[, rapporto_platea_su_residenti := quota_platea_pct / quota_residenti_pct]
setorder(T19, -platea_per_1000)
T19_out <- T19[, .(regione, ripartizione,
  residenti_migliaia = round(residenti_migliaia), gravi_migliaia = round(gravi_migliaia),
  gravi_per_100 = round(gravi_per_100, 1),
  platea_migliaia = round(platea_migliaia), platea_per_1000 = round(platea_per_1000, 1),
  platea_scarse_migliaia = round(platea_scarse_migliaia),
  quota_residenti_pct = round(quota_residenti_pct, 1), quota_platea_pct = round(quota_platea_pct, 1),
  invciv_per_100, n_campione_platea)]
dir.create("output/tabelle", showWarnings = FALSE, recursive = TRUE)
dir.create("output/grafici", showWarnings = FALSE, recursive = TRUE)
fwrite(nota_periodo(T19_out, paste0("AVQ: anni ", min(ANNI_MAPPA), "-", max(ANNI_MAPPA),
                    " uniti in un unico campione pesato, le migliaia sono una media annua; stime campionarie. ",
                    "Casellario: pensionati di invalidita' civile al 31/12/", ANNO_CASELLARIO,
                    " per 100 residenti (Valle d'Aosta e Trentino-Alto Adige, media di Trento e Bolzano: prestazioni erogate dagli enti territoriali e non dall'INPS, sottostimate)"),
                    paste(FONTE_ISTAT, "e ISTAT, Casellario centrale dei pensionati (dataflow 46_812)")),
       "output/tabelle/T19_platea_potenziale_per_regione.csv", sep = ";", dec = ",", bom = TRUE)

# Per ripartizione: quanta parte della platea potenziale sta nel Mezzogiorno?
T19_rip <- T19[, .(residenti_pct = round(100 * sum(residenti_migliaia) / sum(T19$residenti_migliaia), 1),
                   platea_pct    = round(100 * sum(platea_migliaia) / sum(T19$platea_migliaia), 1),
                   platea_per_1000 = round(1000 * sum(platea_migliaia) / sum(residenti_migliaia), 1)),
               by = ripartizione]
print(T19_out); print(T19_rip)

# ---- 5. G09: la mappa, due pannelli ---------------------------------------------
mappa <- merge(mappa, T19[, .(cod_reg, platea_per_1000, invciv_per_100)], by = "cod_reg")
centri <- cbind(st_drop_geometry(mappa)[, c("cod_reg", "regione", "platea_per_1000", "invciv_per_100")],
                st_coordinates(suppressWarnings(st_point_on_surface(mappa))))
it1 <- function(x) format(round(x, 1), decimal.mark = ",", nsmall = 1)
BASSO <- "#FDE0DD"; ALTO <- "#99000D"
tema_mappa <- theme_void(base_family = "sans") +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 8.5, colour = "#555555", hjust = 0, margin = margin(b = 4)),
        legend.position = "bottom", legend.title = element_text(size = 8), legend.text = element_text(size = 7.5),
        legend.key.height = unit(0.3, "cm"), legend.key.width = unit(1.1, "cm"),
        plot.margin = margin(4, 4, 4, 4))
p1 <- ggplot(mappa) +
  geom_sf(aes(fill = platea_per_1000), colour = "white", linewidth = 0.3) +
  geom_text(data = centri, aes(X, Y, label = it1(platea_per_1000)), size = 2.6,
            colour = ifelse(centri$platea_per_1000 > 28, "white", "black")) +
  scale_fill_gradient(low = BASSO, high = ALTO, name = "per 1.000 residenti") +
  labs(title = "A. Persone con limitazioni gravi che convivono\n    con un adulto senza lavoro",
       subtitle = paste0("per 1.000 residenti, microdati AVQ ", min(ANNI_MAPPA), "-", max(ANNI_MAPPA),
                         " (Italia: ", it1(1000 * sum(T19$platea_migliaia) / sum(T19$residenti_migliaia)), ")")) +
  tema_mappa
p2 <- ggplot(mappa) +
  geom_sf(aes(fill = invciv_per_100), colour = "white", linewidth = 0.3) +
  geom_text(data = centri, aes(X, Y, label = it1(invciv_per_100)), size = 2.6,
            colour = ifelse(centri$invciv_per_100 > 6.5, "white", "black")) +
  scale_fill_gradient(low = BASSO, high = ALTO, name = "per 100 residenti") +
  labs(title = "B. Pensionati di invalidità civile\n    (dato amministrativo)",
       subtitle = paste0("per 100 residenti, Casellario dei pensionati ", ANNO_CASELLARIO,
                         " (Italia: ", it1(italia_invciv), ")")) +
  tema_mappa

png("output/grafici/G09_mappa_platea_potenziale.png", width = 10, height = 8.4, units = "in", res = 240, type = TIPO_PNG, bg = SFONDO)
grid.newpage()
grid.text("Dove sta la platea potenziale del contributo: una geografia del Mezzogiorno",
          x = 0.02, y = 0.965, just = "left", gp = gpar(fontsize = 14, fontface = "bold", fontfamily = "sans"))
grid.text(paste0("Due misure indipendenti, un'indagine e un archivio amministrativo, danno la stessa mappa. ",
                 "Il contributo è uguale in tutta Italia: va dove sono i potenziali aventi diritto."),
          x = 0.02, y = 0.935, just = "left", gp = gpar(fontsize = 9, col = "#555555", fontfamily = "sans"))
print(p1, vp = viewport(x = 0.26, y = 0.615, width = 0.48, height = 0.60))
print(p2, vp = viewport(x = 0.74, y = 0.615, width = 0.48, height = 0.60))
# Riquadro "come leggere"
grid.rect(x = 0.02, y = 0.085, width = 0.96, height = 0.205, just = c("left", "bottom"),
          gp = gpar(fill = "white", col = NA))   # bianco sul fondo neutro
grid.text("COME LEGGERE LA MAPPA", x = 0.035, y = 0.275, just = c("left", "top"),
          gp = gpar(fontsize = 8.5, fontface = "bold", fontfamily = "sans", col = "#333333"))
testo_lettura <- paste0(
  "Il contributo previsto dal disegno di legge (fino a 400 € al mese, 1.200 a trimestre) spetta solo al caregiver prevalente: convivente con la persona assistita,\n",
  "con almeno 91 ore di assistenza a settimana, senza lavoro o con un reddito da lavoro fino a 3.000 € l'anno, con ISEE del nucleo fino a 25.000 €\n",
  "(importo pieno fino a 15.000 €, poi ridotto in proporzione fino a zero a 25.000 €). L'importo effettivo è fissato ogni trimestre dentro un fondo di 251 milioni l'anno.\n",
  "\n",
  "Nessuna indagine rileva le ore di assistenza né l'ISEE. Il pannello A conta quindi le famiglie in cui potrebbe esserci un caregiver prevalente: le persone con\n",
  "limitazioni gravi che vivono con un adulto senza lavoro, ogni 1.000 residenti della regione. Il pannello B conta, ogni 100 residenti, chi riceve una pensione di\n",
  "invalidità civile, cioè le persone con disabilità riconosciute dallo Stato. Più il colore è scuro, più persone potenzialmente interessate ci sono rispetto alla\n",
  "popolazione. Esempio: in Sardegna 34 persone ogni 1.000 residenti, in Lombardia 18, in Italia 23. Il Mezzogiorno ha il ",
  T19_rip[ripartizione == "Mezzogiorno", round(residenti_pct)], "% dei residenti e il ",
  T19_rip[ripartizione == "Mezzogiorno", round(platea_pct)], "% della platea potenziale.")
grid.text(testo_lettura, x = 0.035, y = 0.248, just = c("left", "top"),
          gp = gpar(fontsize = 7.6, fontfamily = "sans", col = "#333333", lineheight = 1.15))
grid.text(paste0("Fonte: elaborazione su microdati ISTAT AVQ ", min(ANNI_MAPPA), "-", max(ANNI_MAPPA),
                 " uniti (stime campionarie, prima esplorazione; stesse definizioni dell'imbuto sulla platea) e ISTAT, ",
                 "Casellario centrale dei pensionati, ", ANNO_CASELLARIO, ".\n",
                 "Requisiti del contributo: A.C. 2789-A (Camera, 14/10/2026), artt. 2 e 14. Confini: ISTAT via openpolis.\n",
                 "Pannello B: in Valle d'Aosta e nelle province autonome di Trento e Bolzano l'invalidità civile è erogata dagli enti territoriali e non dall'INPS, quindi i valori ",
                 "sono sottostimati\n(Trentino-Alto Adige: media delle due province). Friuli-Venezia Giulia, Sicilia e Sardegna, pur a statuto speciale, sono gestite dall'INPS."),
          x = 0.02, y = 0.012, just = c("left", "bottom"), gp = gpar(fontsize = 7.2, col = "#555555", fontfamily = "sans"))
dev.off()

# ---- 6. G10: la classifica delle regioni, con il peso di ciascuna sul totale ------
ord <- T19[order(platea_per_1000)]
ord[, regione_f := factor(regione, levels = regione)]
ord[, colore := fifelse(ripartizione == "Mezzogiorno", "#B2182B", fifelse(ripartizione == "Centro", "#9A9A9A", "#4D4D4D"))]
lungo <- rbind(
  ord[, .(regione_f, colore, pannello = "A. Persone con limitazioni gravi conviventi con un adulto senza lavoro,\nper 1.000 residenti (AVQ 2022-2024)", valore = platea_per_1000,
          etichetta = paste0(it1(platea_per_1000), "  (", format(round(platea_migliaia), big.mark = ".", decimal.mark = ",", trim = TRUE), " mila)"))],
  ord[, .(regione_f, colore, pannello = paste0("B. Pensionati di invalidità civile per 100 residenti\n(Casellario, ", ANNO_CASELLARIO, ")"), valore = invciv_per_100,
          etichetta = it1(invciv_per_100))])
media_it <- data.table(pannello = unique(lungo$pannello),
                       valore = c(1000 * sum(T19$platea_migliaia) / sum(T19$residenti_migliaia), italia_invciv))
g10 <- ggplot(lungo, aes(x = valore, y = regione_f)) +
  geom_col(aes(fill = colore), width = 0.72) +
  geom_vline(data = media_it, aes(xintercept = valore), linetype = "dashed", colour = "#B2182B", linewidth = 0.4) +
  geom_text(aes(label = etichetta), hjust = -0.08, size = 2.6) +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
  facet_wrap(~ pannello, scales = "free_x") +
  labs(title = "Le regioni in classifica: la platea potenziale è più fitta nel Mezzogiorno",
       subtitle = "In rosso le regioni del Mezzogiorno, in grigio chiaro il Centro, in grigio scuro il Nord. La linea tratteggiata è il valore dell'Italia.",
       x = NULL, y = NULL,
       caption = paste0("Come leggere: il contributo (fino a 400 € al mese) spetta al caregiver convivente con almeno 91 ore a settimana, senza lavoro, con ISEE fino a 25.000 €. Le indagini non rilevano\n",
                        "ore e ISEE: il pannello A conta le persone con limitazioni gravi che vivono con un adulto senza lavoro, cioè le famiglie in cui può esserci un caregiver prevalente, ogni 1.000\n",
                        "residenti della regione (tra parentesi quante sono, media annua in migliaia); il pannello B conta chi riceve una pensione di invalidità civile ogni 100 residenti.\n",
                        "\nFonte: elaborazione su microdati ISTAT AVQ ", min(ANNI_MAPPA), "-", max(ANNI_MAPPA),
                        " uniti (stime campionarie, prima esplorazione) e ISTAT, Casellario centrale dei pensionati, ", ANNO_CASELLARIO, ".\n",
                        "Pannello B: Valle d'Aosta e Trentino-Alto Adige (media di Trento e Bolzano) erogano in proprio l'invalidità civile, non tramite INPS: valori sottostimati.")) +
  theme_minimal(base_family = "sans", base_size = 10) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "#DDDDDD", linewidth = 0.3),
        strip.text = element_text(hjust = 0, size = 8.6, face = "bold"),
        plot.title = element_text(face = "bold", size = 13), plot.subtitle = element_text(colour = "#555555", size = 9),
        plot.caption = element_text(hjust = 0, colour = "#555555", size = 7.5, margin = margin(t = 10)),
        plot.title.position = "plot", plot.caption.position = "plot", axis.text.y = element_text(size = 8.5),
        plot.background = element_rect(fill = SFONDO, colour = NA))   # ggplot2 4 mette il fondo bianco: qui lo stesso fondo neutro delle altre figure
ggsave("output/grafici/G10_classifica_platea_potenziale.png", g10, width = 10, height = 7.3, units = "in", dpi = 240, bg = SFONDO)

cat("\nSalvati: T19, G09, G10. Il Mezzogiorno ha il", T19_rip[ripartizione == "Mezzogiorno", residenti_pct],
    "% dei residenti e il", T19_rip[ripartizione == "Mezzogiorno", platea_pct], "% della platea potenziale (AVQ).\n")
