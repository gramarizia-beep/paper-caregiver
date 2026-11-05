# =============================================================================
# 12_mappa_limitazioni_gravi.R
# Mappa per il paragrafo 1.1 della tesina: persone con limitazioni gravi per
# regione, in percentuale dei residenti, microdati ISTAT AVQ 2014-2024 uniti
# (con un solo anno le percentuali regionali oscillerebbero troppo).
# Legge la tabella M01 prodotta dallo script 92_mappa_disabilita.R: non servono
# i microdati, basta output/tabelle/M01_gravi_per_regione_eta.csv.
# Confini: dati/grezzi/geo/regioni_italia.geojson (openpolis, da ISTAT).
# Uscita: output/grafici/G12_mappa_limitazioni_gravi.png
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(sf); library(ggplot2) })
source("script/00_funzioni_comuni.R")

TIPO_PNG <- if (Sys.info()[["sysname"]] == "Darwin") "quartz" else "cairo"   # il Mac non ha cairo
it1 <- function(x) format(round(x, 1), decimal.mark = ",", nsmall = 1)

# ---- 1. La tabella M01 e i confini ------------------------------------------
m01 <- fread("output/tabelle/M01_gravi_per_regione_eta.csv", sep = ";", dec = ",", encoding = "UTF-8")
mappa <- st_read("dati/grezzi/geo/regioni_italia.geojson", quiet = TRUE)
mappa$regione <- sub("/.*", "", mappa$reg_name)                 # "Valle d'Aosta/Vallée d'Aoste" -> "Valle d'Aosta"
mappa <- merge(mappa, m01[, .(regione, gravi_pct, gravi_migliaia)], by = "regione")
centri <- cbind(st_drop_geometry(mappa)[, c("regione", "gravi_pct")],
                st_coordinates(suppressWarnings(st_point_on_surface(mappa))))
# Italia: media ponderata delle regioni (persone gravi / residenti), uguale al 5,1% degli script 90-92
italia <- m01[, round(100 * sum(gravi_migliaia) / sum(gravi_migliaia / (gravi_pct / 100)), 1)]
alto  <- m01[which.max(gravi_pct)]; basso <- m01[which.min(gravi_pct)]

# ---- 2. La mappa --------------------------------------------------------------
g <- ggplot(mappa) +
  geom_sf(aes(fill = gravi_pct), colour = "white", linewidth = 0.35) +
  geom_text(data = centri, aes(X, Y, label = paste0(it1(gravi_pct), "%")), size = 3.1,
            fontface = "bold", colour = ifelse(centri$gravi_pct >= 6.5, "white", "black")) +
  scale_fill_gradient(low = "#FDE0DD", high = "#99000D", name = "% dei residenti",
                      labels = function(x) paste0(format(x, decimal.mark = ","), "%")) +
  labs(title = "Persone con limitazioni gravi, per regione",
       subtitle = paste0("Percentuale dei residenti che dichiara limitazioni gravi da almeno sei mesi, anni 2014-2024 uniti.\n",
                         "Italia ", it1(italia), "%; ", alto$regione, " ", it1(alto$gravi_pct), "%, ", basso$regione, " ", it1(basso$gravi_pct), "%"),
       caption = didascalia(PERIODO_UNITI)) +
  theme_void(base_family = "sans", base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0, margin = margin(b = 5)),
        plot.subtitle = element_text(colour = "#555555", size = 9.5, hjust = 0, lineheight = 1.15, margin = margin(b = 6)),
        plot.caption = element_text(hjust = 0, size = 8, colour = "#555555", margin = margin(t = 6)),
        plot.title.position = "plot", plot.caption.position = "plot",
        legend.position = "right", legend.title = element_text(size = 9), legend.text = element_text(size = 8.5),
        plot.margin = margin(8, 8, 8, 8))

dir.create("output/grafici", showWarnings = FALSE, recursive = TRUE)
png("output/grafici/G12_mappa_limitazioni_gravi.png", width = 8, height = 8.6, units = "in", res = 240, type = TIPO_PNG)
print(g)
dev.off()
cat("Salvato output/grafici/G12_mappa_limitazioni_gravi.png. Italia:", it1(italia), "%\n")
