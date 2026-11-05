# 00_funzioni_comuni.R
# Funzioni usate da tutti gli script per dichiarare, in ogni tabella e grafico,
# QUALE PERIODO e QUALE METODO ci sono dietro i numeri. Viene caricato dagli
# altri script con source("script/00_funzioni_comuni.R").

# Se uno script deve installare un pacchetto, sa da dove prenderlo (serve quando si esegue da terminale)
options(repos = c(CRAN = "https://cloud.r-project.org"))

FONTE_ISTAT <- "Fonte: elaborazione su microdati ISTAT, Aspetti della vita quotidiana (AVQ), file ad uso pubblico"

# Testi standard dei periodi (usare sempre questi, così sono uguali ovunque)
PERIODO_UNITI    <- "anni 2014-2024 uniti in un unico campione pesato (non media di percentuali annue); stime campionarie"
PERIODO_PER_ANNO <- "un valore per ciascun anno dal 2014 al 2024; stime campionarie"
PERIODO_2024     <- "solo anno 2024; stime campionarie"
PERIODO_MEDIA    <- "persone stimate in un anno medio del periodo 2014-2024 (media annua); stime campionarie"
PERIODO_PRIMA_DOPO <- "prima = 2014-2022, dopo = 2023-2024 (D.Lgs. 105/2022); stime campionarie"
PERIODO_INPS     <- "ISTAT: anno 2014 (stime campionarie); INPS: prestazioni vigenti al 31-12-2013 (dati amministrativi, conteggio completo)"

# Aggiunge a una tabella le colonne 'periodo_dati' e 'fonte_dati' (in fondo, a destra).
# Si chiamano così per non confondersi con colonne dei dati come 'periodo' (es. T05).
nota_periodo <- function(x, periodo, fonte = FONTE_ISTAT) {
  x <- data.table::copy(data.table::as.data.table(x))
  # uso set() e non := perché, se la tabella ha già una colonna chiamata
  # 'periodo' (es. T05), dentro x[...] il nome 'periodo' indicherebbe quella colonna
  data.table::set(x, j = "periodo_dati", value = periodo)
  data.table::set(x, j = "fonte_dati",   value = sub("^Fonte: ", "", fonte))
  x
}

# Didascalia per i grafici: riga del periodo + riga della fonte
didascalia <- function(periodo, fonte = FONTE_ISTAT, larghezza = 105) {
  # spezzo le righe lunghe, altrimenti ggplot le taglia ai bordi del grafico
  paste(c(strwrap(paste0("Periodo: ", periodo, "."), width = larghezza), strwrap(fonte, width = larghezza)), collapse = "\n")
}

# Convenzione ISTAT per la prevalenza delle limitazioni gravi: si contano le
# persone con LIMITA = 1 e si dividono per TUTTA la popolazione (chi non ha
# risposto alla domanda, 5-9% secondo l'anno, sta al denominatore ma non al
# numeratore). Così si ottiene il 5,2% del 2017 pubblicato da ISTAT.
# grave1() restituisce TRUE solo per le limitazioni gravi, FALSE per le altre
# risposte E per le mancate risposte (così non produce NA nelle somme).
grave1 <- function(limitazioni) !is.na(limitazioni) & limitazioni == 1
