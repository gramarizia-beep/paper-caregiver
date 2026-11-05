# =============================================================================
# 08_fondo_fisso_platea.R
# Simulazione: con un fondo fisso, quanto spetta a ciascun beneficiario del
# contributo per il caregiver prevalente al variare della platea, e come la
# soglia ISEE portata da 15.000 a 25.000 euro riduce l'importo tra le due soglie.
#
# Serve solo R di base: nessun pacchetto, nessun microdato. I numeri di
# partenza sono quelli del disegno di legge approvato dalla Camera il
# 14 ottobre 2026 (A.C. 2789-A) e della relazione tecnica al testo originario,
# più i due estremi dell'imbuto costruito sui microdati AVQ 2024 (script 05).
# =============================================================================

# --- Parametri del disegno di legge --------------------------------------------
# Limite di spesa del contributo (art. 14, comma 5, del testo approvato dalla Camera):
# 186,08 milioni nel 2027 (si pagano tre trimestri), 251,41 milioni nel 2028,
# 251,21 nel 2029-2034, 251,01 dal 2035. Lo stanziamento complessivo della legge
# (art. 16: 257 milioni nel 2027, 260 dal 2028) comprende anche piattaforma e
# personale INPS e il congedo parentale allungato.
FONDO_CONTRIBUTO  <- 251.41e6     # euro l'anno a regime (2028)
FONDO_2027        <- 186.08e6     # euro nel 2027, tre trimestri
FONDO_GOVERNO     <- 248.05e6     # testo del Governo, 2027: e' la base della relazione tecnica
CONTRIBUTO_MAX    <- 1200 * 4     # 1.200 euro a trimestre = 4.800 euro l'anno
ISEE_PIENO        <- 15000        # fino a questo ISEE l'importo e' pieno
ISEE_MAX          <- 25000        # oltre questo ISEE non spetta nulla

# --- Numeri di riferimento sulla platea ----------------------------------------
# Relazione tecnica al disegno di legge (A.C. 2789, 6/2/2026, p. 24 dello stampato), su EHIS 2019:
RT_CONVIVENTI_20H <- 1096000      # caregiver familiari conviventi non occupati, almeno 20 ore a settimana
RT_NON_OCCUPATI   <- 522000       # ...di cui assistono una persona con limitazioni gravi
RT_PLATEA_MAX     <- 87000        # ...di cui con almeno 91 ore: "non piu' di un sesto" (ipotesi del Governo)
QUOTA_ISEE_15000  <- 0.7164       # quota di nuclei con un disabile e ISEE <= 15.000 (DSU INPS 2023-24)
RT_BENEFICIARI    <- 62333        # = 87.000 x 71,64%
# Imbuto sui microdati ISTAT AVQ 2024 (script 05, tabella T10; persone assistite):
AVQ_CONVIVENTI_NON_OCC   <- 1345000  # con limitazioni gravi, convivono con un adulto non occupato
AVQ_RISORSE_SCARSE       <- 557000   # ...in famiglie con risorse economiche scarse o insufficienti

# --- Tabella A: importo annuo per beneficiario al variare della platea ---------
platea <- c(FONDO_CONTRIBUTO / CONTRIBUTO_MAX, RT_BENEFICIARI, RT_PLATEA_MAX, 100000,
            150000, 200000, 300000, AVQ_RISORSE_SCARSE)
importo_annuo   <- pmin(CONTRIBUTO_MAX, FONDO_CONTRIBUTO / platea)
tabella_A <- data.frame(
  platea_beneficiari     = round(platea),
  importo_annuo_euro     = round(importo_annuo),
  importo_mensile_euro   = round(importo_annuo / 12),
  quota_del_massimo_pct  = round(100 * importo_annuo / CONTRIBUTO_MAX, 1),
  spesa_se_tutti_pieni_mln = round(platea * CONTRIBUTO_MAX / 1e6)
)
tabella_A$nota <- c("contributi pieni pagabili con 251,41 milioni",
                    "relazione tecnica, testo del Governo (ISEE 15.000)",
                    "tetto della relazione tecnica: tutti i caregiver con 91 ore, qualunque ISEE",
                    "", "", "", "",
                    "estremo basso dell'imbuto AVQ 2024 (persone assistite)")

# --- Tabella B: la formula dell'ISEE tra 15.000 e 25.000 ----------------------
# importo = importo pieno x (25.000 - ISEE) / 10.000, per ISEE tra 15.000 e 25.000
isee <- c(5000, 10000, 15000, 17500, 20000, 22500, 25000)
coeff <- ifelse(isee <= ISEE_PIENO, 1,
         ifelse(isee >= ISEE_MAX, 0, (ISEE_MAX - isee) / (ISEE_MAX - ISEE_PIENO)))
tabella_B <- data.frame(
  ISEE_euro               = isee,
  quota_dell_importo_pct  = round(100 * coeff),
  importo_annuo_massimo   = round(CONTRIBUTO_MAX * coeff),
  importo_mensile_massimo = round(CONTRIBUTO_MAX * coeff / 12)
)

# --- Tabella C: i numeri di riferimento, messi in fila ------------------------
tabella_C <- data.frame(
  grandezza = c("Persone con limitazioni gravi che convivono con un adulto non occupato (AVQ 2024)",
                "...di cui in famiglie con risorse scarse o insufficienti (AVQ 2024)",
                "Caregiver familiari conviventi non occupati, almeno 20 ore (EHIS 2019, relazione tecnica)",
                "...di cui assistono una persona con limitazioni gravi (relazione tecnica)",
                "...di cui con almeno 91 ore: un sesto, ipotesi della relazione tecnica",
                "...di cui con ISEE fino a 15.000 euro = beneficiari stimati (relazione tecnica)",
                "Contributi pieni (4.800 euro) pagabili con 251,41 milioni l'anno"),
  valore = c(AVQ_CONVIVENTI_NON_OCC, AVQ_RISORSE_SCARSE, RT_CONVIVENTI_20H, RT_NON_OCCUPATI,
             RT_PLATEA_MAX, RT_BENEFICIARI, round(FONDO_CONTRIBUTO / CONTRIBUTO_MAX)),
  quota_sulla_riga_precedente_pct = c(NA, round(100 * AVQ_RISORSE_SCARSE / AVQ_CONVIVENTI_NON_OCC, 1), NA,
             round(100 * RT_NON_OCCUPATI / RT_CONVIVENTI_20H, 1), round(100 * RT_PLATEA_MAX / RT_NON_OCCUPATI, 1),
             round(100 * RT_BENEFICIARI / RT_PLATEA_MAX, 1), NA)
)

# --- Salvataggio e stampa -------------------------------------------------------
dir.create(file.path("output", "tabelle"), showWarnings = FALSE, recursive = TRUE)
write.csv2(tabella_A, file.path("output", "tabelle", "T16_fondo_fisso_importo_per_platea.csv"), row.names = FALSE)
write.csv2(tabella_B, file.path("output", "tabelle", "T17_formula_isee_15000_25000.csv"), row.names = FALSE)
write.csv2(tabella_C, file.path("output", "tabelle", "T18_numeri_di_riferimento_platea.csv"), row.names = FALSE)

cat("\nTabella A - importo per beneficiario con fondo fisso di 251,41 milioni\n")
print(tabella_A, row.names = FALSE)
cat("\nTabella B - formula ISEE 15.000-25.000\n")
print(tabella_B, row.names = FALSE)
cat("\nTabella C - numeri di riferimento\n")
print(tabella_C, row.names = FALSE)
cat("\nNel 2027 (186,08 milioni, tre trimestri) i contributi pieni pagabili sono", round(FONDO_2027 / (CONTRIBUTO_MAX * 3/4)), "\n")
# Controllo della relazione tecnica: 248,05 milioni divisi per 62.333 beneficiari
cat("Verifica: 248,05 milioni / 62.333 =", round(FONDO_GOVERNO / RT_BENEFICIARI), "euro l'anno, cioe'",
    round(FONDO_GOVERNO / RT_BENEFICIARI / 4), "euro a trimestre (la relazione tecnica diceva circa 1.000)\n")
# Con l'ISEE a 25.000 la platea non puo' superare gli 87.000 della relazione tecnica (al piu' 24.667 in piu', con importo ridotto)
cat("Con ISEE 25.000: al massimo", RT_PLATEA_MAX, "beneficiari, cioe' fino a", RT_PLATEA_MAX - RT_BENEFICIARI,
    "in piu'; importo medio se fossero tutti pieni:", round(FONDO_CONTRIBUTO / RT_PLATEA_MAX), "euro l'anno\n")
