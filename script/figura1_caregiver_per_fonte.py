# figura1_caregiver_per_fonte.py
# Figura 1 della tesina: quanti sono i caregiver in Italia secondo le diverse
# indagini ISTAT (EHIS 2015 e 2019, Famiglie e soggetti sociali 2024, AVQ 2019
# e 2024, Conciliazione 2018). I numeri sono trascritti dalle tavole ISTAT
# citate nella nota in calce; i valori AVQ vengono dagli script 02-03 di questo
# progetto. Serve Python 3 con matplotlib. Si lancia dalla cartella del progetto:
#   python3 script/figura1_caregiver_per_fonte.py
# Uscita: output/grafici/Figura1_caregiver_per_fonte.png
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

plt.rcParams.update({"font.family":"DejaVu Sans","font.size":11})
DARK="#1f4e79"; MID="#6f93bf"; LIGHT="#b7c9df"; INK="#222"; MUTED="#555"

# (etichetta, valore mln, livello 0=totale 1=sottoinsieme, nota a destra, stimato?)
blocks = [
 ("ISTAT, Indagine europea sulla salute (EHIS) 2015 – tavole del 19 ottobre 2017\n"
  "Persone di 15 anni e più che forniscono cure o assistenza almeno una volta a settimana a persone\n"
  "con problemi dovuti all'invecchiamento, a patologie croniche o a infermità",
  [("totale (16,4% dei 15+)", 8.555, 0, False, "8,56 mln"),
   ("   di cui prevalentemente a familiari", 7.293, 1, False, "7,29 mln"),
   ("   di cui per 20 ore o più a settimana (25,1%)", 2.146, 1, False, "2,15 mln")]),
 ("ISTAT, Indagine europea sulla salute (EHIS) 2019 – tavole del 13 gennaio 2022\n"
  "Stessa definizione del 2015",
  [("totale (15,4% dei 15+)", 7.992, 0, False, "7,99 mln"),
   ("   di cui prevalentemente a familiari", 7.014, 1, False, "7,01 mln"),
   ("   di cui per 20 ore o più a settimana (29,6%)", 2.367, 1, False, "2,37 mln")]),
 ("ISTAT, Famiglie e soggetti sociali 2024 – report «Aiuti dati, ricevuti e reti di solidarietà», 21 luglio 2026\n"
  "Persone di 18 anni e più che nelle quattro settimane precedenti l'intervista hanno prestato cura o assistenza",
  [("caregiver in complesso, di coabitanti e/o non coabitanti (24,9% dei 18+)", 12.3, 0, False, "12,3 mln"),
   ("   di cui per 20 ore o più a settimana (31,9%, «circa 4 milioni»)", 4.0, 1, False, "≈4,0 mln"),
   ("   di cui caregiver di persone non coabitanti (19,4%)", 9.6, 1, False, "9,6 mln"),
   ("   di cui «caregiver familiari», di un coabitante con problemi di salute (11,0%)", 4.435, 1, False, "4,4 mln"),
   ("      di cui per 20 ore o più a settimana (29,3% di 4,4 mln, calcolo nostro)", 1.30, 1, True, "1,3 mln")]),

 ("ISTAT, Aspetti della vita quotidiana (AVQ) 2019 e 2024 – nostra elaborazione sui microdati ad uso pubblico\n"
  "Persone di 18 anni e più che vivono con un familiare con limitazioni gravi (domanda GALI): «caregiver potenziali»,\n"
  "perché l'AVQ non chiede se e quanto si assiste",
  [("2019: conviventi con un familiare con limitazioni gravi (7,3% dei 18+)", 3.674, 0, True, "3,67 mln"),
   ("   di cui senza limitazioni gravi loro stessi", 3.204, 1, True, "3,20 mln"),
   ("2024: conviventi con un familiare con limitazioni gravi (6,3% dei 18+)", 3.124, 0, True, "3,12 mln"),
   ("   di cui senza limitazioni gravi loro stessi", 2.763, 1, True, "2,76 mln")]),
 ("ISTAT, Conciliazione tra lavoro e famiglia 2018 – report del 18 novembre 2019 (modulo ad hoc Forze di lavoro)\n"
  "Persone di 18-64 anni che si prendono cura di parenti malati, disabili o anziani",
  [("totale (7,7% dei 18-64)", 2.827, 0, False, "2,83 mln")]),
]

rows=[]; y=0; headers=[]
for title, items in blocks:
    headers.append((y, title))
    y -= 1.75
    for lab,val,lvl,est,vl in items:
        rows.append((y,lab,val,lvl,est,vl)); y -= 1.0
    y -= 0.55
ymin=y

fig, ax = plt.subplots(figsize=(14, 16.2))
for yy,lab,val,lvl,est,vl in rows:
    col = DARK if lvl==0 else MID
    ax.barh(yy, val, height=0.72, color=col, edgecolor="white", linewidth=1,
            hatch="//" if est else None)
    ax.text(-0.15, yy, lab, ha="right", va="center", fontsize=10.5, color=INK)
    ax.text(val+0.1, yy, vl,
            va="center", fontsize=11, color=INK, fontweight="bold" if lvl==0 else "normal")
for yy,t in headers:
    ax.annotate(t, xy=(0.02, yy), xycoords=("figure fraction","data"), ha="left", va="top", fontsize=10.5, color=DARK, fontweight="bold", linespacing=1.25, annotation_clip=False)
    ax.axhline(yy+0.45, color="#dddddd", lw=0.8, xmin=0, xmax=1, clip_on=False)

ax.set_xlim(0, 13.5); ax.set_ylim(ymin, 0.6)
ax.set_yticks([])
ax.xaxis.grid(True, color="#e5e5e5", lw=0.8); ax.set_axisbelow(True)
for s in ("top","right","left"): ax.spines[s].set_visible(False)
ax.spines["bottom"].set_color("#999")
ax.set_xlabel("Milioni di persone", fontsize=11, color=MUTED)
ax.set_xticks(range(0,14,2))

fig.suptitle("Quanti sono i caregiver in Italia? Dipende da chi conta e da come",
             x=0.02, y=0.985, ha="left", fontsize=16, fontweight="bold", color=INK)
fig.text(0.02, 0.955, "Stime ISTAT a confronto: ogni indagine usa una definizione, un'età minima e un periodo di riferimento diversi",
         fontsize=11.5, color=MUTED)

ax.legend(handles=[Patch(color=DARK,label="totale della rilevazione"),
                   Patch(color=MID,label="sottoinsieme («di cui»)"),
                   Patch(facecolor=MID,hatch="//",edgecolor="white",label="valore calcolato da noi (non pubblicato dall'ISTAT)")],
          loc="lower right", frameon=False, fontsize=10)

note = ("Fonti: ISTAT, tavole EHIS 2015 (19/10/2017) e EHIS 2019 (13/01/2022); «Aiuti dati, ricevuti e reti di solidarietà. Anno 2024», Statistiche Report, 21/07/2026; "
 "«Conciliazione tra lavoro e famiglia. Anno 2018», Statistiche Report, 18/11/2019; microdati AVQ 2019 e 2024 ad uso pubblico (elaborazione propria).\n"
 "Le stime non sono confrontabili fra loro: ogni indagine usa una definizione, un'età minima e un periodo di riferimento diversi; l'AVQ conta chi vive con la persona "
 "con limitazioni gravi, non chi la assiste. EHIS e Famiglie e soggetti sociali pubblicano le ore di assistenza in classi, l'ultima «20 e più», e l'AVQ non le chiede: "
 "nessuna indagine permette quindi di individuare chi assiste per le 91 ore settimanali del disegno di legge, e la stessa relazione tecnica le stima per ipotesi.")
fig.text(0.02, 0.012, note, fontsize=9.2, color=MUTED, va="bottom", wrap=True, linespacing=1.35)

plt.subplots_adjust(left=0.52, right=0.97, top=0.93, bottom=0.095)
out="output/grafici/Figura1_caregiver_per_fonte.png"
fig.savefig(out, dpi=200); pass
print("ok")
