"""
13_irrbb_stress_test.py — поглиблений стрес-тест процентного ризику банківської книги (IRRBB) по кожному банку.

Методологія:
  * BCBS (2016) «Interest rate risk in the banking book», стандартизована рамка: 19 часових кошиків, шість сценаріїв
    (паралельний ±, steepener, flattener, короткі ±), агрегація ΔEVE між валютами (прибутки з вагою 50 %),
    поведінкові моделі коштів на вимогу (ядро/неядро, ліміти частки ядра і середнього строку), дострокові
    погашення кредитів (CPR) і дострокове вилучення строкових депозитів (TDRR) з множниками сценаріїв,
    мінімальна межа ставок після шоку, тест на викид 15 % капіталу першого рівня.
  * BCBS (2024) рекалібрування шоків: 99,9-й процентиль 6-місячних змін ставок, нижня межа 100 б.п., верхні
    400/500/300 б.п., округлення до 25 б.п. — тут застосовано до кривої нульового купона НБУ.
  * EBA (GL/2022/14, RTS SOT): тест на викид ЧПД — зниження ЧПД > 5 % капіталу першого рівня за паралельних ±200 б.п.
  * НБУ: Постанова № 64 від 11.06.2018 (п. 240, ред. Постанови № 172 від 29.12.2020) — шоки 200 б.п. для гривні та
    валют 1 групи (усі строки і строки до 6 міс.), 500/400 б.п. для валют 2–3 груп; ГЕП / ЧПД і модифікована дюрація / EVE (п. 246).
  * Власне дослідження (п. 2.1–2.3): виявлена передача облікової ставки (β_h, θ) для ЧПД і поведінкового ядра
    коштів на вимогу; історичне моделювання на кривій НБУ; зворотний стрес-тест з відтоком депозитної франшизи
    (Drechsler, Savov & Schnabl, 2023).

Запуск: python3 13_irrbb_stress_test.py [OUTPUT_DIR]  → OUTPUT_DIR/bank_excel/IRRBB_Stress_Test.xlsx
"""
import csv, os, re, sys, math, statistics, datetime as dt
from collections import defaultdict
from openpyxl import Workbook
from openpyxl.styles import Font, Alignment, Border, Side, PatternFill
from openpyxl.utils import get_column_letter as CL
from openpyxl.worksheet.datavalidation import DataValidation
from openpyxl.formatting.rule import CellIsRule, ColorScaleRule, FormulaRule
from openpyxl.chart import BarChart, LineChart, Reference, Series
from openpyxl.chart.series import SeriesLabel
from openpyxl.chart.text import RichText
from openpyxl.drawing.text import Paragraph, ParagraphProperties, CharacterProperties, Font as DFont

OUT_ROOT = sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "output_nbu")
T23 = os.path.join(OUT_ROOT, "section_2_3", "tables")
T24 = os.path.join(OUT_ROOT, "section_2_4", "tables")
DEST = os.path.join(OUT_ROOT, "bank_excel"); os.makedirs(DEST, exist_ok=True)
FILE = os.path.join(DEST, "IRRBB_Stress_Test.xlsx")


def rd(p):
    with open(p, encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def num(x, d=0.0):
    try:
        v = float(x); return v if v == v else d
    except (TypeError, ValueError):
        return d


def clean(s):
    from bank_names import clean_bank_name
    return clean_bank_name(s)


# ============================================================ data
items = rd(os.path.join(T23, "S23_items_parameters.csv"))
irates = {r["item"]: (num(r["uah"]), num(r["fx"])) for r in rd(os.path.join(T23, "S23_item_rates_latest.csv"))}
betas = rd(os.path.join(T23, "S23_beta_paths.csv"))
totals = rd(os.path.join(T23, "S23_bank_totals_latest.csv"))
bitems = rd(os.path.join(T23, "S23_bank_items_latest.csv"))
theta = rd(os.path.join(T23, "S23_bank_theta.csv"))
curves = rd(os.path.join(T23, "S23_curves_history.csv"))
OUD = os.path.join(OUT_ROOT, "ou_dynamics", "tables")
HAS_OU = all(os.path.exists(os.path.join(OUD, f)) for f in ("OU_stress_shocks.csv", "OU_bank_core.csv", "OU_O5_stable_core_system.csv", "OU_paths_item_rate_change_12m.csv"))
OUSH, OUB, OUS, OUX, OUN, OUFAN = [], {}, {}, [], {}, []
if HAS_OU:
    OUSH = rd(os.path.join(OUD, "OU_stress_shocks.csv"))
    OUB = {r["nkb"]: r for r in rd(os.path.join(OUD, "OU_bank_core.csv"))}
    OUS = {r["product"]: r for r in rd(os.path.join(OUD, "OU_O5_stable_core_system.csv"))}
    OUX = rd(os.path.join(OUD, "OU_paths_item_rate_change_12m.csv"))
    if os.path.exists(os.path.join(OUD, "OU_bank_nii_at_risk.csv")):
        OUN = {r["nkb"]: r for r in rd(os.path.join(OUD, "OU_bank_nii_at_risk.csv"))}
    if os.path.exists(os.path.join(OUD, "OU_fan_policy.csv")):
        OUFAN = rd(os.path.join(OUD, "OU_fan_policy.csv"))
OWN = {"State": "Державний", "Foreign": "Іноземний", "Private": "Приватний"}
IT = [r["item"] for r in items]; NI = len(IT)
ILAB = {r["item"]: r["label"] for r in items}
ISIDE = {r["item"]: r["side"] for r in items}
IKEY = {r["item"]: r["key"] for r in items}
ILAM = {r["item"]: num(r["lambda"]) for r in items}
NMD = ["hh_dem", "corp_dem"]
TYPE = {i: ("nmd" if i in NMD else "td" if i in ("hh_term", "corp_term") else "loan" if i in ("loan_corp", "loan_hh") else "oth") for i in IT}
banks = sorted(totals, key=lambda r: -num(r["TA"]))
NB = len(banks)
LATEST = totals[0]["month"][:10]
latest_txt = dt.date.fromisoformat(LATEST).strftime("%m.%Y")
stock = defaultdict(float)
for r in bitems:
    stock[(r["nkb"], r["item"], "U")] += num(r["uah"]); stock[(r["nkb"], r["item"], "F")] += num(r["fx"])
TH = {(r["nkb"], r["beta_key"]): num(r["theta"], None) for r in theta}
BETA = defaultdict(dict)
for r in betas:
    for v in ("b", "b_up", "b_dn"):
        BETA[(r["beta_key"], v)][int(float(r["h"]))] = num(r[v])
avgb = lambda k, v: (sum(BETA[(k, v)].get(h, 0.0) for h in range(1, 13)) / 12.0) if k else 0.0
TEN = ["3m", "6m", "1y", "2y", "3y", "5y", "7y", "10y"]; TENY = [0.25, 0.5, 1, 2, 3, 5, 7, 10]
CUR = defaultdict(dict)
for r in curves:
    m = r["month"][:10]
    CUR[m][r["ccy"]] = [num(r[t], None) if r[t] not in ("", "NA") else None for t in TEN]
MONTHS = sorted(CUR)
DBASE = max(m for m in MONTHS if CUR[m].get("uah") and sum(v is not None for v in CUR[m]["uah"]) >= 6)
D_b = [num(b["D_hhdem_b"]) for b in banks]; D_c = [num(b["D_corpdem_b"]) for b in banks]
D_hh0 = statistics.median(D_b) if D_b else 2.0
_th = lambda k: [v for (n_, kk), v in TH.items() if kk == k and v is not None]
TH_MED = {"hh_dem": statistics.median(_th("hh_demand")) if _th("hh_demand") else 0.1, "corp_dem": statistics.median(_th("corp_demand")) if _th("corp_demand") else 0.3}; D_corp0 = statistics.median(D_c) if D_c else 2.0

# ============================================================ styles
A10 = Font(name="Arial", size=10); A10B = Font(name="Arial", size=10, bold=True); A10I = Font(name="Arial", size=10, italic=True)
A9 = Font(name="Arial", size=9); A9I = Font(name="Arial", size=9, italic=True); A12B = Font(name="Arial", size=12, bold=True)
A10L = Font(name="Arial", size=10, underline="single", color="0000FF")
INP = Font(name="Arial", size=10, color="0000FF"); LINKF = Font(name="Arial", size=10, color="008000")
FILL_IN = PatternFill("solid", fgColor="FFF2CC"); FILL_KEY = PatternFill("solid", fgColor="F2F2F2"); FILL_HEAD = PatternFill("solid", fgColor="DDEBF7")
RED = PatternFill("solid", fgColor="F8CBAD"); GREEN = PatternFill("solid", fgColor="C6EFCE"); AMBER = PatternFill("solid", fgColor="FFEB9C")
TH_ = Side(style="thin", color="000000"); BOX = Border(left=TH_, right=TH_, top=TH_, bottom=TH_); TOPB = Border(top=TH_)
CEN = Alignment(horizontal="center", vertical="center", wrap_text=True); LEFT = Alignment(horizontal="left", vertical="center", wrap_text=True)
LNW = Alignment(horizontal="left", vertical="center", wrap_text=False); RIGHT = Alignment(horizontal="right", vertical="center")
N0 = '#,##0;\\–#,##0;"–"'; N1 = '#,##0.0;\\–#,##0.0;"–"'; N2 = '#,##0.00;\\–#,##0.00;"–"'; N4 = '0.0000;\\–0.0000;"–"'
P1 = '0.0%;\\–0.0%;"–"'; P2 = '0.00%;\\–0.00%;"–"'; BP = '#,##0" б.п.";\\–#,##0" б.п.";"0 б.п."'
MON = "mm.yyyy"; DATEF = "dd.mm.yyyy"

wb = Workbook(); wb.remove(wb.active)
NAMES = {}             # symbolic name -> absolute reference; substituted into formulas at the end (no defined names: works in Excel, Numbers, LibreOffice)


def sheet(n, tab=None):
    ws = wb.create_sheet(n); ws.sheet_view.showGridLines = False
    if tab: ws.sheet_properties.tabColor = tab
    return ws


def put(ws, r, c, v, font=A10, fmt=None, al=None, border=None, fill=None):
    cell = ws.cell(r, c, v); cell.font = font
    if fmt: cell.number_format = fmt
    if al: cell.alignment = al
    if border: cell.border = border
    if fill: cell.fill = fill
    return cell


def titles(ws, sub, note=None):
    put(ws, 1, 1, "13. Стрес-тестування процентного ризику банківської книги (IRRBB)", A10B, al=LNW)
    put(ws, 2, 1, sub, A10B, al=LNW)
    if note: put(ws, 3, 1, note, A10, al=LNW)
    return 5


def header(ws, r, c0, labels, height=None, numbers=True, fill=None):
    for j, lab in enumerate(labels):
        put(ws, r, c0 + j, lab, al=CEN, border=BOX, fill=fill)
    if height: ws.row_dimensions[r].height = height
    r += 1
    if numbers:
        for j in range(len(labels)):
            put(ws, r, c0 + j, j + 1, A9, al=CEN, border=BOX)
        r += 1
    ws.row_dimensions[r].height = 6
    return r + 1


def foot(ws, r, lines=()):
    r += 1
    put(ws, r, 1, "«–» — явища не було; «…» — даних немає або показник не визначений.", A9I); r += 1
    for t in lines:
        put(ws, r, 1, t, A9I, al=LNW); r += 1
    put(ws, r, 1, "Джерело: розрахунки автора за даними НБУ; методологія — BCBS (2016, 2024), EBA (2022), Постанова НБУ № 64; скрипт 13_irrbb_stress_test.py.", A9I)
    return r + 1


def nm(name, ws, r, c):
    NAMES[name] = f"'{ws.title}'!${CL(c)}${r}"


def axis_font(ch):
    rt = RichText(p=[Paragraph(pPr=ParagraphProperties(defRPr=CharacterProperties(latin=DFont(typeface="Arial"), sz=900)), endParaRPr=CharacterProperties())])
    for ax in (ch.x_axis, ch.y_axis):
        ax.txPr = rt; ax.delete = False
    if ch.legend is not None:
        ch.legend.txPr = rt; ch.legend.position = "b"


# ============================================================ buckets & scenarios
BK = [("O/N", 0, 1 / 365, 0.0028), ("O/N–1 міс.", 1 / 365, 1 / 12, 0.0417), ("1–3 міс.", 1 / 12, 0.25, 0.1667), ("3–6 міс.", 0.25, 0.5, 0.375),
      ("6–9 міс.", 0.5, 0.75, 0.625), ("9–12 міс.", 0.75, 1, 0.875), ("1–1,5 р.", 1, 1.5, 1.25), ("1,5–2 р.", 1.5, 2, 1.75), ("2–3 р.", 2, 3, 2.5),
      ("3–4 р.", 3, 4, 3.5), ("4–5 р.", 4, 5, 4.5), ("5–6 р.", 5, 6, 5.5), ("6–7 р.", 6, 7, 6.5), ("7–8 р.", 7, 8, 7.5), ("8–9 р.", 8, 9, 8.5),
      ("9–10 р.", 9, 10, 9.5), ("10–15 р.", 10, 15, 12.5), ("15–20 р.", 15, 20, 17.5), ("> 20 р.", 20, None, 25)]
NK = len(BK); NK1Y = 6
SC = [("Паралельний вгору (BCBS)", "s1"), ("Паралельний вниз (BCBS)", "s2"), ("Steepener: короткі ↓, довгі ↑ (BCBS)", "s3"), ("Flattener: короткі ↑, довгі ↓ (BCBS)", "s4"),
      ("Короткі ставки вгору (BCBS)", "s5"), ("Короткі ставки вниз (BCBS)", "s6"), ("НБУ: усі строки +200 б.п.", "s1"), ("НБУ: усі строки −200 б.п.", "s2"),
      ("НБУ: строки до 6 міс. +200 б.п.", "s5"), ("НБУ: строки до 6 міс. −200 б.п.", "s6"), ("Власний сценарій", "s0"), ("Історичний сценарій (крива НБУ)", "s0")]
def _ou_group(r):
    if r.get("type", "vasicek") != "curve":
        return "s1" if num(r["d_i_bp"]) > 0 else "s2"
    t = [num(r.get(f"t_{x}")) for x in ("3m", "6m", "1y", "2y", "3y", "5y", "7y", "10y")]
    if "нахил" in r["name"]:
        return "s3" if t[-1] > t[0] else "s4"
    return "s1" if sum(t) > 0 else "s2"
SC += [(r["name"], _ou_group(r)) for r in OUSH]
NOU = len(OUSH)
NS = len(SC)
SCN = [s[0] for s in SC]
GROUPS = ["s0", "s1", "s2", "s3", "s4", "s5", "s6"]          # behavioural-multiplier groups → distinct cash-flow distributions
GROUP_LAB = {"s0": "базовий", "s1": "паралельний вгору", "s2": "паралельний вниз", "s3": "steepener", "s4": "flattener", "s5": "короткі вгору", "s6": "короткі вниз"}
CCY = [("uah", "гривня"), ("fx", "валюта (USD)")]

# ============================================================ sheets
S0 = sheet("13"); S1 = sheet("13.1", "1F4E79"); S2 = sheet("13.2"); S3 = sheet("13.3"); S4 = sheet("13.4"); S5 = sheet("13.5")
S6 = sheet("13.6"); S7 = sheet("13.7"); S8 = sheet("13.8"); S9 = sheet("13.9", "1F4E79"); S10 = sheet("13.10"); S11 = sheet("13.11", "1F4E79"); D1 = sheet("Д1", "808080"); D2 = sheet("Д2", "808080"); D3 = sheet("Д3", "808080")

# ------------------------------------------------------------ Д1 bank data
put(D1, 1, 1, f"Д1. Баланс банків на {latest_txt}: статті, що переоцінюються (млн грн), капітал, ЧПД, поведінкові параметри (не редагувати)", A10B)
D1C = ["nkb", "bank", "owner", "TA", "EQ", "NII12", "th_hh", "th_corp", "ou_hh", "ou_corp", "ou_nii_q1"] + [f"U_{i}" for i in IT] + [f"F_{i}" for i in IT]
D1L = ["Код", "Банк", "Власність", "Активи", "Капітал (балансовий)", "ЧПД за 12 міс.", "θ коштів ФО на вимогу (п. 2.2)", "θ коштів СГ на вимогу (п. 2.2)",
       "OU: ядро коштів ФО на вимогу", "OU: ядро коштів СГ на вимогу", "OU: ЧПД 1-й процентиль, млн грн"] + \
      [f"{ILAB[i]}: гривня" for i in IT] + [f"{ILAB[i]}: валюта" for i in IT]
for j, (k, l) in enumerate(zip(D1C, D1L)):
    put(D1, 3, j + 1, k, A9, al=CEN, border=BOX); put(D1, 4, j + 1, l, al=CEN, border=BOX)
D1.row_dimensions[4].height = 70
DC = {k: j + 1 for j, k in enumerate(D1C)}
R1 = 5
for n, b in enumerate(banks):
    r = R1 + n
    vals = [b["nkb"], clean(b["bank"]), OWN.get(b["owner"], b["owner"] or "…"), num(b["TA"]), num(b["EQ"]), num(b["NII12"]),
            TH.get((b["nkb"], "hh_demand")), TH.get((b["nkb"], "corp_demand")),
            num(OUB.get(b["nkb"], {}).get("core_hh_dem"), None), num(OUB.get(b["nkb"], {}).get("core_corp_dem"), None), num(OUN.get(b["nkb"], {}).get("dnii_q1"), None)] + \
           [stock[(b["nkb"], i, "U")] for i in IT] + [stock[(b["nkb"], i, "F")] for i in IT]
    for j, v in enumerate(vals):
        put(D1, r, j + 1, v, fmt=(N2 if D1C[j].startswith(("th", "ou_h", "ou_c")) else N1 if j >= 3 else None))
RL = R1 + NB - 1
D1.freeze_panes = "C5"; D1.column_dimensions["B"].width = 34
for j in range(4, len(D1C) + 1): D1.column_dimensions[CL(j)].width = 12
d1col = lambda k: f"'Д1'!${CL(DC[k])}${R1}:${CL(DC[k])}${RL}"
U0, U1 = CL(DC[f"U_{IT[0]}"]), CL(DC[f"U_{IT[-1]}"]); F0, F1 = CL(DC[f"F_{IT[0]}"]), CL(DC[f"F_{IT[-1]}"])

# ------------------------------------------------------------ Д2 curves (wide) + 6/12-month change helpers for calibration
put(D2, 1, 1, "Д2. Криві нульового купона НБУ (середньомісячні, частки) і зміни за 6 міс. для калібрування шоків (не редагувати)", A10B)
lab = ["Місяць"] + [f"грн {t}" for t in TEN] + [f"вал. {t}" for t in TEN] + ["грн: короткі (3м–1р)", "грн: довгі (7–10р)", "грн: паралельний (усі)",
                                                                             "|Δ6м| короткі", "|Δ6м| довгі", "|Δ6м| паралельний",
                                                                             "вал.: короткі", "вал.: довгі", "вал.: паралельний", "|Δ6м| вал. короткі", "|Δ6м| вал. довгі", "|Δ6м| вал. паралельний"]
for j, l in enumerate(lab):
    put(D2, 3, j + 1, l, al=CEN, border=BOX)
D2.row_dimensions[3].height = 40
H0 = 4
for n, m in enumerate(MONTHS):
    r = H0 + n
    put(D2, r, 1, dt.date.fromisoformat(m), fmt=MON)
    for cc, off in (("uah", 2), ("fx", 10)):
        vals = CUR[m].get(cc) or [None] * 8
        for j, v in enumerate(vals):
            if v is not None: put(D2, r, off + j, v, fmt=P2)
    for cc, off, aoff in (("uah", 2, 18), ("fx", 10, 24)):
        c = lambda j: f"{CL(off + j)}{r}"
        put(D2, r, aoff, f'=IFERROR(AVERAGE({c(0)}:{c(2)}),"")', fmt=P2)
        put(D2, r, aoff + 1, f'=IFERROR(AVERAGE({c(6)}:{c(7)}),"")', fmt=P2)
        put(D2, r, aoff + 2, f'=IFERROR(AVERAGE({c(0)}:{c(7)}),"")', fmt=P2)
        for q in range(3):
            if n >= 6:
                put(D2, r, aoff + 3 + q, f'=IFERROR(ABS({CL(aoff + q)}{r}-{CL(aoff + q)}{r - 6}),"")', fmt=P2)
HL = H0 + len(MONTHS) - 1
D2.freeze_panes = "B4"; D2.column_dimensions["A"].width = 10
for j in range(2, 30): D2.column_dimensions[CL(j)].width = 9.5
d2 = lambda col: f"'Д2'!${col}${H0}:${col}${HL}"

# ------------------------------------------------------------ 13.1 switches (first, others reference them)
r = titles(S1, "13.1. Панель: перемикачі і результати стрес-тесту обраного банку",
           f"(баланс на {latest_txt}; млн грн; блакитні клітинки на жовтому тлі — перемикачі; результати перераховуються формулами)")
r = header(S1, r, 1, ["Перемикач", "Значення", "Пояснення"], numbers=False, fill=FILL_HEAD)
SW0 = r
shock_uah_list = ["Калібрування BCBS на кривій НБУ", "Максимальні BCBS (400/500/300)", "Власні"]
shock_fx_list = ["USD (BCBS 2024)", "EUR (BCBS 2024)", "Власні"]
nmd_list = ["Виявлений (п. 2.1–2.2)", "BCBS стандартизований", "Договірний (усе O/N)", "OU стохастична (п. 2.3)"]
nii_list = ["Договірний ГЕП (НБУ п. 246)", "Виявлена передача β (п. 2.2)"]
yes = ["Так", "Ні"]
th_list = ["Системний (медіана банків)", "Банку"]
SWL = [
    ("bank", "Банк", clean(banks[0]["bank"]), None, "Оберіть банк (впорядковано за активами)."),
    ("t1_in", "Капітал першого рівня, млн грн (необов'язково)", None, N1, "Порожньо — береться балансовий капітал банку (наближення Tier 1)."),
    ("shk_u", "Розмір шоків для гривні", shock_uah_list[0], None, "Калібрування: 99,9-й процентиль 6-міс. змін кривої НБУ, межі 100…400/500/300 б.п. (BCBS 2024)."),
    ("shk_f", "Розмір шоків для валюти", shock_fx_list[0], None, "BCBS 2024: USD 200/300/225, EUR 225/350/200 б.п. (паралельний/короткі/довгі)."),
    ("nmd", "Модель коштів на вимогу", nmd_list[0], None, "Виявлений: ядро = 1 − θ (п. 2.2); OU: ядро = стабільна частка (1 % квантиль 12-міс. мінімуму) × (1 − β); BCBS: ліміти 90 %/5 р., 50 %/4 р."),
    ("nmdcap", "Ліміти BCBS до виявленої моделі", "Так", None, "Так — частка ядра і строк не вищі за ліміти BCBS."),
    ("thsrc", "θ для ядра коштів на вимогу", "Системний (медіана банків)", None, "Оцінки θ окремих банків шумні (п. 2.2); «Банку» — власна θ банку зі зсувом до середньої групи."),
    ("floor", "Мінімальна межа ставок після шоку", "Так", None, "BCBS/EBA: −100 б.п. на коротких строках, +5 б.п. на рік до 0 % на 20 р."),
    ("nii", "Метод ЧПД", nii_list[0], None, "Договірний — ГЕП за строками; виявлений — передача облікової у ставки статей з п. 2.2 (лише гривня)."),
    ("asym", "β для ЧПД: асиметричні (підвищення ≠ зниження)", "Ні", None, "Так — для шоків вгору/вниз окремі β з п. 2.2."),
    ("h_from", "Історичний сценарій: з місяця", dt.date.fromisoformat("2022-02-01"), MON, "Зміна кривої НБУ між двома датами застосовується до поточного балансу."),
    ("h_to", "Історичний сценарій: по місяць", dt.date.fromisoformat("2022-09-01"), MON, "02.2022 → 09.2022: облікова 10 % → 25 % (криву за 04–08.2022 НБУ не публікував)."),
    ("hwin", "Вікно історичного моделювання, міс.", 6, N0, "6 — як у калібруванні BCBS; 12 — річні зміни."),
    ("detail", "Сценарій для деталізації (13.5, графік кривої)", SCN[0], None, "Вплив на EVE за статтями — на аркуші 13.5."),
    ("sot", "Поріг тесту на викид EVE, частка Tier 1", 0.15, P1, "BCBS (2016): 15 %."),
    ("sot_nii", "Поріг тесту на викид ЧПД, частка Tier 1", 0.05, P1, "EBA (2022): 5 % за паралельних ±200 б.п."),
]
for k, (key, labx, val, fmt, expl) in enumerate(SWL):
    rr = SW0 + k
    put(S1, rr, 1, labx, al=LEFT, border=BOX)
    put(S1, rr, 2, val, INP, fmt, RIGHT if fmt else LEFT, BOX, FILL_IN)
    put(S1, rr, 3, expl, A9, al=LNW)
    nm(key, S1, rr, 2)
rr = SW0 + len(SWL)
put(S1, rr, 1, "Рядок банку в даних", al=LEFT, border=BOX); put(S1, rr, 2, f"=MATCH(bank,{d1col('bank')},0)", fmt=N0, al=RIGHT, border=BOX, fill=FILL_KEY); nm("row", S1, rr, 2)
rr += 1
put(S1, rr, 1, "Капітал першого рівня, що використовується, млн грн", al=LEFT, border=BOX)
put(S1, rr, 2, f'=IF(ISNUMBER(t1_in),t1_in,INDEX({d1col("EQ")},row))', A10B, N1, RIGHT, BOX, FILL_KEY); nm("T1", S1, rr, 2)
rr += 1
put(S1, rr, 1, "ЧПД за 12 міс., млн грн", al=LEFT, border=BOX); put(S1, rr, 2, f'=INDEX({d1col("NII12")},row)', fmt=N1, al=RIGHT, border=BOX, fill=FILL_KEY); nm("NII12", S1, rr, 2)
rr += 1
put(S1, rr, 1, "Номер сценарію для деталізації", al=LEFT, border=BOX); put(S1, rr, 2, None, fmt=N0, al=RIGHT, border=BOX, fill=FILL_KEY); nm("det_n", S1, rr, 2)
DET_ROW = rr
SW_END = rr + 1
DVs = []


def dv_list(ws, cell, src):
    dv = DataValidation(type="list", formula1=src, allow_blank=True); ws.add_data_validation(dv); dv.add(cell); DVs.append(dv)


# ------------------------------------------------------------ 13.2 scenarios
r = titles(S2, "13.2. Сценарії: розміри шоків, базові криві, зміни ставок і коефіцієнти дисконтування за кошиками",
           "(ставки — частки річних, ефективна дохідність; шоки — б.п.; DF = (1 + r)^−t)")
put(S2, r, 1, "13.2.1. Розміри шоків, б.п.", A10B); r += 1
r = header(S2, r, 1, ["Варіант", "Паралельний", "Короткі", "Довгі", "Примітка"], numbers=False, fill=FILL_HEAD)
SH0 = r
# calibration from NBU curve (formulas on Д2)
cap = {"par": 0.04, "short": 0.05, "long": 0.03}
put(S2, r, 1, "Гривня: калібрування BCBS на кривій НБУ", al=LNW)
for j, (col, key) in enumerate([("W", "par"), ("U", "short"), ("V", "long")]):
    put(S2, r, 2 + j, f"=MIN({cap[key]},MAX(0.01,MROUND(PERCENTILE({d2(col)},0.999),0.0025)))*10000", fmt=N0, al=RIGHT)
put(S2, r, 5, "99,9-й процентиль |Δ6м| середніх ставок (короткі 3м–1р, довгі 7–10р, паралельний — усі строки); межі 100 … 400/500/300 б.п.; округлення 25 б.п.", A9, al=LNW)
r += 1
put(S2, r, 1, "Гривня: максимальні BCBS", al=LNW)
for j, v in enumerate((400, 500, 300)): put(S2, r, 2 + j, v, INP, N0, RIGHT, fill=FILL_IN)
r += 1
put(S2, r, 1, "Гривня: власні", al=LNW)
for j, v in enumerate((400, 500, 300)): put(S2, r, 2 + j, v, INP, N0, RIGHT, fill=FILL_IN)
r += 1
put(S2, r, 1, "Валюта: USD (BCBS 2024)", al=LNW)
for j, v in enumerate((200, 300, 225)): put(S2, r, 2 + j, v, INP, N0, RIGHT, fill=FILL_IN)
r += 1
put(S2, r, 1, "Валюта: EUR (BCBS 2024)", al=LNW)
for j, v in enumerate((225, 350, 200)): put(S2, r, 2 + j, v, INP, N0, RIGHT, fill=FILL_IN)
r += 1
put(S2, r, 1, "Валюта: власні", al=LNW)
for j, v in enumerate((200, 300, 150)): put(S2, r, 2 + j, v, INP, N0, RIGHT, fill=FILL_IN)
r += 1
put(S2, r, 1, "НБУ: усі строки / до 6 міс. (гривня, валюти 1 групи)", al=LNW)
put(S2, r, 2, 200, INP, N0, RIGHT, fill=FILL_IN); put(S2, r, 3, 200, INP, N0, RIGHT, fill=FILL_IN)
put(S2, r, 5, "Постанова НБУ № 64, п. 240 (ред. № 172 від 29.12.2020); для валют 2–3 груп — 400 / 500 б.п.", A9, al=LNW)
nm("nbu_all", S2, r, 2); nm("nbu_6m", S2, r, 3)
r += 1
OU_CURVE_ROW = {}
for j, rr_ in enumerate(OUSH):
    if rr_.get("type", "vasicek") == "curve":
        put(S2, r, 1, f"{rr_['name']}: шок за строками 3м…10р, б.п. (стовпці F–M)", al=LNW)
        for q_, x in enumerate(("3m", "6m", "1y", "2y", "3y", "5y", "7y", "10y")):
            put(S2, r, 6 + q_, round(num(rr_.get(f"t_{x}"))), INP, N0, RIGHT, fill=FILL_IN)
        put(S2, r, 5, "OU рівня/нахилу кривої НБУ", A9, al=LNW)
        OU_CURVE_ROW[j] = r
    else:
        put(S2, r, 1, f"{rr_['name']}: Δ облікової за 6 міс., б.п. / κ", al=LNW)
        put(S2, r, 2, round(num(rr_["d_i_bp"])), INP, N0, RIGHT, fill=FILL_IN); put(S2, r, 3, num(rr_["kappa"]), INP, N2, RIGHT, fill=FILL_IN)
        put(S2, r, 5, "Монте-Карло OU (14_ou_deposit_dynamics_v6.R); на криву — за Васічеком: ΔR(τ) = Δi·(1 − e^(−κτ))/(κτ); валюта — 0", A9, al=LNW)
        nm(f"ou_d{j}", S2, r, 2); nm(f"ou_k{j}", S2, r, 3)
    r += 1
SEL = r
put(S2, r, 1, "Обрані: гривня", A10B, al=LNW)
put(S2, r + 1, 1, "Обрані: валюта", A10B, al=LNW)
for j in range(3):
    col = CL(2 + j)
    put(S2, r, 2 + j, f'=INDEX({col}{SH0}:{col}{SH0 + 2},MATCH(shk_u,{{"{shock_uah_list[0]}","{shock_uah_list[1]}","{shock_uah_list[2]}"}},0))', A10B, N0, RIGHT, BOX, FILL_KEY)
    put(S2, r + 1, 2 + j, f'=INDEX({col}{SH0 + 3}:{col}{SH0 + 5},MATCH(shk_f,{{"{shock_fx_list[0]}","{shock_fx_list[1]}","{shock_fx_list[2]}"}},0))', A10B, N0, RIGHT, BOX, FILL_KEY)
for cc, rr_ in (("uah", r), ("fx", r + 1)):
    nm(f"Rp_{cc}", S2, rr_, 2); nm(f"Rs_{cc}", S2, rr_, 3); nm(f"Rl_{cc}", S2, rr_, 4)
r += 3
# custom scenario inputs + bucket table
put(S2, r, 1, "13.2.2. Кошики BCBS, базові криві (НБУ, " + dt.date.fromisoformat(DBASE).strftime("%m.%Y") + "), інтерполяція і власний сценарій", A10B); r += 1
labs = ["Кошик", "Початок, р.", "Кінець, р.", "Середина t, р.", "Нижній строк кривої", "Вага верхнього", "База: гривня", "База: валюта", "Мін. межа ставки",
        "Власний: гривня, б.п.", "Власний: валюта, б.п.", "Історичний: гривня", "Історичний: валюта"]
r = header(S2, r, 1, labs, height=42, fill=FILL_HEAD)
BT0 = r
# tenors table helper (row of tenor years) placed to the right: columns P..W
put(S2, BT0 - 3, 16, "Строки кривої, р.", A9I)
for j, ty in enumerate(TENY): put(S2, BT0 - 2, 16 + j, ty, A9, N2, CEN, BOX)
put(S2, BT0 - 1, 15, "База грн", A9); put(S2, BT0, 15, "База вал.", A9)
base_row = H0 + MONTHS.index(DBASE)
for j in range(8):
    put(S2, BT0 - 1, 16 + j, f"='Д2'!{CL(2 + j)}{base_row}", LINKF, P2, RIGHT)
    put(S2, BT0, 16 + j, f"='Д2'!{CL(10 + j)}{base_row}", LINKF, P2, RIGHT)
put(S2, BT0 + 1, 15, "Δ історичний грн", A9); put(S2, BT0 + 2, 15, "Δ історичний вал.", A9)
for j in range(8):
    for q, off in ((0, 2), (1, 10)):
        cc = CL(off + j)
        put(S2, BT0 + 1 + q, 16 + j, f'=IFERROR(INDEX({d2(cc)},MATCH(h_to,{d2("A")},0))-INDEX({d2(cc)},MATCH(h_from,{d2("A")},0)),0)', fmt=P2, al=RIGHT)
TROW = f"$P${BT0 - 2}:$W${BT0 - 2}"
S2.row_dimensions[BT0 - 1].height = None
for k, (labk, a, b, t) in enumerate(BK):
    q = BT0 + k
    put(S2, q, 1, labk, al=LNW); put(S2, q, 2, a, fmt=N4); put(S2, q, 3, b if b is not None else "…", fmt=N4, al=RIGHT); put(S2, q, 4, t, fmt=N4)
    put(S2, q, 5, f"=MAX(1,MIN(7,MATCH(D{q},{TROW},1)))" if t >= 0.25 else "=1", fmt=N0, al=RIGHT)
    put(S2, q, 6, f"=IF(D{q}<=$P${BT0 - 2},0,IF(D{q}>=$W${BT0 - 2},1,(D{q}-INDEX({TROW},E{q}))/(INDEX({TROW},E{q}+1)-INDEX({TROW},E{q}))))", fmt=N4, al=RIGHT)
    for c_, rowb in ((7, BT0 - 1), (8, BT0)):
        put(S2, q, c_, f"=(1-F{q})*INDEX($P${rowb}:$W${rowb},E{q})+F{q}*INDEX($P${rowb}:$W${rowb},E{q}+1)", fmt=P2, al=RIGHT)
    put(S2, q, 9, f'=IF(floor="Так",MIN(0,-0.01+0.0005*D{q}),-1)', fmt=P2, al=RIGHT)
    put(S2, q, 10, 0, INP, N0, RIGHT, fill=FILL_IN); put(S2, q, 11, 0, INP, N0, RIGHT, fill=FILL_IN)
    for c_, rowd in ((12, BT0 + 1), (13, BT0 + 2)):
        put(S2, q, c_, f"=(1-F{q})*INDEX($P${rowd}:$W${rowd},E{q})+F{q}*INDEX($P${rowd}:$W${rowd},E{q}+1)", fmt=P2, al=RIGHT)
BT1 = BT0 + NK - 1
for c_, w in zip("ABCDEFGHIJKLM", [12, 8, 8, 8, 8, 8, 9, 9, 9, 10, 10, 10, 10]): S2.column_dimensions[c_].width = w
for j in range(15, 24): S2.column_dimensions[CL(j)].width = 8
# ΔR and DF tables (rows buckets, columns scenarios) per currency
r = BT1 + 2
DR = {}; DF = {}; RT = {}
for cc, cl in CCY:
    put(S2, r, 1, f"13.2.3. Зміна ставок ΔR за сценаріями, частки — {cl}", A10B); r += 1
    r = header(S2, r, 1, ["Кошик"] + SCN, height=66, numbers=False, fill=FILL_HEAD)
    DR[cc] = r
    basec = "G" if cc == "uah" else "H"; custc = "J" if cc == "uah" else "K"; histc = "L" if cc == "uah" else "M"
    for k in range(NK):
        q = r + k; b = BT0 + k; t = f"$D${b}"
        Ssh = f"Rs_{cc}/10000*EXP(-{t}/4)"; Slg = f"Rl_{cc}/10000*(1-EXP(-{t}/4))"
        f = [f"=Rp_{cc}/10000", f"=-Rp_{cc}/10000", f"=-0.65*{Ssh}+0.9*{Slg}", f"=0.8*{Ssh}-0.6*{Slg}", f"={Ssh}", f"=-{Ssh}",
             "=nbu_all/10000", "=-nbu_all/10000", f"=IF({t}<=0.5,nbu_6m/10000,0)", f"=IF({t}<=0.5,-nbu_6m/10000,0)", f"=${custc}${b}/10000", f"=${histc}${b}"]
        for j in range(NOU):
            if cc != "uah":
                f.append("=0")
            elif j in OU_CURVE_ROW:
                cr = OU_CURVE_ROW[j]
                f.append(f"=((1-$F${b})*INDEX($F${cr}:$M${cr},$E${b})+$F${b}*INDEX($F${cr}:$M${cr},$E${b}+1))/10000")
            else:
                f.append(f"=ou_d{j}/10000*(1-EXP(-ou_k{j}*{t}))/(ou_k{j}*{t})")
        put(S2, q, 1, BK[k][0], al=LNW)
        for s in range(NS):
            put(S2, q, 2 + s, f[s], fmt=P2, al=RIGHT)
    r += NK + 1
    put(S2, r, 1, f"13.2.4. Ставки після шоку з мінімальною межею і коефіцієнти дисконтування — {cl}", A10B); r += 1
    r = header(S2, r, 1, ["Кошик", "DF база"] + [f"DF: {s}" for s in SCN], height=66, numbers=False, fill=FILL_HEAD)
    DF[cc] = r
    for k in range(NK):
        q = r + k; b = BT0 + k
        put(S2, q, 1, BK[k][0], al=LNW)
        put(S2, q, 2, f"=(1+${basec}${b})^(-$D${b})", fmt=N4, al=RIGHT)
        for s in range(NS):
            dr = f"{CL(2 + s)}{DR[cc] + k}"
            put(S2, q, 3 + s, f"=(1+MAX(${basec}${b}+{dr},MIN(${basec}${b},$I${b})))^(-$D${b})", fmt=N4, al=RIGHT)
    r += NK + 2
S2.freeze_panes = "B5"
for j in range(2, 2 + NS + 1): S2.column_dimensions[CL(j)].width = max(S2.column_dimensions[CL(j)].width or 0, 11)
S2_END = foot(S2, r, ["Формули BCBS: короткий шок S(t) = R_short·e^(−t/4), довгий L(t) = R_long·(1 − e^(−t/4)); steepener = −0,65·|S| + 0,9·|L|; flattener = +0,8·|S| − 0,6·|L|.",
                      "Межа після шоку: max(r₀ + ΔR, min(r₀, −1 % + 0,05 %·t)). Крива валюти — крива НБУ для валютних ОВДП (USD)."])

# ------------------------------------------------------------ 13.3 assumptions
r = titles(S3, "13.3. Поведінкові та договірні припущення (редагуються)", "(λ — частка залишку, що переоцінюється щомісяця, з п. 2.3; ставки — купони/ставки статей для грошових потоків)")
put(S3, r, 1, "13.3.1. Статті балансу", A10B); r += 1
labs = ["Стаття", "Бік", "Знак", "Тип", "λ договірна, на міс.", "Ставка: гривня", "Ставка: валюта", "Дострокове погашення / вилучення, % річних",
        "Те саме, на міс.", "Частка ядра (для коштів на вимогу)", "λ ядра, на міс.", "Ключ передачі (п. 2.2)", "Сер. β за 1-й рік", "Сер. β, підвищення", "Сер. β, зниження"]
r = header(S3, r, 1, labs, height=66, fill=FILL_HEAD)
A0 = r
TYPE_UA = {"nmd": "кошти на вимогу", "td": "строкові депозити (TDRR)", "loan": "кредити (CPR)", "oth": "інше"}
NMD_ROW = {}
for k, i in enumerate(IT):
    q = A0 + k
    put(S3, q, 1, ILAB[i], al=LNW); put(S3, q, 2, "актив" if ISIDE[i] == "A" else "зобов'язання")
    put(S3, q, 3, 1 if ISIDE[i] == "A" else -1, fmt=N0, al=RIGHT); put(S3, q, 4, TYPE_UA[TYPE[i]] if ILAM[i] > 0 else "не чутлива до ставки (λ = 0: виключено)", A9)
    put(S3, q, 5, ILAM[i], INP, N4, RIGHT, fill=FILL_IN)
    ru, rf = irates.get(i, (0.0, 0.0))
    put(S3, q, 6, ru, INP, P2, RIGHT, fill=FILL_IN); put(S3, q, 7, rf, INP, P2, RIGHT, fill=FILL_IN)
    put(S3, q, 8, 0.05 if TYPE[i] == "loan" else 0.10 if TYPE[i] == "td" else 0.0, INP, P1, RIGHT, fill=FILL_IN)
    put(S3, q, 9, f"=1-(1-H{q})^(1/12)", fmt=N4, al=RIGHT)
    if i in NMD:
        NMD_ROW[i] = q
    else:
        put(S3, q, 10, 1, fmt=N2, al=RIGHT); put(S3, q, 11, f"=E{q}", fmt=N4, al=RIGHT)
    put(S3, q, 12, IKEY[i] or "—", A9)
    put(S3, q, 13, avgb(IKEY[i], "b"), INP, N4, RIGHT, fill=FILL_IN)
    put(S3, q, 14, avgb(IKEY[i], "b_up"), INP, N4, RIGHT, fill=FILL_IN)
    put(S3, q, 15, avgb(IKEY[i], "b_dn"), INP, N4, RIGHT, fill=FILL_IN)
A1 = A0 + NI - 1
r = A1 + 2
put(S3, r, 1, "13.3.2. Кошти на вимогу: ядро і строк", A10B); r += 1
labs = ["Стаття", "Категорія BCBS", "Ліміт частки ядра", "Ліміт сер. строку ядра, р.", "BCBS: частка ядра (оцінка банку)", "BCBS: сер. строк, р.",
        "Виявлене θ (обране джерело)", "Виявлена частка ядра 1 − θ", "Виявлений строк (дюрація франшизи, п. 2.1), р.", "Обрана частка ядра", "Обраний сер. строк, р.",
        "θ системний (медіана банків)", "θ обраного банку", "OU: ядро (система)", "OU: ядро (обраний банк)"]
r = header(S3, r, 1, labs, height=66, fill=FILL_HEAD)
N0r = r
for k, (i, cat, capc, capm, thc, D0) in enumerate([("hh_dem", "роздрібні трансакційні", 0.9, 5.0, "th_hh", D_hh0), ("corp_dem", "оптові (нефінансові)", 0.5, 4.0, "th_corp", D_corp0)]):
    q = r + k
    put(S3, q, 1, ILAB[i], al=LNW); put(S3, q, 2, cat, A9)
    put(S3, q, 3, capc, INP, P1, RIGHT, fill=FILL_IN); put(S3, q, 4, capm, INP, N1, RIGHT, fill=FILL_IN)
    put(S3, q, 5, capc, INP, P1, RIGHT, fill=FILL_IN); put(S3, q, 6, capm, INP, N1, RIGHT, fill=FILL_IN)
    put(S3, q, 12, TH_MED[i], INP, N2, RIGHT, fill=FILL_IN)
    put(S3, q, 13, f'=IFERROR(INDEX({d1col(thc)},row)*1,"…")', LINKF, N2, RIGHT)
    put(S3, q, 7, f'=IF(AND(thsrc="Банку",ISNUMBER(M{q})),M{q},L{q})', fmt=N2, al=RIGHT)
    oc = num(OUS.get(i, {}).get("core"), capc)
    put(S3, q, 14, oc, INP, P1, RIGHT, fill=FILL_IN)
    put(S3, q, 15, f'=IFERROR(INDEX({d1col("ou_hh" if i == "hh_dem" else "ou_corp")},row)*1,"…")', LINKF, P1, RIGHT)
    ou_sel = f'IF(AND(thsrc="Банку",ISNUMBER(O{q})),O{q},N{q})'
    put(S3, q, 8, f'=IF(ISNUMBER(G{q}),MIN(1,MAX(0,1-G{q})),0)', fmt=P1, al=RIGHT)
    put(S3, q, 9, D0, INP, N2, RIGHT, fill=FILL_IN)
    core_rev = f'IF(nmd="{nmd_list[3]}",{ou_sel},H{q})'
    put(S3, q, 10, f'=IF(nmd="{nmd_list[2]}",0,IF(nmd="{nmd_list[1]}",MIN(E{q},C{q}),IF(nmdcap="Так",MIN({core_rev},C{q}),{core_rev})))', A10B, P1, RIGHT, fill=FILL_KEY)
    put(S3, q, 11, f'=IF(nmd="{nmd_list[2]}",0,IF(nmd="{nmd_list[1]}",MIN(F{q},D{q}),IF(nmdcap="Так",MIN(I{q},D{q}),I{q})))', A10B, N2, RIGHT, fill=FILL_KEY)
    a = NMD_ROW[i]
    put(S3, a, 10, f"=J{q}", LINKF, P1, RIGHT); put(S3, a, 11, f"=IF(K{q}>0,1/(12*K{q}),1)", fmt=N4, al=RIGHT)
    nm(f"c_{i}", S3, q, 10); nm(f"th_{i}", S3, q, 7)
r += 3
put(S3, r, 1, "13.3.3. Множники сценаріїв для дострокових погашень (CPR) і вилучень (TDRR)", A10B); r += 1
r = header(S3, r, 1, ["Група сценаріїв", "Множник CPR (кредити)", "Множник TDRR (строкові депозити)", "Застосовується до"], height=40, fill=FILL_HEAD)
M0 = r
MULT = {"s0": (1, 1), "s1": (0.8, 1.2), "s2": (1.2, 0.8), "s3": (0.8, 0.8), "s4": (1.2, 1.2), "s5": (0.9, 1.2), "s6": (1.1, 0.8)}
for k, g in enumerate(GROUPS):
    q = M0 + k
    put(S3, q, 1, GROUP_LAB[g], al=LNW); put(S3, q, 2, MULT[g][0], INP, N2, RIGHT, fill=FILL_IN); put(S3, q, 3, MULT[g][1], INP, N2, RIGHT, fill=FILL_IN)
    put(S3, q, 4, "; ".join(n for n, gg in SC if gg == g) or "—", A9, al=LNW)
S3.freeze_panes = S3.cell(A0, 2)
S3.column_dimensions["A"].width = 40
for j in range(2, 16): S3.column_dimensions[CL(j)].width = 11
foot(S3, M0 + len(GROUPS), ["Множники CPR/TDRR — за таблицями стандартизованої рамки BCBS (2016); перевірте за першоджерелом перед використанням у звітності.",
                            "Ставки статей — середньозважені по банках на останню дату (S23_item_rates_latest.csv); їх можна замінити ставками конкретного банку."])

# ------------------------------------------------------------ 13.4 cash-flow distributions and factors
r = titles(S4, "13.4. Розподіл грошових потоків переоцінки за кошиками (на 1 грн залишку) і коефіцієнти чутливості (формули; не редагувати)",
           "(потік = номінал, що переоцінюється в кошику, + проценти до переоцінки; кошти на вимогу: (1 − ядро) — O/N, ядро — за поведінковим строком)")
DIST = {}; PNOM = {}
for cc, cl in CCY:
    rate_col = "F" if cc == "uah" else "G"
    for g_i, g in enumerate(GROUPS):
        put(S4, r, 1, f"13.4.{len(DIST) + 1}. Потоки на 1 грн — {cl}, поведінка «{GROUP_LAB[g]}»", A10B); r += 1
        r = header(S4, r, 1, ["Стаття"] + [b[0] for b in BK] + ["Разом"], numbers=False, fill=FILL_HEAD)
        DIST[(cc, g)] = r
        mrow = M0 + g_i
        for k, i in enumerate(IT):
            q = r + k; a3 = A0 + k
            put(S4, q, 1, ILAB[i], al=LNW)
            mult = f"'13.3'!$B${mrow}" if TYPE[i] == "loan" else f"'13.3'!$C${mrow}" if TYPE[i] == "td" else "0"
            lam = f"(1-(1-'13.3'!$K${a3})*(1-'13.3'!$I${a3}*{mult}))"          # effective monthly λ incl. behavioural runoff
            cshare = f"'13.3'!$J${a3}"; rr_ = f"'13.3'!${rate_col}${a3}"
            for kk in range(NK):
                am = f"('13.2'!$B${BT0 + kk}*12)"; bm = f"('13.2'!$C${BT0 + kk}*12)"
                L = lam
                if kk < NK - 1:
                    pr = f"((1-{L})^{am}-(1-{L})^{bm})"
                else:
                    pr = f"((1-{L})^{am})"
                core = f"IF({L}>=1,{1 if kk == 0 else 0},IF({L}<=0,0,{pr}*(1+{rr_}/12/{L})))"
                on = f"(1-{cshare})+" if kk == 0 else ""
                put(S4, q, 2 + kk, f"={on}{cshare}*{core}", fmt=N4, al=RIGHT)
            put(S4, q, 2 + NK, f"=SUM(B{q}:{CL(1 + NK)}{q})", fmt=N4, al=RIGHT)
        # core-only rows for NMD (c = 1) — used in ranking/reverse stress
        for k2, i in enumerate(NMD):
            q = r + NI + k2; a3 = NMD_ROW[i]
            put(S4, q, 1, f"{ILAB[i]}: лише ядро", A10I, al=LNW)
            L = f"'13.3'!$K${a3}"; rr_ = f"'13.3'!${rate_col}${a3}"
            for kk in range(NK):
                am = f"('13.2'!$B${BT0 + kk}*12)"; bm = f"('13.2'!$C${BT0 + kk}*12)"
                pr = f"((1-{L})^{am}-(1-{L})^{bm})" if kk < NK - 1 else f"((1-{L})^{am})"
                put(S4, q, 2 + kk, f"=IF({L}>=1,{1 if kk == 0 else 0},IF({L}<=0,0,{pr}*(1+{rr_}/12/{L})))", fmt=N4, al=RIGHT)
            put(S4, q, 2 + NK, f"=SUM(B{q}:{CL(1 + NK)}{q})", fmt=N4, al=RIGHT)
        r += NI + 3
    # principal-only repricing shares within 1 year (base behaviour) — NII gap
    put(S4, r, 1, f"13.4.{len(DIST) + len(PNOM) + 1}. Частка номіналу, що переоцінюється в кошиках до 1 року (базова поведінка) — {cl}", A10B); r += 1
    r = header(S4, r, 1, ["Стаття"] + [b[0] for b in BK[:NK1Y]], numbers=False, fill=FILL_HEAD)
    PNOM[cc] = r
    for k, i in enumerate(IT):
        q = r + k; a3 = A0 + k
        put(S4, q, 1, ILAB[i], al=LNW)
        lam = f"(1-(1-'13.3'!$K${a3})*(1-'13.3'!$I${a3}))"; cshare = f"'13.3'!$J${a3}"
        for kk in range(NK1Y):
            am = f"('13.2'!$B${BT0 + kk}*12)"; bm = f"('13.2'!$C${BT0 + kk}*12)"
            core = f"IF({lam}>=1,{1 if kk == 0 else 0},IF({lam}<=0,0,(1-{lam})^{am}-(1-{lam})^{bm}))"
            on = f"(1-{cshare})+" if kk == 0 else ""
            put(S4, q, 2 + kk, f"={on}{cshare}*{core}", fmt=N4, al=RIGHT)
    r += NI + 2
# DF rows (horizontal copies) and factors
put(S4, r, 1, "13.4.D. Коефіцієнти дисконтування за кошиками (горизонтальна копія 13.2.4)", A10B); r += 1
DFH = {}
for cc, cl in CCY:
    for s in range(-1, NS):
        put(S4, r, 1, f"DF {cl}: {'база' if s < 0 else SCN[s]}", A9, al=LNW)
        for kk in range(NK):
            put(S4, r, 2 + kk, f"='13.2'!{CL(3 + s if s >= 0 else 2)}{DF[cc] + kk}", fmt=N4, al=RIGHT)
        DFH[(cc, s)] = r; r += 1
r += 1
put(S4, r, 1, "13.4.F. Чутливість EVE на 1 грн залишку: PV(сценарій) − PV(база), за статтями (рядки) і сценаріями (стовпці)", A10B); r += 1
FAC = {}
rowsel = lambda g: GROUPS.index(g)
for cc, cl in CCY:
    put(S4, r, 1, cl, A10B); r += 1
    r = header(S4, r, 1, ["Стаття", "PV база"] + SCN, height=66, numbers=False, fill=FILL_HEAD)
    FAC[cc] = r
    dcols = f"$B${{}}:${CL(1 + NK)}${{}}"
    for k, i in enumerate(IT + [f"{x}_core" for x in NMD] + ["ON"]):
        q = r + k
        if i == "ON":
            put(S4, q, 1, "O/N (1 грн у першому кошику)", A10I, al=LNW)
            put(S4, q, 2, f"=B{DFH[(cc, -1)]}", fmt=N4, al=RIGHT)
            for s in range(NS):
                put(S4, q, 3 + s, f"=B{DFH[(cc, s)]}-B{DFH[(cc, -1)]}", fmt=N4, al=RIGHT)
            continue
        if i.endswith("_core"):
            base_i = i[:-5]; put(S4, q, 1, f"{ILAB[base_i]}: лише ядро", A10I, al=LNW)
            drow0 = DIST[(cc, "s0")] + NI + NMD.index(base_i)
            dr = lambda s: drow0
        else:
            put(S4, q, 1, ILAB[i], al=LNW)
            dr = lambda s, k=k: DIST[(cc, "s0" if s < 0 else SC[s][1])] + k
        rng = lambda rw: f"$B${rw}:${CL(1 + NK)}${rw}"
        put(S4, q, 2, f"=SUMPRODUCT({rng(dr(-1))},{rng(DFH[(cc, -1)])})", fmt=N4, al=RIGHT)
        for s in range(NS):
            put(S4, q, 3 + s, f"=SUMPRODUCT({rng(dr(s))},{rng(DFH[(cc, s)])})-$B{q}", fmt=N4, al=RIGHT)
    r += NI + 4
# NII gap factors per item & scenario: Σ_{k≤1y} Pnom·ΔR(t_k)·(1 − t_k)
put(S4, r, 1, "13.4.G. Чутливість ЧПД за 12 міс. на 1 грн залишку (договірний ГЕП): Σ частка × ΔR × (1 − t)", A10B); r += 1
GFAC = {}
for cc, cl in CCY:
    put(S4, r, 1, cl, A10B); r += 1
    r = header(S4, r, 1, ["Стаття"] + SCN, height=66, numbers=False, fill=FILL_HEAD)
    GFAC[cc] = r
    for k, i in enumerate(IT):
        q = r + k
        put(S4, q, 1, ILAB[i], al=LNW)
        for s in range(NS):
            terms = "+".join(f"{CL(2 + kk)}{PNOM[cc] + k}*'13.2'!{CL(2 + s)}{DR[cc] + kk}*(1-'13.2'!$D${BT0 + kk})" for kk in range(NK1Y))
            put(S4, q, 2 + s, f"={terms}", fmt=N4, al=RIGHT)
    r += NI + 2
S4.freeze_panes = "B5"; S4.column_dimensions["A"].width = 40
for j in range(2, 2 + max(NK + 1, NS + 1)): S4.column_dimensions[CL(j)].width = 9.5
# horizontal factor vectors for the ranking (items in Д1 order): per ccy per scenario
r += 1
put(S4, r, 1, "13.4.V. Вектори для рейтингу: знак × чутливість (горизонтально, порядок статей як у Д1)", A10B); r += 1
VEC = {}
for cc, cl in CCY:
    for s in range(NS):
        put(S4, r, 1, f"EVE {cl}: {SCN[s]}", A9, al=LNW)
        for k in range(NI):
            put(S4, r, 2 + k, f"='13.3'!$C${A0 + k}*{CL(3 + s)}{FAC[cc] + k}", fmt=N4, al=RIGHT)
        VEC[("eve", cc, s)] = f"'13.4'!$B${r}:${CL(1 + NI)}${r}"; r += 1
    for s in range(NS):
        put(S4, r, 1, f"ЧПД (ГЕП) {cl}: {SCN[s]}", A9, al=LNW)
        for k in range(NI):
            put(S4, r, 2 + k, f"='13.3'!$C${A0 + k}*{CL(2 + s)}{GFAC[cc] + k}", fmt=N4, al=RIGHT)
        VEC[("nii", cc, s)] = f"'13.4'!$B${r}:${CL(1 + NI)}${r}"; r += 1
for s in range(NS):                       # revealed NII per 1 UAH: sign × avg β × ΔR at the policy end (UAH only)
    put(S4, r, 1, f"ЧПД (виявлена β) гривня: {SCN[s]}", A9, al=LNW)
    for k in range(NI):
        a3 = A0 + k
        bsel = f"IF(asym=\"Так\",IF('13.2'!{CL(2 + s)}{DR['uah']}>=0,'13.3'!$N${a3},'13.3'!$O${a3}),'13.3'!$M${a3})"
        put(S4, r, 2 + k, f"='13.3'!$C${a3}*{bsel}*'13.2'!{CL(2 + s)}{DR['uah']}", fmt=N4, al=RIGHT)
    VEC[("niir", "uah", s)] = f"'13.4'!$B${r}:${CL(1 + NI)}${r}"; r += 1

# ------------------------------------------------------------ 13.5 bank ladder
r = titles(S5, "13.5. Банк: драбина переоцінки, ключові строкові чутливості (PV01) і внесок статей у ΔEVE", "(млн грн; знак: + актив, − зобов'язання)")
put(S5, 4, 1, '="Банк: "&bank', A10I)
LAD = {}
for cc, cl in CCY:
    put(S5, r, 1, f"13.5.{len(LAD) + 1}. Потоки переоцінки банку за кошиками — {cl} (базова поведінка)", A10B); r += 1
    r = header(S5, r, 1, ["Стаття", "Залишок"] + [b[0] for b in BK], numbers=False, fill=FILL_HEAD)
    LAD[cc] = r
    pref = "U_" if cc == "uah" else "F_"
    for k, i in enumerate(IT):
        q = r + k
        put(S5, q, 1, ILAB[i], al=LNW)
        put(S5, q, 2, f"=INDEX({d1col(pref + i)},row)", LINKF, N1, RIGHT)
        for kk in range(NK):
            put(S5, q, 3 + kk, f"='13.3'!$C${A0 + k}*$B{q}*'13.4'!{CL(2 + kk)}{DIST[(cc, 's0')] + k}", fmt=N1, al=RIGHT)
    q = r + NI
    put(S5, q, 1, "Чистий потік (розрив) у кошику", A10B, al=LNW, border=TOPB); put(S5, q, 2, None, border=TOPB)
    for kk in range(NK):
        put(S5, q, 3 + kk, f"=SUM({CL(3 + kk)}{r}:{CL(3 + kk)}{q - 1})", A10B, N1, RIGHT, TOPB)
    put(S5, q + 1, 1, "Накопичений розрив", A10I, al=LNW)
    for kk in range(NK):
        put(S5, q + 1, 3 + kk, f"=SUM($C{q}:{CL(3 + kk)}{q})", A10I, N1, RIGHT)
    put(S5, q + 2, 1, "PV01: зміна EVE при +1 б.п. у кошику", A10I, al=LNW)
    bc = "G" if cc == "uah" else "H"
    for kk in range(NK):
        put(S5, q + 2, 3 + kk, f"=-{CL(3 + kk)}{q}*'13.2'!$D${BT0 + kk}*(1+'13.2'!${bc}${BT0 + kk})^(-'13.2'!$D${BT0 + kk}-1)*0.0001", A10I, N2, RIGHT)
    put(S5, q + 2, 2, f"=SUM(C{q + 2}:{CL(2 + NK)}{q + 2})", A10B, N2, RIGHT)
    LAD[(cc, "net")] = q; LAD[(cc, "pv01")] = q + 2
    r = q + 4
put(S5, r, 1, "13.5.3. Внесок статей у ΔEVE за обраного сценарію деталізації", A10B); r += 1
r = header(S5, r, 1, ["Стаття", "Гривня: залишок", "Гривня: ΔEVE", "Валюта: залишок", "Валюта: ΔEVE", "Разом ΔEVE", "% Tier 1"], numbers=True, fill=FILL_HEAD)
CTR0 = r
for k, i in enumerate(IT):
    q = r + k
    put(S5, q, 1, ILAB[i], al=LNW)
    put(S5, q, 2, f"=B{LAD['uah'] + k}", fmt=N1, al=RIGHT)
    put(S5, q, 3, f"='13.3'!$C${A0 + k}*B{q}*INDEX('13.4'!$C${FAC['uah'] + k}:${CL(2 + NS)}${FAC['uah'] + k},det_n)", fmt=N1, al=RIGHT)
    put(S5, q, 4, f"=B{LAD['fx'] + k}", fmt=N1, al=RIGHT)
    put(S5, q, 5, f"='13.3'!$C${A0 + k}*D{q}*INDEX('13.4'!$C${FAC['fx'] + k}:${CL(2 + NS)}${FAC['fx'] + k},det_n)", fmt=N1, al=RIGHT)
    put(S5, q, 6, f"=C{q}+E{q}", fmt=N1, al=RIGHT); put(S5, q, 7, f"=F{q}/T1", fmt=P1, al=RIGHT)
q = r + NI
put(S5, q, 1, "Разом", A10B, al=LNW, border=TOPB)
for c in range(2, 8):
    put(S5, q, c, f"=SUM({CL(c)}{r}:{CL(c)}{q - 1})" if c != 7 else f"=F{q}/T1", A10B, N1 if c != 7 else P1, RIGHT, TOPB)
CTR_TOT = q
S5.freeze_panes = "C5"; S5.column_dimensions["A"].width = 40; S5.column_dimensions["B"].width = 11
for j in range(3, 3 + NK): S5.column_dimensions[CL(j)].width = 9.5
foot(S5, q + 1, ["PV01 > 0 — EVE зростає при зростанні ставки в цьому кошику. Сума PV01 ≈ чутливість EVE до паралельного зсуву на 1 б.п."])

# ------------------------------------------------------------ 13.1 results table (EVE + NII)
r = SW_END + 1
put(S1, r, 1, "13.1.1. Результати за сценаріями", A10B); r += 1
labs = ["Сценарій", "ΔEVE: гривня", "ΔEVE: валюта", "Втрата EVE (агрегація BCBS)", "Втрата EVE, % Tier 1", "Тест на викид EVE",
        "ΔЧПД: гривня", "ΔЧПД: валюта", "ΔЧПД: разом", "ΔЧПД, % ЧПД", "ΔЧПД, % Tier 1", "Тест на викид ЧПД"]
RS0 = header(S1, r, 1, labs, height=54, fill=FILL_HEAD)
for s in range(NS):
    q = RS0 + s
    put(S1, q, 1, SCN[s], al=LNW)
    Urow = f"INDEX('Д1'!${U0}${R1}:${U1}${RL},row,0)"; Frow = f"INDEX('Д1'!${F0}${R1}:${F1}${RL},row,0)"
    put(S1, q, 2, f"=SUMPRODUCT({Urow},{VEC[('eve', 'uah', s)]})", fmt=N1, al=RIGHT)
    put(S1, q, 3, f"=SUMPRODUCT({Frow},{VEC[('eve', 'fx', s)]})", fmt=N1, al=RIGHT)
    put(S1, q, 4, f"=MAX(0,MAX(-B{q},0)+MAX(-C{q},0)+0.5*(MIN(-B{q},0)+MIN(-C{q},0)))", A10B, N1, RIGHT)
    put(S1, q, 5, f"=D{q}/T1", A10B, P1, RIGHT)
    put(S1, q, 6, f'=IF(E{q}>sot,"порушено","ні")', al=CEN)
    put(S1, q, 7, f'=IF(nii="{nii_list[1]}",SUMPRODUCT({Urow},{VEC[("niir", "uah", s)]}),SUMPRODUCT({Urow},{VEC[("nii", "uah", s)]}))', fmt=N1, al=RIGHT)
    put(S1, q, 8, f"=SUMPRODUCT({Frow},{VEC[('nii', 'fx', s)]})", fmt=N1, al=RIGHT)
    put(S1, q, 9, f"=G{q}+H{q}", A10B, N1, RIGHT)
    put(S1, q, 10, f'=IFERROR(I{q}/NII12,"…")', fmt=P1, al=RIGHT)
    put(S1, q, 11, f"=I{q}/T1", fmt=P1, al=RIGHT)
    put(S1, q, 12, f'=IF(-K{q}>sot_nii,"порушено","ні")' if s in (6, 7, 0, 1) else "–", al=CEN)
RS1 = RS0 + NS - 1
S1.conditional_formatting.add(f"F{RS0}:F{RS1}", CellIsRule(operator="equal", formula=['"порушено"'], fill=RED))
S1.conditional_formatting.add(f"L{RS0}:L{RS1}", CellIsRule(operator="equal", formula=['"порушено"'], fill=RED))
S1.conditional_formatting.add(f"E{RS0}:E{RS1}", ColorScaleRule(start_type="num", start_value=0, start_color="FFFFFF", end_type="num", end_value=0.3, end_color="F8696B"))
S1.cell(DET_ROW, 2).value = f"=MATCH(detail,$A${RS0}:$A${RS1},0)"
# KPIs
r = RS1 + 2
put(S1, r, 1, "13.1.2. Ключові показники", A10B); r += 1
K0 = r
hist_sheet = "'13.7'"
KP = [
    ("EVE під ризиком: найбільша втрата за шести сценаріїв BCBS, млн грн", f"=MAX(D{RS0}:D{RS0 + 5})", N1),
    ("… у % Tier 1", f"=B{K0}/T1", P1),
    ("… сценарій", f"=INDEX(A{RS0}:A{RS0 + 5},MATCH(B{K0},D{RS0}:D{RS0 + 5},0))", None),
    ("Найбільша втрата EVE за всіма сценаріями, % Tier 1", f"=MAX(E{RS0}:E{RS1})", P1),
    ("ЧПД під ризиком: найгірша ΔЧПД за паралельних ±200 б.п. (НБУ), % Tier 1", f"=MIN(K{RS0 + 6}:K{RS0 + 7})", P1),
    ("Чутливість EVE до паралельного +1 б.п. (сума PV01), млн грн", f"='13.5'!B{LAD[('uah', 'pv01')]}+'13.5'!B{LAD[('fx', 'pv01')]}", N2),
    ("Зворотний стрес: паралельний шок гривні, що порушує поріг EVE (вгору / вниз), б.п.", f"='13.8'!B{{REV}}", None),
    ("Історичне моделювання: 99-й процентиль втрати EVE, % Tier 1", f"={hist_sheet}!B{{HQ}}", P1),
    ("Історичне моделювання: найбільша втрата EVE, % Tier 1", f"={hist_sheet}!B{{HMX}}", P1),
    ("Частка ядра коштів ФО / СГ на вимогу (обрана модель)", '=TEXT(c_hh_dem,"0%")&" / "&TEXT(c_corp_dem,"0%")', None),
    ("ЧПД під ризиком: OU Монте-Карло, 1-й процентиль, % Tier 1", "='13.11'!B{NQ1}", P1),
    ("ЧПД під ризиком: OU Монте-Карло, 1-й процентиль, % ЧПД", "='13.11'!B{NQ2}", P1),
]
KROW = {}
for k, (labk, f, fmt) in enumerate(KP):
    put(S1, K0 + k, 1, labk, al=LEFT, border=BOX); put(S1, K0 + k, 2, f, A10B, fmt, RIGHT, BOX); KROW[k] = K0 + k
S1.column_dimensions["A"].width = 52; S1.column_dimensions["B"].width = 24; S1.column_dimensions["C"].width = 14
for j in range(4, 13): S1.column_dimensions[CL(j)].width = 12
S1.freeze_panes = "A5"
r1_end = foot(S1, K0 + len(KP), ["Втрата EVE = −ΔEVE; агрегація BCBS: втрати за валютами сумуються, прибутки — з вагою 50 %, нижня межа 0.",
                                  "ΔЧПД — 12 міс., постійний баланс. Договірний ГЕП (НБУ, п. 246 Постанови № 64): Σ розрив кошика × ΔR × (1 − t). Виявлений: Σ залишок × середня β за 1-й рік × ΔR облікової.",
                                  "Tier 1 — балансовий капітал банку, якщо не введено регулятивний основний капітал (перемикач)."])
# dropdowns
dv_list(S1, NAMES["bank"].split("!")[1].replace("$", ""), f"'Д1'!$B${R1}:$B${RL}")
for key, lst in (("thsrc", th_list), ("shk_u", shock_uah_list), ("shk_f", shock_fx_list), ("nmd", nmd_list), ("nii", nii_list), ("nmdcap", yes), ("floor", yes), ("asym", yes)):
    dv_list(S1, NAMES[key].split("!")[1].replace("$", ""), '"' + ",".join(lst) + '"')
dv_list(S1, NAMES["hwin"].split("!")[1].replace("$", ""), '"6,12"')
dv_list(S1, NAMES["detail"].split("!")[1].replace("$", ""), f"$A${RS0}:$A${RS1}")
for key in ("h_from", "h_to"):
    dv_list(S1, NAMES[key].split("!")[1].replace("$", ""), f"'Д2'!$A${H0}:$A${HL}")

# ------------------------------------------------------------ 13.6 NII detail
r = titles(S6, "13.6. Чистий процентний дохід: договірний ГЕП проти виявленої передачі (п. 2.2)", "(млн грн за 12 міс.; постійний баланс; гривня)")
put(S6, 4, 1, '="Банк: "&bank', A10I)
r = header(S6, r, 1, ["Стаття", "Залишок, гривня", "Частка номіналу, що переоцінюється до 1 року", "Сер. β за 1-й рік (п. 2.2)",
                      "ΔЧПД +100 б.п.: договірний ГЕП", "ΔЧПД +100 б.п.: виявлена β", "Різниця (виявлений − договірний)"], height=54, fill=FILL_HEAD)
N60 = r
for k, i in enumerate(IT):
    q = r + k; a3 = A0 + k
    put(S6, q, 1, ILAB[i], al=LNW)
    put(S6, q, 2, f"=INDEX({d1col('U_' + i)},row)", LINKF, N1, RIGHT)
    put(S6, q, 3, f"=SUM('13.4'!B{PNOM['uah'] + k}:{CL(1 + NK1Y)}{PNOM['uah'] + k})", fmt=P1, al=RIGHT)
    put(S6, q, 4, f"='13.3'!M{a3}", LINKF, N2, RIGHT)
    terms = "+".join(f"'13.4'!{CL(2 + kk)}{PNOM['uah'] + k}*(1-'13.2'!$D${BT0 + kk})" for kk in range(NK1Y))
    put(S6, q, 5, f"='13.3'!$C${a3}*B{q}*({terms})*0.01", fmt=N1, al=RIGHT)
    put(S6, q, 6, f"='13.3'!$C${a3}*B{q}*D{q}*0.01", fmt=N1, al=RIGHT)
    put(S6, q, 7, f"=F{q}-E{q}", fmt=N1, al=RIGHT)
q = r + NI
put(S6, q, 1, "Разом", A10B, al=LNW, border=TOPB)
for c in range(2, 8):
    put(S6, q, c, f"=SUM({CL(c)}{r}:{CL(c)}{q - 1})" if c in (2, 5, 6, 7) else None, A10B, N1, RIGHT, TOPB)
put(S6, q + 1, 1, "у % ЧПД за 12 міс.", A10I, al=LNW)
for c in (5, 6, 7): put(S6, q + 1, c, f'=IFERROR({CL(c)}{q}/NII12,"…")', A10I, P1, RIGHT)
S6.column_dimensions["A"].width = 40
for j in range(2, 8): S6.column_dimensions[CL(j)].width = 15
foot(S6, q + 2, ["Розбіжність стовпців 5 і 6 — головний результат п. 2.3: договірні строки переоцінки не описують, як банки насправді змінюють ставки.",
                 "Для коштів на вимогу договірний ГЕП припускає миттєву повну переоцінку (або поведінкове ядро за обраною моделлю), виявлена β — фактичну слабку передачу."])

# ------------------------------------------------------------ 13.7 historical simulation
r = titles(S7, "13.7. Історичне моделювання: зміни кривої НБУ за кожне вікно, застосовані до поточного балансу банку",
           "(ΔEVE, млн грн; кожен рядок — зміна кривої за «вікно» місяців, що закінчується в цьому місяці; базова поведінка)")
put(S7, 4, 1, '="Банк: "&bank', A10I)
SUMR = r
put(S7, r, 1, "Квантилі втрати EVE, % Tier 1", A10B); r += 1
STATS = [("95-й процентиль втрати", 0.95), ("99-й процентиль втрати", 0.99), ("Найбільша втрата", None), ("Найбільший прибуток", "min"), ("Кількість вікон", "n")]
SROW = {}
for k, (labk, qv) in enumerate(STATS):
    put(S7, r + k, 1, labk, al=LEFT, border=BOX); SROW[k] = r + k
r += len(STATS) + 1
# weights matrix: bucket × 8 tenors, horizontal per bucket, for SUMPRODUCT
put(S7, r, 1, "Ваги інтерполяції кошиків на строки кривої (службово)", A9I); r += 1
WROW = {}
for kk in range(NK):
    q = r + kk
    put(S7, q, 1, BK[kk][0], A9)
    for j in range(8):
        put(S7, q, 2 + j, f"=IF('13.2'!$E${BT0 + kk}={j + 1},1-'13.2'!$F${BT0 + kk},0)+IF('13.2'!$E${BT0 + kk}+1={j + 1},'13.2'!$F${BT0 + kk},0)", A9, N2, RIGHT)
    WROW[kk] = q
r += NK + 1
labs = ["Кінець вікна", "Δ 3м, грн", "Δ 1р, грн", "Δ 5р, грн", "ΔEVE: гривня", "ΔEVE: валюта", "Втрата EVE (BCBS)", "Втрата, % Tier 1"]
r = header(S7, r, 1, labs, height=40, fill=FILL_HEAD)
HS0 = r
# per-window helper: Δ tenors uah (8) and fx (8) in columns J..Y, bucket contributions in AA.. (19 uah) and AT.. (19 fx)
CU0 = 10; CF0 = 18; BU0 = 27; BF0 = 27 + NK
put(S7, HS0 - 3, CU0, "Δ кривої гривні за вікно (3м…10р)", A9I); put(S7, HS0 - 3, CF0, "Δ кривої валюти за вікно", A9I)
put(S7, HS0 - 3, BU0, "Внески кошиків у ΔEVE: гривня", A9I); put(S7, HS0 - 3, BF0, "Внески кошиків у ΔEVE: валюта", A9I)
net_u = LAD[("uah", "net")]; net_f = LAD[("fx", "net")]
for n, m in enumerate(MONTHS):
    q = HS0 + n; idx = n + 1
    put(S7, q, 1, dt.date.fromisoformat(m), fmt=MON)
    for j in range(8):
        for off, base in ((CU0, 2), (CF0, 10)):
            col = CL(base + j)
            put(S7, q, off + j, f'=IF({idx}-hwin<1,"",IFERROR(INDEX({d2(col)},{idx})-INDEX({d2(col)},{idx}-hwin),""))', A9, P2, RIGHT)
    put(S7, q, 2, f"=J{q}", fmt=P2, al=RIGHT); put(S7, q, 3, f"=L{q}", fmt=P2, al=RIGHT); put(S7, q, 4, f"=O{q}", fmt=P2, al=RIGHT)
    for cc, off, boff, netr, basec in (("uah", CU0, BU0, net_u, "G"), ("fx", CF0, BF0, net_f, "H")):
        dvec = f"${CL(off)}{q}:${CL(off + 7)}{q}"
        for kk in range(NK):
            b = BT0 + kk
            put(S7, q, boff + kk, f"=IFERROR('13.5'!{CL(3 + kk)}${netr}*((1+MAX('13.2'!${basec}${b}+SUMPRODUCT($B${WROW[kk]}:$I${WROW[kk]},{dvec}),MIN('13.2'!${basec}${b},'13.2'!$I${b})))^(-'13.2'!$D${b})-'13.2'!$B${DF[cc] + kk}),\"\")", A9, N1, RIGHT)
    put(S7, q, 5, f'=IF(COUNT({CL(CU0)}{q}:{CL(CU0 + 7)}{q})<6,"",SUM({CL(BU0)}{q}:{CL(BU0 + NK - 1)}{q}))', fmt=N1, al=RIGHT)
    put(S7, q, 6, f'=IF(COUNT({CL(CF0)}{q}:{CL(CF0 + 7)}{q})<6,0,SUM({CL(BF0)}{q}:{CL(BF0 + NK - 1)}{q}))', fmt=N1, al=RIGHT)
    put(S7, q, 7, f'=IF(ISNUMBER(E{q}),MAX(-E{q},0)+MAX(-F{q},0)+0.5*(MIN(-E{q},0)+MIN(-F{q},0)),"")', fmt=N1, al=RIGHT)
    put(S7, q, 8, f'=IF(ISNUMBER(G{q}),G{q}/T1,"")', A10B, P1, RIGHT)
HS1 = HS0 + len(MONTHS) - 1
hr = f"$H${HS0}:$H${HS1}"
for k, (labk, qv) in enumerate(STATS):
    f = {0: f"=PERCENTILE({hr},0.95)", 1: f"=PERCENTILE({hr},0.99)", 2: f"=MAX({hr})", 3: f"=MIN({hr})", 4: f"=COUNT({hr})"}[k]
    put(S7, SROW[k], 2, f, A10B, N0 if k == 4 else P1, RIGHT, BOX)
S7.conditional_formatting.add(f"H{HS0}:H{HS1}", ColorScaleRule(start_type="num", start_value=-0.1, start_color="63BE7B", mid_type="num", mid_value=0, mid_color="FFFFFF", end_type="num", end_value=0.3, end_color="F8696B"))
S7.freeze_panes = S7.cell(HS0, 2); S7.column_dimensions["A"].width = 30
for j in range(2, 9): S7.column_dimensions[CL(j)].width = 12
for j in range(9, BF0 + NK + 1): S7.column_dimensions[CL(j)].width = 7
foot(S7, HS1 + 1, ["Агрегація валют — як у BCBS. Вікна з неповною кривою пропущено. Військовий 2022 рік дає найбільші зміни кривої."])
KP_HQ, KP_HMX = SROW[1], SROW[2]

# ------------------------------------------------------------ 13.8 reverse stress + franchise run heat map
r = titles(S8, "13.8. Зворотний стрес-тест: паралельний шок гривні × відтік ядра коштів на вимогу (депозитна франшиза)",
           "(втрата EVE у % Tier 1; валютні статті незмінні; відтік — частка ядра, що стає O/N: «біг з франшизи», Drechsler, Savov & Schnabl, 2023)")
put(S8, 4, 1, '="Банк: "&bank', A10I)
SHK = [-500, -400, -300, -200, -100, 0, 100, 200, 300, 400, 500]
OUT = [0, 0.1, 0.2, 0.3, 0.4, 0.5]
put(S8, r, 1, "Службово: PV на 1 грн за паралельного шоку гривні", A9I); r += 1
put(S8, r, 1, "Шок, б.п.", A9, border=BOX)
for j, s in enumerate(SHK): put(S8, r, 2 + j, s, A9, N0, CEN, BOX)
SHROW = r; r += 1
DFP = {}
for kk in range(NK):
    put(S8, r, 1, f"DF {BK[kk][0]}", A9)
    b = BT0 + kk
    for j in range(len(SHK)):
        put(S8, r, 2 + j, f"=(1+MAX('13.2'!$G${b}+{CL(2 + j)}${SHROW}/10000,MIN('13.2'!$G${b},'13.2'!$I${b})))^(-'13.2'!$D${b})", A9, N4, RIGHT)
    DFP[kk] = r; r += 1
dfcol = lambda j: f"{CL(2 + j)}${DFP[0]}:{CL(2 + j)}${DFP[NK - 1]}"
PVR = {}
for k, i in enumerate(IT + ["hh_dem_core", "corp_dem_core"]):
    put(S8, r, 1, f"PV: {ILAB.get(i, ILAB.get(i[:-5], i) + ': ядро')}", A9)
    drow = DIST[("uah", "s0")] + k if k < NI else DIST[("uah", "s0")] + NI + (k - NI)
    for j in range(len(SHK)):
        # SUMPRODUCT of a horizontal distribution row and a vertical DF column → via TRANSPOSE-free MMULT-free loop
        terms = "+".join(f"'13.4'!{CL(2 + kk)}{drow}*{CL(2 + j)}{DFP[kk]}" for kk in range(NK))
        put(S8, r, 2 + j, f"={terms}", A9, N4, RIGHT)
    PVR[i] = r; r += 1
put(S8, r, 1, "PV: O/N", A9)
for j in range(len(SHK)): put(S8, r, 2 + j, f"={CL(2 + j)}{DFP[0]}", A9, N4, RIGHT)
PVR["ON"] = r; r += 2
z = SHK.index(0)
put(S8, r, 1, "Втрата EVE, % Tier 1", A10B); r += 1
put(S8, r, 1, "Відтік ядра ↓ / шок гривні, б.п. →", A9, al=CEN, border=BOX)
for j, s in enumerate(SHK): put(S8, r, 2 + j, s, A10B, N0, CEN, BOX, FILL_HEAD)
GH = r; r += 1
G0 = r
for n_o, o in enumerate(OUT):
    q = r + n_o
    put(S8, q, 1, o, A10B, P1 if True else None, CEN, BOX, FILL_HEAD)
    for j in range(len(SHK)):
        col = CL(2 + j); zc = CL(2 + z)
        non = "+".join(f"'13.3'!$C${A0 + k}*'13.5'!$B${LAD['uah'] + k}*({col}${PVR[i]}-{zc}${PVR[i]})" for k, i in enumerate(IT) if i not in NMD)
        nmdp = "+".join(
            f"-'13.5'!$B${LAD['uah'] + IT.index(i)}*(((1-c_{i}*(1-$A{q}))*{col}${PVR['ON']}+c_{i}*(1-$A{q})*{col}${PVR[i + '_core']})-((1-c_{i})*{zc}${PVR['ON']}+c_{i}*{zc}${PVR[i + '_core']}))"
            for i in NMD)
        put(S8, q, 2 + j, f"=MAX(0,-({non}{nmdp}))/T1", fmt=P1, al=RIGHT, border=BOX)
G1 = r + len(OUT) - 1
S8.conditional_formatting.add(f"B{G0}:{CL(1 + len(SHK))}{G1}", ColorScaleRule(start_type="num", start_value=0, start_color="FFFFFF", mid_type="num", mid_value=0.15, mid_color="FFEB84", end_type="num", end_value=0.4, end_color="F8696B"))
r = G1 + 2
put(S8, r, 1, "Шок гривні (без відтоку), за якого втрата EVE сягає порогу, б.п. (лінійна інтерполяція; «> 500» — не досягається)", A10B, al=LEFT); r += 1
REVR = r
row0 = f"$B${G0}:${CL(1 + len(SHK))}${G0}"
put(S8, r, 1, "Найменший шок вгору / вниз, що порушує поріг", al=LEFT, border=BOX)
nu = f'COUNTIF($H${G0}:$L${G0},"<"&sot)'; nd = f'COUNTIF($B${G0}:$F${G0},"<"&sot)'
put(S8, r, 2, f'=IF({nu}<5,"+"&({nu}+1)*100,"> +500")&" / "&IF({nd}<5,"−"&({nd}+1)*100,"< −500")', A10B, None, RIGHT, BOX)
put(S8, r, 3, "Порогова сітка 100 б.п.; точніше — див. 13.1 (PV01).", A9)
S8.column_dimensions["A"].width = 46
for j in range(2, 2 + len(SHK)): S8.column_dimensions[CL(j)].width = 9
foot(S8, r + 1, ["Відтік ядра переводить частину стабільних коштів у O/N: депозитна франшиза (PV ядра < номіналу) зникає, EVE падає навіть без зміни ставок.",
                 "Сітку можна використовувати як карту ризику: червоні клітинки — поєднання шоку і відтоку, за яких втрата EVE перевищує поріг."])

# ------------------------------------------------------------ 13.9 ranking of all banks
r = titles(S9, "13.9. Стрес-тест усіх банків за обраних припущень", "(втрата EVE — % Tier 1 (балансовий капітал); ΔЧПД — % Tier 1; фільтр і сортування — у шапці)")
labs = ["Код", "Банк", "Власність", "Активи, млн грн", "Tier 1, млн грн"] + [f"Втрата EVE: {SCN[s]}" for s in range(6)] + \
       ["EVE під ризиком (макс. BCBS), %", "Найгірший сценарій BCBS", "Тест на викид EVE", "Втрата EVE: НБУ +200", "Втрата EVE: НБУ −200",
        "ΔЧПД +200 (НБУ), %", "ΔЧПД −200 (НБУ), %", "Тест на викид ЧПД", "Ранг ризику EVE", "ЧПД під ризиком OU 1 % (Монте-Карло), % Tier 1"]
R9 = header(S9, r, 1, labs, height=80, fill=FILL_HEAD)
HELP0 = len(labs) + 2           # helper columns: E_uah, E_fx per scenario (10 scenarios) + NII
for n in range(NB):
    q = R9 + n; d = R1 + n
    Ur = f"'Д1'!${U0}{d}:${U1}{d}"; Fr = f"'Д1'!${F0}{d}:${F1}{d}"
    put(S9, q, 1, f"='Д1'!A{d}", LINKF); put(S9, q, 2, f"='Д1'!B{d}", LINKF, al=LNW); put(S9, q, 3, f"='Д1'!C{d}", LINKF)
    put(S9, q, 4, f"='Д1'!{CL(DC['TA'])}{d}", LINKF, N0, RIGHT); put(S9, q, 5, f"='Д1'!{CL(DC['EQ'])}{d}", LINKF, N0, RIGHT)
    loss_cols = []
    for s in list(range(6)) + [6, 7]:
        hc = HELP0 + 2 * (s if s < 6 else s)
        corr = {}
        for cc in ("uah", "fx"):
            pref = "U_" if cc == "uah" else "F_"
            terms = []
            for i, thc in (("hh_dem", "th_hh"), ("corp_dem", "th_corp")):
                k = IT.index(i)
                cb = f"IF(ISNUMBER('Д1'!{CL(DC[thc])}{d}),MIN(IF(nmdcap=\"Так\",'13.3'!$C${N0r + NMD.index(i)},1),MAX(0,1-'Д1'!{CL(DC[thc])}{d})),0)"
                diff = f"('13.4'!{CL(3 + s)}{FAC[cc] + NI + NMD.index(i)}-'13.4'!{CL(3 + s)}{FAC[cc] + NI + 2})"
                terms.append(f"-'Д1'!{CL(DC[pref + i])}{d}*({cb}-c_{i})*{diff}")
            terms_ou = []
            for i, oc_ in (("hh_dem", "ou_hh"), ("corp_dem", "ou_corp")):
                cb = f"IF(ISNUMBER('Д1'!{CL(DC[oc_])}{d}),MIN(IF(nmdcap=\"Так\",'13.3'!$C${N0r + NMD.index(i)},1),'Д1'!{CL(DC[oc_])}{d}),c_{i})"
                diff = f"('13.4'!{CL(3 + s)}{FAC[cc] + NI + NMD.index(i)}-'13.4'!{CL(3 + s)}{FAC[cc] + NI + 2})"
                terms_ou.append(f"-'Д1'!{CL(DC[pref + i])}{d}*({cb}-c_{i})*{diff}")
            corr[cc] = f'IF(thsrc="Банку",IF(nmd="{nmd_list[0]}",{"+".join(terms)},IF(nmd="{nmd_list[3]}",{"+".join(terms_ou)},0)),0)'
        put(S9, q, hc, f"=SUMPRODUCT({Ur},{VEC[('eve', 'uah', s)]})+{corr['uah']}", A9, N1, RIGHT)
        put(S9, q, hc + 1, f"=SUMPRODUCT({Fr},{VEC[('eve', 'fx', s)]})+{corr['fx']}", A9, N1, RIGHT)
        a, b_ = f"{CL(hc)}{q}", f"{CL(hc + 1)}{q}"
        loss = f"MAX(0,MAX(-{a},0)+MAX(-{b_},0)+0.5*(MIN(-{a},0)+MIN(-{b_},0)))/E{q}"
        if s < 6:
            put(S9, q, 6 + s, f"={loss}", fmt=P1, al=RIGHT)
        else:
            put(S9, q, 15 + (s - 6), f"={loss}", fmt=P1, al=RIGHT)
    put(S9, q, 12, f"=MAX(F{q}:K{q})", A10B, P1, RIGHT)
    put(S9, q, 13, f"=INDEX($F${R9 - 3}:$K${R9 - 3},MATCH(L{q},F{q}:K{q},0))", A9, al=LNW)
    put(S9, q, 14, f'=IF(L{q}>sot,"порушено","ні")', al=CEN)
    for j, s in enumerate((6, 7)):
        hc = HELP0 + 20 + j
        nii_u = f'IF(nii="{nii_list[1]}",SUMPRODUCT({Ur},{VEC[("niir", "uah", s)]}),SUMPRODUCT({Ur},{VEC[("nii", "uah", s)]}))'
        put(S9, q, hc, f"={nii_u}+SUMPRODUCT({Fr},{VEC[('nii', 'fx', s)]})", A9, N1, RIGHT)
        put(S9, q, 17 + j, f"={CL(hc)}{q}/E{q}", fmt=P1, al=RIGHT)
    put(S9, q, 19, f'=IF(-MIN(Q{q}:R{q})>sot_nii,"порушено","ні")', al=CEN)
    put(S9, q, 20, f"=RANK(L{q},$L${R9}:$L${R9 + NB - 1},0)", fmt=N0, al=CEN)
    put(S9, q, 21, f"=IF(ISNUMBER('Д1'!{CL(DC['ou_nii_q1'])}{d}),'Д1'!{CL(DC['ou_nii_q1'])}{d}/E{q},\"…\")", fmt=P1, al=RIGHT)
R9L = R9 + NB - 1
for c_ in ("N", "S"):
    S9.conditional_formatting.add(f"{c_}{R9}:{c_}{R9L}", CellIsRule(operator="equal", formula=['"порушено"'], fill=RED))
S9.conditional_formatting.add(f"F{R9}:L{R9L}", ColorScaleRule(start_type="num", start_value=0, start_color="FFFFFF", mid_type="num", mid_value=0.15, mid_color="FFEB84", end_type="num", end_value=0.4, end_color="F8696B"))
q = R9L + 1
put(S9, q, 2, "Частка банків, що порушують тест EVE / ЧПД", A10B, al=LNW, border=TOPB)
put(S9, q, 14, f'=COUNTIF(N{R9}:N{R9L},"порушено")/COUNTA(N{R9}:N{R9L})', A10B, P1, RIGHT, TOPB)
put(S9, q, 19, f'=COUNTIF(S{R9}:S{R9L},"порушено")/COUNTA(S{R9}:S{R9L})', A10B, P1, RIGHT, TOPB)
put(S9, q + 1, 2, "Система: EVE під ризиком, зважено за Tier 1", A10B, al=LNW)
put(S9, q + 1, 12, f"=SUMPRODUCT(E{R9}:E{R9L},L{R9}:L{R9L})/SUM(E{R9}:E{R9L})", A10B, P1, RIGHT)
S9.auto_filter.ref = f"A{R9 - 3}:U{R9L}"
S9.freeze_panes = S9.cell(R9, 3)
for c_, w in zip(range(1, 22), [7, 34, 11, 12, 11] + [11] * 6 + [12, 22, 10, 11, 11, 11, 11, 10, 9, 13]):
    S9.column_dimensions[CL(c_)].width = w
for j in range(HELP0, HELP0 + 24):
    S9.column_dimensions[CL(j)].hidden = True
put(S9, R9 - 4, HELP0, "службові: ΔEVE за валютами і ΔЧПД (приховано)", A9I)
foot(S9, q + 2, ["Для моделі «Виявлений» частка ядра коштів на вимогу береться для кожного банку окремо (1 − θ банку з п. 2.2)."])

# ------------------------------------------------------------ 13.10 methodology & sources
r = titles(S10, "13.10. Методологія, відповідність стандартам і джерела")
rows = [
    ("Елемент", "Як реалізовано", "Стандарт / джерело"),
    ("Міри ризику", "ΔEVE (економічна вартість капіталу) і ΔЧПД (12 міс., постійний баланс)", "BCBS (2016) IRRBB; Постанова НБУ № 64, п. 246"),
    ("Часові кошики", "19 кошиків BCBS, середини кошиків, DF = (1 + r)^−t на кривій нульового купона НБУ", "BCBS (2016), стандартизована рамка"),
    ("Сценарії", "6 сценаріїв BCBS + 4 сценарії НБУ (±200 б.п. усі строки і до 6 міс.) + власний + історичний", "BCBS (2016); НБУ, п. 240 (ред. № 172 від 29.12.2020)"),
    ("Розмір шоків гривні", "99,9-й процентиль 6-міс. змін кривої НБУ, межі 100 … 400/500/300 б.п., округлення 25 б.п.; місячні дані (≈ максимум вибірки)", "BCBS (2024) рекалібрування шоків"),
    ("Розмір шоків валюти", "USD 200/300/225, EUR 225/350/200 б.п.", "BCBS (2024)"),
    ("Мінімальна межа ставок", "−100 б.п. на коротких строках, +5 б.п. на рік до 0 % на 20 р.", "BCBS (2016); EBA GL/2022/14"),
    ("Кошти на вимогу", "Ядро/неядро; ліміти 90 %/5 р. (роздрібні трансакційні), 50 %/4 р. (оптові); виявлена модель: ядро = 1 − θ, строк = дюрація франшизи", "BCBS (2016); п. 2.1–2.2 цієї роботи; Drechsler, Savov & Schnabl (2017, 2021)"),
    ("Дострокові погашення / вилучення", "CPR кредитів і TDRR строкових депозитів з множниками за сценаріями", "BCBS (2016), стандартизована рамка"),
    ("Агрегація валют", "Втрати сумуються, прибутки з вагою 50 %, нижня межа 0", "BCBS (2016)"),
    ("Тест на викид", "EVE: втрата > 15 % Tier 1 за шести сценаріїв; ЧПД: зниження > 5 % Tier 1 за ±200 б.п.", "BCBS (2016); EBA RTS SOT (2022)"),
    ("ЧПД", "Договірний ГЕП (Σ розрив × ΔR × (1 − t)) або виявлена передача β_h (п. 2.2)", "НБУ, п. 246 і Додаток 5; English, Van den Heuvel & Zakrajšek (2018)"),
    ("Історичне моделювання", "Усі вікна 6/12 міс. змін кривої НБУ з 2018 р., застосовані до поточного балансу", "Практика ICAAP; NBU Resilience Assessment"),
    ("Зворотний стрес", "Сітка шоків ±500 б.п. × відтік ядра 0–50 %", "Drechsler, Savov & Schnabl (2023), Deposit Franchise Runs; Jiang et al. (2024)"),
    ("Процеси Орнштейна–Уленбека", "Облікова ставка, ставки статей і обсяги депозитів — OU з рухомим середнім (облікова, π^e, E[Δs], BLS) і стрибками; Монте-Карло з корельованими шоками і невизначеністю параметрів", "Vasicek (1977); Merton (1976); Barndorff-Nielsen & Shephard (2001); п. 2.3 цієї роботи"),
    ("Сценарії OU для EVE", "Δ облікової за 6 міс. на рівні 99,9 % / 0,1 % (зі стрибками і без) переноситься на криву за Васічеком: ΔR(τ) = Δi·(1 − e^(−κτ))/(κτ)", "Vasicek (1977); BCBS (2016) — внутрішні сценарії"),
    ("Стабільна частка коштів на вимогу (OU)", "1-й процентиль 12-міс. мінімуму обсягу (OU-стрибки) × (1 − β довгострокової передачі) = ядро; для банку — власна волатильність і стрибки", "EBA GL/2022/14 (поведінкові моделі NMD); практика ALM"),
    ("Обмеження", "Капітал — балансовий (не регулятивний Tier 1); ставки статей — середні по системі; без опціонів (кепи, флори), без комерційної маржі; валютна крива — USD", "—"),
]
for k, row_ in enumerate(rows):
    for j, v in enumerate(row_):
        put(S10, r + k, 1 + j, v, A10B if k == 0 else A10, al=LEFT, border=BOX, fill=FILL_HEAD if k == 0 else None)
    S10.row_dimensions[r + k].height = 30 if k else 18
S10.column_dimensions["A"].width = 26; S10.column_dimensions["B"].width = 80; S10.column_dimensions["C"].width = 55
r += len(rows) + 1
for t in ["Basel Committee on Banking Supervision (2016). Interest rate risk in the banking book. https://www.bis.org/bcbs/publ/d368.htm",
          "Basel Committee on Banking Supervision (2024). Recalibration of shocks for interest rate risk in the banking book (липень 2024; набирає чинності 01.01.2026).",
          "European Banking Authority (2022). Guidelines on IRRBB and CSRBB (EBA/GL/2022/14); RTS on supervisory outlier tests.",
          "Національний банк України. Постанова Правління № 64 від 11.06.2018 «Про затвердження Положення про організацію системи управління ризиками…», п. 240, 246 (ред. Постанови № 172 від 29.12.2020).",
          "Національний банк України (2025). Методологія оцінки стійкості банків (стрес-тестування), модель 2025.",
          "Drechsler, I., Savov, A., Schnabl, P. (2021). Banking on Deposits: Maturity Transformation without Interest Rate Risk. Journal of Finance. doi:10.1111/jofi.13013",
          "Drechsler, I., Savov, A., Schnabl, P. (2023). Deposit Franchise Runs. doi:10.1111/jofi.70034",
          "English, W., Van den Heuvel, S., Zakrajšek, E. (2018). Interest rate risk and bank equity valuations. Journal of Monetary Economics."]:
    put(S10, r, 1, t, A9, al=LNW); r += 1

# ------------------------------------------------------------ contents 13
S0.column_dimensions["A"].width = 110
r = 2
put(S0, r, 1, "13. Стрес-тестування процентного ризику банківської книги (IRRBB) за стандартами BCBS, практикою НБУ і результатами дослідження", A10B); r += 2
TOC = [("13.1", "13.1. Панель: перемикачі, результати за 12 сценаріями, ключові показники, графіки"),
       ("13.5", "13.5. Банк: драбина переоцінки, PV01 за кошиками, внесок статей у ΔEVE"),
       ("13.6", "13.6. ЧПД: договірний ГЕП проти виявленої передачі ставки"),
       ("13.7", "13.7. Історичне моделювання на кривій НБУ"),
       ("13.8", "13.8. Зворотний стрес-тест і карта «шок × відтік депозитної франшизи»"),
       ("13.9", "13.9. Стрес-тест усіх банків"),
       ("13.2", "13.2. Сценарії: шоки, базові криві, ΔR і коефіцієнти дисконтування"),
       ("13.3", "13.3. Припущення: статті, кошти на вимогу, CPR/TDRR"),
       ("13.4", "13.4. Грошові потоки на 1 грн і коефіцієнти чутливості (розрахунок)"),
       ("13.11", "13.11. Стохастичний ЧПД: Монте-Карло процесів Орнштейна–Уленбека (п. 2.3)"),
       ("13.10", "13.10. Методологія, відповідність стандартам і джерела"),
       ("Д1", f"Д1. Дані банків на {latest_txt}"), ("Д2", "Д2. Криві нульового купона НБУ"), ("Д3", "Д3. Траєкторії Монте-Карло OU")]
for sh, txt in TOC:
    c = put(S0, r, 1, txt, A10L, al=LNW); c.hyperlink = f"#'{sh}'!A1"; r += 1
r += 1
put(S0, r, 1, "Як користуватися", A10B); r += 1
for t in ["1. На аркуші 13.1 оберіть банк і припущення. Таблиця результатів, ключові показники, графіки й аркуші 13.5–13.9 перераховуються.",
          "2. Порівняйте моделі коштів на вимогу: «Договірний (усе O/N)» — найконсервативніший; «BCBS» — регуляторні ліміти; «Виявлений» — поведінка з п. 2.1–2.2.",
          "3. Метод ЧПД «Виявлена передача β» показує, як змінюється дохід, якщо ставки статей реагують так, як оцінено в п. 2.2, а не за договірними строками.",
          "4. Блакитні клітинки на 13.2–13.3 (розміри шоків, власний сценарій, ставки статей, CPR/TDRR, ліміти) можна змінювати для аналізу чутливості.",
          "5. Кольори: блакитний шрифт — вхідні дані; зелений — посилання; червоне тло — порушення тесту на викид."]:
    put(S0, r, 1, t, al=LEFT); S0.row_dimensions[r].height = 27; r += 1
r += 1
put(S0, r, 1, f"Згенеровано {dt.date.today().strftime('%d.%m.%Y')} скриптом 13_irrbb_stress_test.py з результатів 10_section_2_3_repricing_gap_v6.R. «–» — явища не було; «…» — даних немає.", A9I)

# ------------------------------------------------------------ Д3 + 13.11 stochastic NII (OU Monte Carlo)
put(D3, 1, 1, "Д3. Траєкторії Монте-Карло OU: середня за 12 міс. зміна ставки кожної статті (частки), гривня; і віяло облікової (не редагувати)", A10B)
put(D3, 3, 1, "Траєкторія", al=CEN, border=BOX)
for k, i in enumerate(IT): put(D3, 3, 2 + k, i, A9, al=CEN, border=BOX)
NX = len(OUX)
for p_, row_ in enumerate(OUX):
    put(D3, 4 + p_, 1, p_ + 1, A9, N0)
    for k, i in enumerate(IT):
        put(D3, 4 + p_, 2 + k, num(row_.get(i, 0)), A9, N4)
X3L = 3 + NX
FC0 = 2 + NI + 2
fan_cols = ["h", "q1", "q5", "q25", "q50", "q75", "q95", "q99"]
for j, c_ in enumerate(fan_cols): put(D3, 3, FC0 + j, c_, A9, al=CEN, border=BOX)
for p_, row_ in enumerate(OUFAN):
    for j, c_ in enumerate(fan_cols): put(D3, 4 + p_, FC0 + j, num(row_.get(c_)), A9, N2)
FANL = 3 + len(OUFAN)
D3.freeze_panes = "B4"
r = titles(S11, "13.11. Стохастичний ЧПД: Монте-Карло процесів Орнштейна–Уленбека з очікуваннями і стрибками (п. 2.3)",
           f"(млн грн за 12 міс., постійний баланс; {NX} траєкторій облікової ставки, очікувань і ставок статей; оцінка — 14_ou_deposit_dynamics_v6.R)")
put(S11, 4, 1, '="Банк: "&bank', A10I)
put(S11, r, 1, "Знак × залишок у гривні, млн грн", A9I)
for k, i in enumerate(IT):
    put(S11, r, 4 + k, ILAB[i], A9, al=CEN, border=BOX)
    put(S11, r + 1, 4 + k, f"='13.3'!$C${A0 + k}*INDEX({d1col('U_' + i)},row)", A9, N1, RIGHT, BOX)
S11.row_dimensions[r].height = 54
WROW11 = f"$D${r + 1}:${CL(3 + NI)}${r + 1}"
r += 3
put(S11, r, 1, "Розподіл ΔЧПД", A10B); r += 1
ST0 = r
PR0 = ST0 + 14 + 34                                   # first path row (after stats and histogram)
pr = f"$B${PR0}:$B${PR0 + NX - 1}"
STAT = [("Середнє, млн грн", f"=AVERAGE({pr})", N1), ("Медіана", f"=MEDIAN({pr})", N1), ("5-й процентиль", f"=PERCENTILE({pr},0.05)", N1),
        ("1-й процентиль", f"=PERCENTILE({pr},0.01)", N1), ("0,1-й процентиль", f"=PERCENTILE({pr},0.001)", N1),
        ("Очікуваний дефіцит ES 1 % (середнє гірших 1 %)", f'=AVERAGEIF({pr},"<="&B{ST0 + 3})', N1),
        ("ЧПД під ризиком (1 %), % Tier 1", f"=-MIN(0,B{ST0 + 3})/T1", P1), ("ЧПД під ризиком (1 %), % ЧПД", f'=IFERROR(-MIN(0,B{ST0 + 3})/NII12,"…")', P1),
        ("Тест на викид ЧПД (1 % < −поріг)", f'=IF(-B{ST0 + 3}/T1>sot_nii,"порушено","ні")', None), ("Траєкторій", f"=COUNT({pr})", N0)]
for k, (lab_, f_, fm) in enumerate(STAT):
    put(S11, ST0 + k, 1, lab_, al=LEFT, border=BOX); put(S11, ST0 + k, 2, f_, A10B, fm, RIGHT, BOX)
NQ1, NQ2 = ST0 + 6, ST0 + 7
S11.conditional_formatting.add(f"B{ST0 + 8}", CellIsRule(operator="equal", formula=['"порушено"'], fill=RED))
r = ST0 + len(STAT) + 2
put(S11, r, 1, "Гістограма ΔЧПД (30 інтервалів)", A10B); r += 1
put(S11, r, 1, "Від", A9, al=CEN, border=BOX); put(S11, r, 2, "До", A9, al=CEN, border=BOX); put(S11, r, 3, "Траєкторій", A9, al=CEN, border=BOX); put(S11, r, 4, "Середина, млн грн", A9, al=CEN, border=BOX)
HB0 = r + 1
for k in range(30):
    q = HB0 + k
    put(S11, q, 1, f"=MIN({pr})+(MAX({pr})-MIN({pr}))*{k}/30", A9, N0); put(S11, q, 2, f"=MIN({pr})+(MAX({pr})-MIN({pr}))*{k + 1}/30", A9, N0)
    put(S11, q, 3, f'=COUNTIFS({pr},">="&A{q},{pr},"<"&B{q})' + (f'+COUNTIF({pr},MAX({pr}))' if k == 29 else ""), A9, N0)
    put(S11, q, 4, f"=ROUND((A{q}+B{q})/2,0)", A9, N0)
put(S11, PR0 - 2, 1, "Траєкторії: ΔЧПД банку = Σ знак × залишок × зміна ставки статті (Д3)", A10B)
put(S11, PR0 - 1, 1, "Траєкторія", A9, al=CEN, border=BOX); put(S11, PR0 - 1, 2, "ΔЧПД, млн грн", A9, al=CEN, border=BOX)
for p_ in range(NX):
    q = PR0 + p_
    put(S11, q, 1, p_ + 1, A9, N0); put(S11, q, 2, f"=SUMPRODUCT({WROW11},'Д3'!$B${4 + p_}:${CL(1 + NI)}${4 + p_})", A9, N1, RIGHT)
S11.column_dimensions["A"].width = 46; S11.column_dimensions["B"].width = 16; S11.column_dimensions["C"].width = 11; S11.column_dimensions["D"].width = 13
for k in range(NI): S11.column_dimensions[CL(4 + k)].width = max(S11.column_dimensions[CL(4 + k)].width or 0, 12)
if NX:
    hch = BarChart(); hch.type = "col"; hch.title = "Розподіл ΔЧПД за 12 міс., млн грн"; hch.gapWidth = 10
    hch.add_data(Reference(S11, min_col=3, min_row=HB0, max_row=HB0 + 29), titles_from_data=False)
    hch.set_categories(Reference(S11, min_col=4, min_row=HB0, max_row=HB0 + 29))
    hch.series[0].graphicalProperties.solidFill = "1F4E79"; hch.legend = None; hch.x_axis.tickLblPos = "low"; hch.x_axis.tickLblSkip = 5
    hch.height = 8; hch.width = 17; axis_font(hch); S11.add_chart(hch, "F6")
if OUFAN:
    fch = LineChart(); fch.title = "Облікова ставка: віяло Монте-Карло OU, %"
    for c_, nmx, colr, dash in (("q1", "1 %", "A6A6A6", "dash"), ("q5", "5 %", "7F7F7F", "sysDot"), ("q50", "медіана", "1F4E79", "solid"), ("q95", "95 %", "7F7F7F", "sysDot"), ("q99", "99 %", "A6A6A6", "dash")):
        j = fan_cols.index(c_); se = Series(Reference(D3, min_col=FC0 + j, min_row=4, max_row=FANL), title=nmx)
        se.graphicalProperties.line.solidFill = colr; se.graphicalProperties.line.dashStyle = dash; se.graphicalProperties.line.width = 19000; se.smooth = False; fch.series.append(se)
    fch.set_categories(Reference(D3, min_col=FC0, min_row=4, max_row=FANL)); fch.x_axis.title = "місяців уперед"; fch.x_axis.tickLblSkip = 6
    fch.height = 8; fch.width = 17; axis_font(fch); S11.add_chart(fch, "F23")

# ------------------------------------------------------------ charts
ch = BarChart(); ch.type = "bar"; ch.grouping = "clustered"
ch.title = "Втрата EVE за сценаріями, % Tier 1"
ch.add_data(Reference(S1, min_col=5, min_row=RS0, max_row=RS1), titles_from_data=False)
ch.set_categories(Reference(S1, min_col=1, min_row=RS0, max_row=RS1))
ch.series[0].tx = SeriesLabel(v="втрата EVE"); ch.series[0].graphicalProperties.solidFill = "1F4E79"
ch.x_axis.scaling.orientation = "maxMin"; ch.x_axis.tickLblPos = "low"; ch.x_axis.tickLblSkip = 1; ch.y_axis.number_format = "0%"
ch.legend = None; ch.height = 10; ch.width = 18; axis_font(ch)
S1.add_chart(ch, "N5")
gc = BarChart(); gc.type = "col"; gc.grouping = "clustered"; gc.title = "Розрив переоцінки за кошиками, млн грн"
for cc, nmx in (("uah", "гривня"), ("fx", "валюта")):
    s = Series(Reference(S5, min_col=3, max_col=2 + NK, min_row=LAD[(cc, "net")]), title=nmx); gc.series.append(s)
gc.set_categories(Reference(S5, min_col=3, max_col=2 + NK, min_row=LAD["uah"] - 2))
gc.series[0].graphicalProperties.solidFill = "1F4E79"; gc.series[1].graphicalProperties.solidFill = "A6A6A6"
gc.x_axis.tickLblPos = "low"; gc.height = 9; gc.width = 18; axis_font(gc)
S1.add_chart(gc, "N26")
# curve chart: base and detailed scenario (UAH) — helper on 13.2 right side
hc0 = 26
put(S2, BT0 - 3, hc0, "Для графіка: крива гривні", A9I)
put(S2, BT0 - 2, hc0, "t, р.", A9); put(S2, BT0 - 2, hc0 + 1, "База", A9); put(S2, BT0 - 2, hc0 + 2, "Після шоку (сценарій деталізації)", A9)
for kk in range(NK):
    q = BT0 + kk
    put(S2, q, hc0, f"=D{q}", A9, N2); put(S2, q, hc0 + 1, f"=G{q}", A9, P2)
    put(S2, q, hc0 + 2, f"=MAX(G{q}+INDEX($B${DR['uah'] + kk}:${CL(1 + NS)}${DR['uah'] + kk},det_n),MIN(G{q},I{q}))", A9, P2)
lc = LineChart(); lc.title = "Крива гривні: база і сценарій деталізації"
lc.series.append(Series(Reference(S2, min_col=hc0 + 1, min_row=BT0, max_row=BT0 + NK - 2), title="база"))
lc.series.append(Series(Reference(S2, min_col=hc0 + 2, min_row=BT0, max_row=BT0 + NK - 2), title="після шоку"))
lc.set_categories(Reference(S2, min_col=1, min_row=BT0, max_row=BT0 + NK - 2))
lc.series[0].graphicalProperties.line.solidFill = "A6A6A6"; lc.series[1].graphicalProperties.line.solidFill = "1F4E79"
for s_ in lc.series: s_.graphicalProperties.line.width = 22000; s_.smooth = False
lc.y_axis.number_format = "0%"; lc.x_axis.tickLblPos = "low"; lc.height = 9; lc.width = 18; axis_font(lc)
S1.add_chart(lc, "N46")
hcch = BarChart(); hcch.type = "col"; hcch.title = "Історичне моделювання: втрата EVE, % Tier 1"
hcch.add_data(Reference(S7, min_col=8, min_row=HS0, max_row=HS1), titles_from_data=False)
hcch.set_categories(Reference(S7, min_col=1, min_row=HS0, max_row=HS1))
hcch.series[0].graphicalProperties.solidFill = "1F4E79"; hcch.legend = None
hcch.y_axis.number_format = "0%"; hcch.x_axis.number_format = "mm.yy"; hcch.x_axis.tickLblPos = "low"; hcch.x_axis.tickLblSkip = 6
hcch.height = 8; hcch.width = 20; axis_font(hcch)
S7.add_chart(hcch, "J5")

# fill KPI placeholders now that rows are known
for k, (labk, f, fmt) in enumerate(KP):
    c = S1.cell(KROW[k], 2)
    if isinstance(c.value, str):
        c.value = c.value.replace("{REV}", str(REVR)).replace("{HQ}", str(KP_HQ)).replace("{HMX}", str(KP_HMX)).replace("{NQ1}", str(NQ1)).replace("{NQ2}", str(NQ2))

# ------------------------------------------------------------ substitute symbolic names → absolute references
pat = re.compile(r"(?<![A-Za-z_'!$])(" + "|".join(sorted(map(re.escape, NAMES), key=len, reverse=True)) + r")(?![A-Za-z0-9_(])")
for ws in wb.worksheets:
    for row in ws.iter_rows():
        for c in row:
            if isinstance(c.value, str) and c.value.startswith("="):
                c.value = pat.sub(lambda m: NAMES[m.group(1)], c.value)
for ws in wb.worksheets:
    for dv in ws.data_validations.dataValidation:
        if dv.formula1 and dv.formula1.startswith("="):
            dv.formula1 = dv.formula1[1:]
for ws in wb.worksheets:
    ws.page_setup.orientation = "landscape"; ws.page_setup.fitToWidth = 1; ws.page_setup.fitToHeight = 0
    ws.sheet_properties.pageSetUpPr.fitToPage = True
wb.active = wb.sheetnames.index("13.1")
wb.calculation.fullCalcOnLoad = True; wb.calculation.calcMode = "auto"
wb.save(FILE)
print("OK:", FILE, f"банків: {NB}, кошиків: {NK}, сценаріїв: {NS}, місяців кривої: {len(MONTHS)}")
