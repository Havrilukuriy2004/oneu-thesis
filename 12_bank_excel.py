"""
12_bank_excel.py — динамічний Excel-інструмент «Банк: розрив переоцінки (2.3) і тіньова вартість фондування (2.4)»
Стиль — як у таблицях НБУ (4-Financial_markets.xlsx): заголовки розділів жирним, шапка в тонких рамках з номерами граф,
без сітки, формат чисел #,##0.0;–#,##0.0;"–", закріплена шапка; шрифт Arial 10.

Читає CSV з output_nbu/section_2_3/tables і output_nbu/section_2_4/tables (результати 10_ і 11_) і пише
output_nbu/bank_excel/Bank_Repricing_Funding_Tool.xlsx. Усі показники рахуються формулами Excel від перемикачів.

Запуск:  python3 12_bank_excel.py [OUTPUT_DIR]          (або через 12_bank_excel_v6.R / run_all_v6.R)
Потрібно: pip3 install openpyxl
"""
import csv, os, sys, datetime as dt
from collections import defaultdict
from openpyxl import Workbook
from openpyxl.styles import Font, Alignment, Border, Side, PatternFill
from openpyxl.utils import get_column_letter as CL
from openpyxl.worksheet.datavalidation import DataValidation
from openpyxl.workbook.defined_name import DefinedName
from openpyxl.comments import Comment
from openpyxl.chart import BarChart, LineChart, Reference, Series
from openpyxl.chart.text import RichText
from openpyxl.drawing.text import Paragraph, ParagraphProperties, CharacterProperties, Font as DFont

OUT_ROOT = sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "output_nbu")
T23 = os.path.join(OUT_ROOT, "section_2_3", "tables")
T24 = os.path.join(OUT_ROOT, "section_2_4", "tables")
DEST = os.path.join(OUT_ROOT, "bank_excel"); os.makedirs(DEST, exist_ok=True)
FILE = os.path.join(DEST, "Bank_Repricing_Funding_Tool.xlsx")


def rd(path):
    with open(path, encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def num(x, default=0.0):
    try:
        v = float(x)
        return v if v == v else default
    except (TypeError, ValueError):
        return default


def clean_name(s):
    from bank_names import clean_bank_name
    return clean_bank_name(s)


# ------------------------------------------------------------------ data
items = rd(os.path.join(T23, "S23_items_parameters.csv"))
betas = rd(os.path.join(T23, "S23_beta_paths.csv"))
totals = rd(os.path.join(T23, "S23_bank_totals_latest.csv"))
bitems = rd(os.path.join(T23, "S23_bank_items_latest.csv"))
hist23 = rd(os.path.join(T23, "S23_bank_history_60m.csv"))
prods = rd(os.path.join(T24, "S24_bank_products_latest.csv"))
macro = rd(os.path.join(T24, "S24_macro_latest.csv"))[0]
par24 = {r["param"]: num(r["value"]) for r in rd(os.path.join(T24, "S24_parameters.csv"))}
rr = rd(os.path.join(T24, "S24_rr_schedule.csv"))
hist24 = rd(os.path.join(T24, "S24_bank_history_60m.csv"))

OWN = {"State": "Державний", "Foreign": "Іноземний", "Private": "Приватний"}
LATEST = totals[0]["month"] if totals else macro["month"]
latest_txt = dt.date.fromisoformat(LATEST[:10]).strftime("%m.%Y")

ITEM_CODES = [r["item"] for r in items]
NI = len(ITEM_CODES)
PRODS = [("hh_demand", "uah"), ("hh_demand", "fx"), ("hh_term", "uah"), ("hh_term", "fx"),
         ("corp_demand", "uah"), ("corp_demand", "fx"), ("corp_term", "uah"), ("corp_term", "fx")]
NP = len(PRODS)
PROD_UA = {"hh_demand": "Кошти ФО на вимогу", "hh_term": "Строкові депозити ФО", "corp_demand": "Кошти СГ на вимогу", "corp_term": "Строкові депозити СГ"}
CCY_UA = {"uah": "гривня", "fx": "валюта"}
KEY_UA = {"y_cd": "Депсертифікати / міжбанк (дохідність)", "y_le": "Кредити СГ (дохідність)", "y_hh": "Кредити ФО (дохідність)",
          "y_refin": "ЦП, що рефінансуються (дохідність)", "hh_demand": "Кошти ФО на вимогу (ставка)", "hh_term": "Строкові депозити ФО (ставка)",
          "corp_demand": "Кошти СГ на вимогу (ставка)", "corp_term": "Строкові депозити СГ (ставка)", "c_ibl": "Інші запозичення (вартість)"}
VAR_UA = {"b": "симетрична", "b_up": "підвищення", "b_dn": "зниження"}

# banks sorted by assets
banks = sorted(totals, key=lambda r: -num(r["TA"]))
BI = {r["nkb"]: r for r in banks}
stock = defaultdict(float)
for r in bitems:
    stock[(r["nkb"], r["item"], "U")] += num(r["uah"]); stock[(r["nkb"], r["item"], "F")] += num(r["fx"])
pst, prt = defaultdict(float), {}
for r in prods:
    pst[(r["nkb"], r["prod"], r["ccy"])] += num(r["stock"]); prt[(r["nkb"], r["prod"], r["ccy"])] = num(r["r"])

# β paths
BETA = defaultdict(dict)
for r in betas:
    for v in ("b", "b_up", "b_dn"):
        BETA[(r["beta_key"], v)][int(float(r["h"]))] = num(r[v])
KEYS = sorted({k for k, _ in BETA}, key=lambda k: list(KEY_UA).index(k) if k in KEY_UA else 99)
HMAX = max(max(d) for d in BETA.values()) if BETA else 24

# reserve schedule long → wide
RR_COLS = [("hh_demand", "uah"), ("hh_demand", "fx"), ("hh_term_short", "uah"), ("hh_term_short", "fx"), ("hh_term_long", "uah"), ("hh_term_long", "fx"),
           ("corp_demand", "uah"), ("corp_demand", "fx"), ("corp_term", "uah"), ("corp_term", "fx")]
RR_UA = {"hh_demand": "ФО: поточні / на вимогу", "hh_term_short": "ФО: строкові ≤ 92 дні", "hh_term_long": "ФО: строкові ≥ 93 дні",
         "corp_demand": "СГ: на вимогу", "corp_term": "СГ: строкові"}
dates = sorted({r["from"][:10] for r in rr})
by = defaultdict(list)
for r in rr:
    by[(r["group"], r["ccy"])].append((r["from"][:10], num(r["ratio"]), num(r["cov_max"]), r.get("note", "")))
notes_by_date = defaultdict(str)
for r in rr:
    if r.get("note"):
        notes_by_date[r["from"][:10]] = r["note"]
RRW = []
for d in dates:
    row = []
    cov = 0.0
    for g, c in RR_COLS:
        z = [x for x in sorted(by[(g, c)]) if x[0] <= d]
        row.append(z[-1][1] if z else 0.0)
        if z:
            cov = max(cov, z[-1][2])
    RRW.append((d, row, cov, notes_by_date.get(d, "")))

# history
H = {}
for r in hist23:
    H[(r["nkb"], r["month"][:10])] = {"RG12": num(r["RG12"], None), "CG12": num(r["CG12"], None), "nim": num(r["nim"], None), "cod4": num(r["cod4"], None), "policy": num(r["policy"], None)}
for r in hist24:
    H.setdefault((r["nkb"], r["month"][:10]), {}).update({"cod_nom": num(r["cod_nom"], None), "ec": num(r.get("ec", r.get("sfc")), None)})
MONTHS = sorted({m for _, m in H})[-60:]

# ------------------------------------------------------------------ styles
A10 = Font(name="Arial", size=10)
A10B = Font(name="Arial", size=10, bold=True)
A10I = Font(name="Arial", size=10, italic=True)
A9 = Font(name="Arial", size=9)
A9I = Font(name="Arial", size=9, italic=True)
A10L = Font(name="Arial", size=10, underline="single", color="0000FF")
INP = Font(name="Arial", size=10, color="0000FF")
LINKF = Font(name="Arial", size=10, color="008000")
FILL_IN = PatternFill("solid", fgColor="FFF2CC")
FILL_KEY = PatternFill("solid", fgColor="F2F2F2")
TH = Side(style="thin", color="000000")
BOX = Border(left=TH, right=TH, top=TH, bottom=TH)
TOPB = Border(top=TH)
CEN = Alignment(horizontal="center", vertical="center", wrap_text=True)
LEFT = Alignment(horizontal="left", vertical="center", wrap_text=True)
LEFT_NW = Alignment(horizontal="left", vertical="center", wrap_text=False)
RIGHT = Alignment(horizontal="right", vertical="center")
NUM0 = '#,##0;\\–#,##0;"–"'
NUM1 = '#,##0.0;\\–#,##0.0;"–"'
NUM2 = '#,##0.00;\\–#,##0.00;"–"'
NUM3 = '#,##0.000;\\–#,##0.000;"–"'
PCT1 = '0.0%;\\–0.0%;"–"'
PCT2 = '0.00%;\\–0.00%;"–"'
DATEF = "dd.mm.yyyy"
MONF = "mm.yyyy"

wb = Workbook()
wb.remove(wb.active)


def sheet(name, tab=None):
    ws = wb.create_sheet(name)
    ws.sheet_view.showGridLines = False
    ws.sheet_properties.outlinePr.summaryBelow = False
    if tab:
        ws.sheet_properties.tabColor = tab
    return ws


def put(ws, r, c, v, font=A10, fmt=None, al=None, border=None, fill=None):
    cell = ws.cell(r, c, v)
    cell.font = font
    if fmt: cell.number_format = fmt
    if al: cell.alignment = al
    if border: cell.border = border
    if fill: cell.fill = fill
    return cell


def titles(ws, lines, note=None, start=1):
    r = start
    for t in lines:
        put(ws, r, 1, t, A10B, al=LEFT_NW); r += 1
    if note:
        put(ws, r, 1, note, A10, al=LEFT_NW); r += 1
    return r


def header(ws, r, c0, labels, top=None, numbers=True, height=None):
    """NBU-style header: optional merged top row, label row, then numbers row (9 pt) and a 6-pt spacer."""
    if top:
        for (c_from, c_to, txt) in top:
            for c in range(c0 + c_from, c0 + c_to + 1):
                put(ws, r, c, None, border=BOX)
            put(ws, r, c0 + c_from, txt, al=CEN, border=BOX)
            if c_to > c_from:
                ws.merge_cells(start_row=r, start_column=c0 + c_from, end_row=r, end_column=c0 + c_to)
        covered = {c for (a, b, _) in top for c in range(a, b + 1)}
        for j, lab in enumerate(labels):
            if j not in covered:
                put(ws, r, c0 + j, lab, al=CEN, border=BOX)
                put(ws, r + 1, c0 + j, None, border=BOX)
                ws.merge_cells(start_row=r, start_column=c0 + j, end_row=r + 1, end_column=c0 + j)
            else:
                put(ws, r + 1, c0 + j, lab, al=CEN, border=BOX)
        r += 2
    else:
        for j, lab in enumerate(labels):
            put(ws, r, c0 + j, lab, al=CEN, border=BOX)
        r += 1
    if height:
        ws.row_dimensions[r - 1].height = height
    if numbers:
        for j in range(len(labels)):
            put(ws, r, c0 + j, j + 1, A9, al=CEN, border=BOX)
        r += 1
    ws.row_dimensions[r].height = 6
    return r + 1


def axis_font(ch, size=900):
    ch.title_overlay = False
    rt = RichText(p=[Paragraph(pPr=ParagraphProperties(defRPr=CharacterProperties(latin=DFont(typeface="Arial"), sz=size)), endParaRPr=CharacterProperties())])
    for ax in (ch.x_axis, ch.y_axis):
        ax.txPr = rt
        ax.delete = False
    if ch.legend is not None:
        ch.legend.txPr = rt
        ch.legend.position = "b"


NAMES = {}


def name(nm, ref):
    """Symbolic names are substituted by absolute references before saving (no workbook-defined names:
    they break in Apple Numbers and some Excel builds)."""
    NAMES[nm] = ref


def foot(ws, r, extra=()):
    r += 1
    put(ws, r, 1, "«–» — явища не було; «…» — даних немає або показник не визначений.", A9I); r += 1
    for t in extra:
        put(ws, r, 1, t, A9I, al=LEFT_NW); r += 1
    put(ws, r, 1, "Джерело: розрахунки автора за даними НБУ (скрипти 10_section_2_3_repricing_gap_v6.R, 11_section_2_4_shadow_cost_v6.R, 12_bank_excel.py).", A9I)
    return r + 1


# =================================================================== sheets
S0 = sheet("12")
S1 = sheet("12.1", "1F4E79")
S2 = sheet("12.2")
S3 = sheet("12.3")
S4 = sheet("12.4")
S5 = sheet("12.5")
D1 = sheet("Д1", "808080")
D2 = sheet("Д2", "808080")

# ------------------------------------------------------------------ Д1 bank data
titles(D1, ["Д1. Дані банків на " + latest_txt + " (вхід для формул; не редагувати)"],
       "(залишки — млн грн; ставки — частки; дюрації — роки)")
D1_COLS = ["nkb", "bank", "owner", "TA", "EQ", "NII12", "D_hhdem_b", "D_corpdem_b"] + [f"U_{i}" for i in ITEM_CODES] + [f"F_{i}" for i in ITEM_CODES] \
          + [f"S_{p}_{c}" for p, c in PRODS] + [f"R_{p}_{c}" for p, c in PRODS]
D1_LAB = ["Код банку", "Банк", "Власність", "Активи", "Капітал", "ЧПД за 12 міс.", "Дюрація коштів ФО на вимогу (поведінкова), р.", "Дюрація коштів СГ на вимогу (поведінкова), р."] \
         + [f"{r['label']}: гривня" for r in items] + [f"{r['label']}: валюта" for r in items] \
         + [f"{PROD_UA[p]}, {CCY_UA[c]}: залишок" for p, c in PRODS] + [f"{PROD_UA[p]}, {CCY_UA[c]}: ставка" for p, c in PRODS]
for j, (k, lab) in enumerate(zip(D1_COLS, D1_LAB)):
    put(D1, 3, j + 1, k, A9, al=CEN, border=BOX)
    put(D1, 4, j + 1, lab, al=CEN, border=BOX)
D1.row_dimensions[4].height = 66
COL = {k: j + 1 for j, k in enumerate(D1_COLS)}
R1 = 5
for i, b in enumerate(banks):
    r = R1 + i
    vals = [b["nkb"], clean_name(b["bank"]), OWN.get(b["owner"], b["owner"] or "…"), num(b["TA"]), num(b["EQ"]), num(b["NII12"]), num(b["D_hhdem_b"]), num(b["D_corpdem_b"])]
    vals += [stock[(b["nkb"], it, "U")] for it in ITEM_CODES] + [stock[(b["nkb"], it, "F")] for it in ITEM_CODES]
    vals += [pst[(b["nkb"], p, c)] for p, c in PRODS] + [prt.get((b["nkb"], p, c), 0.0) for p, c in PRODS]
    for j, v in enumerate(vals):
        cell = put(D1, r, j + 1, v)
        k = D1_COLS[j]
        if k.startswith("R_"): cell.number_format = PCT2
        elif k.startswith("D_"): cell.number_format = NUM2
        elif j >= 3: cell.number_format = NUM1
RL = R1 + len(banks) - 1
D1.freeze_panes = "C5"
D1.column_dimensions["A"].width = 9; D1.column_dimensions["B"].width = 34; D1.column_dimensions["C"].width = 12
for j in range(4, len(D1_COLS) + 1):
    D1.column_dimensions[CL(j)].width = 12


def d1rng(k, absolute=True):
    c = CL(COL[k])
    return f"'Д1'!${c}${R1}:${c}${RL}"


def d1row(k, r):
    return f"'Д1'!${CL(COL[k])}{r}"


U0, U1 = CL(COL[f"U_{ITEM_CODES[0]}"]), CL(COL[f"U_{ITEM_CODES[-1]}"])
F0, F1 = CL(COL[f"F_{ITEM_CODES[0]}"]), CL(COL[f"F_{ITEM_CODES[-1]}"])
S0c, S1c = CL(COL[f"S_{PRODS[0][0]}_{PRODS[0][1]}"]), CL(COL[f"S_{PRODS[-1][0]}_{PRODS[-1][1]}"])
Rr0, Rr1 = CL(COL[f"R_{PRODS[0][0]}_{PRODS[0][1]}"]), CL(COL[f"R_{PRODS[-1][0]}_{PRODS[-1][1]}"])

# ------------------------------------------------------------------ Д2 history
titles(D2, ["Д2. Історія показників банків за 60 міс. (вхід для 12.5; не редагувати)"], "(%, в.п.)")
H_COLS = ["key", "nkb", "month", "RG12", "CG12", "nim", "cod4", "policy", "cod_nom", "ec"]
H_LAB = ["Ключ", "Код банку", "Місяць", "Виявлений розрив 12 міс., % активів", "Договірний розрив 12 міс., % активів", "ЧПМ, %", "Вартість депозитів (2.1), %",
         "Облікова ставка, %", "Номінальна вартість депозитів (2.4), %", "Економічна вартість депозитів, %"]
for j, (k, lab) in enumerate(zip(H_COLS, H_LAB)):
    put(D2, 3, j + 1, k, A9, al=CEN, border=BOX); put(D2, 4, j + 1, lab, al=CEN, border=BOX)
D2.row_dimensions[4].height = 54
r = 5
bank_set = {b["nkb"] for b in banks}
for (nkb, m), v in sorted(H.items()):
    if nkb not in bank_set or m not in MONTHS:
        continue
    put(D2, r, 1, f"{nkb}|{(dt.date.fromisoformat(m) - dt.date(1899, 12, 30)).days}"); put(D2, r, 2, nkb); put(D2, r, 3, dt.date.fromisoformat(m), fmt=MONF)
    for j, k in enumerate(H_COLS[3:]):
        x = v.get(k)
        if x is not None:
            put(D2, r, 4 + j, x, fmt=NUM2)
    r += 1
H_LAST = r - 1
D2.freeze_panes = "A5"
for j, w in enumerate([16, 9, 9] + [14] * 7):
    D2.column_dimensions[CL(j + 1)].width = w

# ------------------------------------------------------------------ 12.3 reserve requirements (editable)
r = titles(S3, ["12. Інструмент аналізу банку", "12.3. Нормативи обов'язкового резервування НБУ (редагована таблиця)"],
           "(частка бази резервування; блакитні клітинки можна змінювати і доповнювати новими рішеннями НБУ — рядки в порядку зростання дат)")
labs = ["Діє з"] + [("гривня" if c == "uah" else "валюта") for _, c in RR_COLS] + ["Покриття бенчмарк-ОВДП, частка нормативу", "Рішення / примітка"]
top = [(1 + 2 * k, 2 + 2 * k, RR_UA[RR_COLS[2 * k][0]]) for k in range(5)]
r = header(S3, r + 1, 1, labs, top=top)
RR_R0 = r
for d, row, cov, note in RRW:
    put(S3, r, 1, dt.date.fromisoformat(d), INP, DATEF, RIGHT, fill=FILL_IN)
    for j, v in enumerate(row):
        put(S3, r, 2 + j, v, INP, PCT1, RIGHT, fill=FILL_IN)
    put(S3, r, 12, cov, INP, PCT1, RIGHT, fill=FILL_IN)
    put(S3, r, 13, note, A9, al=LEFT_NW)
    r += 1
for _ in range(8):                                     # spare rows for future NBU decisions
    for c in range(1, 13):
        put(S3, r, c, None, INP, PCT1 if c > 1 else DATEF, RIGHT, fill=FILL_IN)
    r += 1
RR_R1 = r - 1
S3.freeze_panes = S3.cell(RR_R0, 2)
S3.column_dimensions["A"].width = 12
for c in range(2, 13):
    S3.column_dimensions[CL(c)].width = 11
S3.column_dimensions["M"].width = 60
foot(S3, r, ["Резерви формуються в гривні на кореспондентському рахунку в НБУ для бази в усіх валютах. З 11.03.2023 частина нормативу за коштами ФО на вимогу, а з 11.05.2023 — за строковими до 92 днів, у 10 в.п. не покривається бенчмарк-ОВДП.",
             "Джерела: рішення Правління НБУ № 926-рш (12.12.2019), 587-рш (16.12.2022), 40-рш (30.01.2023), 71-рш (23.02.2023), 24.03.2023, 275-рш (09.08.2023), 345-рш (20.09.2024)."])
RRD = f"'12.3'!$A${RR_R0}:$A${RR_R1}"
name("rr_dates", RRD)


def rr_col(g, c):
    j = RR_COLS.index((g, c))
    col = CL(2 + j)
    return f"'12.3'!${col}${RR_R0}:${col}${RR_R1}"


# ------------------------------------------------------------------ 12.1 dashboard: switches first (names used everywhere)
r = titles(S1, ["12. Інструмент аналізу банку", "12.1. Профіль банку: розрив переоцінки (п. 2.3) і тіньова вартість фондування (п. 2.4)"],
           f"(баланс на {latest_txt}; млн грн, ставки — % річних; блакитні клітинки на жовтому тлі — перемикачі)")
r = header(S1, r + 1, 1, ["Перемикач", "Значення", "Пояснення"], numbers=False)
SW0 = r
src_list = ["Виявлений (симетричний)", "Виявлений (асиметричний)", "Договірний"]
nmd_list = ["Поведінкова дюрація", "Договірна"]
yes_list = ["Так", "Ні"]
SW = [
    ("sw_bank", "Банк", clean_name(banks[0]["bank"]), None, "Оберіть банк зі списку (впорядковано за активами)."),
    ("sw_shock", "Шок облікової ставки для ЧПД, б.п.", 100, NUM0, "Паралельна зміна ставок; дія на ЧПД за перший рік, баланс постійний."),
    ("sw_src", "Джерело переоцінки ставок", src_list[0], None, "Виявлений — передача з п. 2.2 (β_h панельних LP); договірний — за строками (λ)."),
    ("sw_eveshock", "Шок для економічної вартості капіталу (EVE), б.п.", 200, NUM0, "Базельський стандартний паралельний шок — 200 б.п."),
    ("sw_nmd", "Кошти на вимогу в EVE", nmd_list[0], None, "Поведінкова — дюрація депозитної франшизи з п. 2.1 (D5) за поточної облікової; договірна — 1 міс."),
    ("sw_sot", "Поріг тесту на викид (SOT), частка капіталу", 0.15, PCT1, "BCBS (2016): |ΔEVE| > 15 % капіталу за найгіршого з шоків ± (модель лінійна, тож |ΔEVE| однаковий)."),
    ("sw_date", "Нормативи резервування станом на", dt.date.fromisoformat(RRW[-1][0]), DATEF, "Оберіть дату рішення з таблиці 12.3."),
    ("sw_s92", "Частка строкових депозитів ФО до 92 днів", par24.get("share_hh_term_le92", 0.3), PCT1, "У даних НБУ строки не розщеплено — припущення."),
    ("sw_cov", "Покриття нормативу бенчмарк-ОВДП", "Так", None, "Ні — увесь норматив тримається на кореспондентському рахунку."),
    ("sw_i", "Ставка альтернативного розміщення i (ДС овернайт)", num(macro["cd_on"]), PCT2, f"Типово — ставка овернайт ДС НБУ на {latest_txt}."),
    ("sw_y", "Дохідність бенчмарк-ОВДП", num(macro["y_bench"]), PCT2, "Типово — крива нульового купона НБУ, 3 роки."),
    ("sw_e", "Очікувана девальвація гривні, 12 міс.", num(macro["edep"]), PCT2, "Типово — очікування банків (опитування НБУ)."),
    ("sw_dgfu", "Збір ФГВФО, гривневі депозити ФО", par24.get("dgf_uah", 0.005), PCT2, "Базовий річний збір; диференційований ≥ базового."),
    ("sw_dgff", "Збір ФГВФО, валютні депозити ФО", par24.get("dgf_fx", 0.008), PCT2, ""),
]
for k, (nm, lab, val, fmt, expl) in enumerate(SW):
    rr_ = SW0 + k
    put(S1, rr_, 1, lab, al=LEFT, border=BOX)
    c = put(S1, rr_, 2, val, INP, fmt, RIGHT if fmt else LEFT, BOX, FILL_IN)
    put(S1, rr_, 3, expl, A9, al=LEFT_NW)
    name(nm, f"'12.1'!$B${rr_}")
r = SW0 + len(SW)
# derived helper cells (grey)
put(S1, r, 1, "Код банку / рядок у даних", al=LEFT, border=BOX)
put(S1, r, 2, f"=MATCH(sw_bank,{d1rng('bank')},0)", fmt=NUM0, al=RIGHT, border=BOX, fill=FILL_KEY)
name("sw_row", f"'12.1'!$B${r}")
put(S1, r, 3, f"=INDEX({d1rng('nkb')},sw_row)", A9, al=LEFT_NW)
r += 1
put(S1, r, 1, "Номер джерела переоцінки / знак шоку", al=LEFT, border=BOX)
put(S1, r, 2, f"=MATCH(sw_src,'12.4'!$P$5:$P$7,0)", fmt=NUM0, al=RIGHT, border=BOX, fill=FILL_KEY)
name("sw_srcn", f"'12.1'!$B${r}")
put(S1, r, 3, '=IF(sw_shock>=0,"шок вгору: асиметричні β підвищень","шок вниз: асиметричні β знижень")', A9, al=LEFT_NW)
r += 1
put(S1, r, 1, "Рядок нормативів у таблиці 12.3", al=LEFT, border=BOX)
put(S1, r, 2, '=COUNTIF(rr_dates,"<="&sw_date)', fmt=NUM0, al=RIGHT, border=BOX, fill=FILL_KEY)
name("sw_rrrow", f"'12.1'!$B${r}")
r += 1
SW_END = r

# data validations
dv_bank = DataValidation(type="list", formula1=f"='Д1'!$B${R1}:$B${RL}", allow_blank=False); S1.add_data_validation(dv_bank); dv_bank.add(f"B{SW0}")
dv_sh = DataValidation(type="list", formula1='"-300,-200,-100,-50,50,100,200,300"'); S1.add_data_validation(dv_sh); dv_sh.add(f"B{SW0 + 1}"); dv_sh.add(f"B{SW0 + 3}")
dv_src = DataValidation(type="list", formula1='"' + ",".join(src_list) + '"'); S1.add_data_validation(dv_src); dv_src.add(f"B{SW0 + 2}")
dv_nmd = DataValidation(type="list", formula1='"' + ",".join(nmd_list) + '"'); S1.add_data_validation(dv_nmd); dv_nmd.add(f"B{SW0 + 4}")
dv_date = DataValidation(type="list", formula1=f"='12.3'!$A${RR_R0}:$A${RR_R0 + len(RRW) - 1}"); S1.add_data_validation(dv_date); dv_date.add(f"B{SW0 + 6}")
dv_yes = DataValidation(type="list", formula1='"' + ",".join(yes_list) + '"'); S1.add_data_validation(dv_yes); dv_yes.add(f"B{SW0 + 8}")
dv_pct = DataValidation(type="decimal", operator="between", formula1="0", formula2="1", showErrorMessage=True, errorTitle="Частка", error="Введіть частку від 0 до 1 (наприклад, 15,5%).")
S1.add_data_validation(dv_pct)
for k in (5, 7, 9, 10, 11, 12, 13):
    dv_pct.add(f"B{SW0 + k}")

# ------------------------------------------------------------------ 12.4 parameters
r = titles(S4, ["12. Інструмент аналізу банку", "12.4. Параметри переоцінки: шляхи передачі облікової ставки, строки статей, неявний податок за продуктами"],
           "(β_h — кумулятивна зміна ставки статті на 1 в.п. облікової через h міс., панельні LP з п. 2.2; λ — частка залишку, що переоцінюється щомісяця)")
# lists for validations (column P)
put(S4, 4, 16, "Списки", A9I)
for k, v in enumerate(src_list): put(S4, 5 + k, 16, v, A9)
for k, v in enumerate(nmd_list): put(S4, 9 + k, 16, v, A9)
for k, v in enumerate(yes_list): put(S4, 12 + k, 16, v, A9)
r = max(r, 15) + 1
put(S4, r, 1, "12.4.1. Шляхи передачі β_h (п. 2.2)", A10B); r += 1
labs = ["Ставка", "Варіант"] + [f"h = {h}" for h in range(HMAX + 1)]
r = header(S4, r, 1, labs, numbers=True)
BROW = {}
for k in KEYS:
    for v in ("b", "b_up", "b_dn"):
        put(S4, r, 1, KEY_UA.get(k, k), al=LEFT_NW); put(S4, r, 2, VAR_UA[v], al=LEFT_NW)
        for h in range(HMAX + 1):
            put(S4, r, 3 + h, BETA[(k, v)].get(h, 0.0), INP, NUM3, RIGHT)
        BROW[(k, v)] = r
        r += 1
H12C = CL(3 + 12); H1C = CL(3 + 1)
S4.column_dimensions["A"].width = 38; S4.column_dimensions["B"].width = 12
for h in range(HMAX + 1):
    S4.column_dimensions[CL(3 + h)].width = 7.5
r += 1
put(S4, r, 1, "12.4.2. Статті балансу: способи переоцінки", A10B); r += 1
IT_LAB = ["Стаття", "Бік", "Знак", "Ставка переоцінки", "λ, частка на місяць", "Дюрація договірна, р.", "β₁₂ симетр.", "β₁₂ підвищ.", "β₁₂ зниж.",
          "Сер. β за 1-й рік, симетр.", "Сер. β, підвищ.", "Сер. β, зниж.", "Договірна частка за 12 міс.", "Договірна сер. частка за 1-й рік", "Обрана частка за 12 міс.", "Обрана сер. частка за 1-й рік"]
r = header(S4, r, 1, IT_LAB, height=54)
IT_R0 = r
for k, it in enumerate(items):
    rr_ = IT_R0 + k
    key = it["key"]
    put(S4, rr_, 1, it["label"], al=LEFT_NW)
    put(S4, rr_, 2, "актив" if it["side"] == "A" else "зобов'язання")
    put(S4, rr_, 3, 1 if it["side"] == "A" else -1, fmt=NUM0, al=RIGHT)
    put(S4, rr_, 4, KEY_UA.get(key, "не переоцінюється") if key else "не переоцінюється", A9, al=LEFT_NW)
    put(S4, rr_, 5, num(it["lambda"]), INP, NUM3, RIGHT, fill=FILL_IN)
    put(S4, rr_, 6, num(it["dur"]), INP, NUM2, RIGHT, fill=FILL_IN)
    for j, v in enumerate(("b", "b_up", "b_dn")):
        if key and (key, v) in BROW:
            br = BROW[(key, v)]
            put(S4, rr_, 7 + j, f"={H12C}{br}", LINKF, NUM3, RIGHT)
            put(S4, rr_, 10 + j, f"=AVERAGE({H1C}{br}:{H12C}{br})", fmt=NUM3, al=RIGHT)
        else:
            put(S4, rr_, 7 + j, 0, fmt=NUM3, al=RIGHT); put(S4, rr_, 10 + j, 0, fmt=NUM3, al=RIGHT)
    # contractual: share repriced by month h+1 = 1-(1-λ)^(h+1); 12-month point uses h = 12 as in 10_; 1st-year mean over h = 1..12
    put(S4, rr_, 13, f"=1-(1-E{rr_})^13", fmt=NUM3, al=RIGHT)
    put(S4, rr_, 14, f"=IF(E{rr_}<=0,0,IF(E{rr_}>=1,1,1-(1-E{rr_})^2*(1-(1-E{rr_})^12)/(12*E{rr_})))", fmt=NUM3, al=RIGHT)
    put(S4, rr_, 15, f"=CHOOSE(sw_srcn,G{rr_},IF(sw_shock>=0,H{rr_},I{rr_}),M{rr_})", fmt=NUM3, al=RIGHT)
    put(S4, rr_, 16, f"=CHOOSE(sw_srcn,J{rr_},IF(sw_shock>=0,K{rr_},L{rr_}),N{rr_})", fmt=NUM3, al=RIGHT)
IT_R1 = IT_R0 + NI - 1
for c, w in zip("CDEFGHIJKLMNOP", [6, 30, 9, 9, 8, 8, 8, 9, 8, 8, 10, 10, 10, 10]):
    S4.column_dimensions[c].width = max(S4.column_dimensions[c].width or 0, w)
r = IT_R1 + 2
# horizontal vectors (same order as Д1 columns) for 12.2
put(S4, r, 1, "12.4.3. Вектори для рейтингу банків (формули; не редагувати)", A10B); r += 1
VEC_LAB = ["знак × обрана частка за 12 міс.", "знак × договірна частка за 12 міс.", "знак × обрана сер. частка за 1-й рік", "знак × договірна дюрація"]
for j, it in enumerate(items):
    put(S4, r, 3 + j, it["item"], A9, al=CEN, border=BOX)
r += 1
VEC = {}
for k, lab in enumerate(VEC_LAB):
    put(S4, r, 1, lab, al=LEFT_NW)
    for j in range(NI):
        src = IT_R0 + j
        f = {0: f"=C{src}*O{src}", 1: f"=C{src}*M{src}", 2: f"=C{src}*P{src}", 3: f"=C{src}*F{src}"}[k]
        put(S4, r, 3 + j, f, fmt=NUM3, al=RIGHT)
    VEC[k] = f"'12.4'!$C${r}:${CL(2 + NI)}${r}"
    r += 1
r += 1
# products: implicit tax
put(S4, r, 1, "12.4.4. Неявний податок на депозити за продуктами (п. 2.4) за обраних перемикачів", A10B); r += 1
PR_LAB = ["Продукт", "Валюта", "Норматив ρ", "Межа покриття ОВДП", "Непокривна частина", "у т.ч. в ОВДП ρ_ОВДП", "на кор. рахунку ρ_кор", "Збір ФГВФО f",
          "Податок: кор. рахунок ρ_кор·i", "Податок: ОВДП ρ_ОВДП·(i − y)", "Неявний податок τ", "Валюта (1/0)"]
r = header(S4, r, 1, PR_LAB, height=54)
PR_R0 = r
idx = lambda g, c: f"INDEX({rr_col(g, c)},sw_rrrow)"
covx = f"INDEX('12.3'!$L${RR_R0}:$L${RR_R1},sw_rrrow)"
for k, (p, c) in enumerate(PRODS):
    rr_ = PR_R0 + k
    put(S4, rr_, 1, PROD_UA[p], al=LEFT_NW); put(S4, rr_, 2, CCY_UA[c])
    if p == "hh_term":
        rho = f"=sw_s92*{idx('hh_term_short', c)}+(1-sw_s92)*{idx('hh_term_long', c)}"
        nc = f'=IF(sw_date>=DATE(2023,5,11),sw_s92*MIN(0.1,{idx("hh_term_short", c)}),0)'
    else:
        rho = f"={idx(p, c)}"
        nc = f"=IF(sw_date>=DATE(2023,3,11),MIN(0.1,C{rr_}),0)" if p == "hh_demand" else "=0"
    put(S4, rr_, 3, rho, fmt=PCT1, al=RIGHT)
    put(S4, rr_, 4, f"={covx}", fmt=PCT1, al=RIGHT)
    put(S4, rr_, 5, nc, fmt=PCT1, al=RIGHT)
    put(S4, rr_, 6, f'=IF(sw_cov="Так",MIN(D{rr_}*C{rr_},MAX(C{rr_}-E{rr_},0)),0)', fmt=PCT1, al=RIGHT)
    put(S4, rr_, 7, f"=C{rr_}-F{rr_}", fmt=PCT1, al=RIGHT)
    put(S4, rr_, 8, (f"=sw_dgfu" if c == "uah" else "=sw_dgff") if p.startswith("hh") else "=0", fmt=PCT2, al=RIGHT)
    put(S4, rr_, 9, f"=G{rr_}*sw_i", fmt=PCT2, al=RIGHT)
    put(S4, rr_, 10, f"=F{rr_}*(sw_i-sw_y)", fmt=PCT2, al=RIGHT)
    put(S4, rr_, 11, f"=H{rr_}+I{rr_}+J{rr_}", fmt=PCT2, al=RIGHT)
    put(S4, rr_, 12, 1 if c == "fx" else 0, fmt=NUM0, al=RIGHT)
PR_R1 = PR_R0 + NP - 1
r = PR_R1 + 2
for j in range(NP):
    put(S4, r, 3 + j, f"{PRODS[j][0]}_{PRODS[j][1]}", A9, al=CEN, border=BOX)
r += 1
for k, (lab, col) in enumerate([("неявний податок τ", "K"), ("валюта (1/0)", "L"), ("збір ФГВФО f", "H"), ("ρ_ОВДП", "F"), ("ρ", "C")]):
    put(S4, r, 1, lab, al=LEFT_NW)
    for j in range(NP):
        put(S4, r, 3 + j, f"={col}{PR_R0 + j}", fmt=NUM3, al=RIGHT)
    VEC[f"p_{k}"] = f"'12.4'!$C${r}:${CL(2 + NP)}${r}"
    r += 1
S4.freeze_panes = "C5"
foot(S4, r, ["Блакитні клітинки (β, λ, дюрації) можна змінювати для аналізу чутливості; решта — формули.",
             "Договірна частка, переоцінена до місяця h: 1 − (1 − λ)^(h+1); кошти на вимогу (λ = 1) — повністю в першому місяці."])

# ------------------------------------------------------------------ 12.1 key figures + tables
row = lambda k: f"INDEX({d1rng(k)},sw_row)"
r = SW_END + 1
put(S1, r, 1, "Ключові показники банку", A10B); r += 1
KF0 = r
U = lambda it: f"INDEX({d1rng('U_' + it)},sw_row)"
Fx = lambda it: f"INDEX({d1rng('F_' + it)},sw_row)"
# item table rows (filled below) are referenced by the key figures
IT1_HDR = KF0 + 18
IT1_R0 = IT1_HDR + 4                     # first data row after header (2 rows top+labels) + numbers + spacer
KF = [
    ("Активи, млн грн", f"={row('TA')}", NUM1),
    ("Капітал, млн грн", f"={row('EQ')}", NUM1),
    ("ЧПД за останні 12 міс., млн грн", f"={row('NII12')}", NUM1),
    ("Розрив переоцінки 12 міс. (обране джерело), % активів", f"=SUM(F{IT1_R0}:F{IT1_R0 + NI - 1})/{row('TA')}", PCT1),
    ("Розрив переоцінки 12 міс. (договірний), % активів", f"=SUM(H{IT1_R0}:H{IT1_R0 + NI - 1})/{row('TA')}", PCT1),
    ("ΔЧПД за перший рік від шоку, млн грн", f"=SUM(J{IT1_R0}:J{IT1_R0 + NI - 1})", NUM1),
    ("ΔЧПД, % річного ЧПД", f'=IFERROR(B{KF0 + 5}/{row("NII12")},"…")', PCT1),
    ("ΔЧПД, % капіталу", f'=IFERROR(B{KF0 + 5}/{row("EQ")},"…")', PCT1),
    ("ΔEVE від шоку для EVE, млн грн", f"=SUM(L{IT1_R0}:L{IT1_R0 + NI - 1})", NUM1),
    ("ΔEVE, % капіталу", f'=IFERROR(B{KF0 + 8}/{row("EQ")},"…")', PCT1),
    ("Тест на викид (SOT) порушено?", f'=IF(ISNUMBER(B{KF0 + 9}),IF(ABS(B{KF0 + 9})>sw_sot,"так","ні"),"…")', None),
    ("Номінальна вартість депозитів, %", None, PCT2),
    ("Економічна вартість депозитів (з резервами і ФГВФО), %", None, PCT2),
    ("Неявний податок на депозити, в.п.", None, PCT2),
    ("Поріг дохідності вільних коштів (SFC), %", None, PCT2),
    ("Маржа «паркування» в ДС: i − SFC, в.п.", None, PCT2),
]
for k, (lab, f, fmt) in enumerate(KF):
    put(S1, KF0 + k, 1, lab, al=LEFT, border=BOX)
    put(S1, KF0 + k, 2, f, A10B, fmt, RIGHT, BOX)
# ---- table 12.1.1 items
r = IT1_HDR - 1
put(S1, r, 1, "12.1.1. Розрив переоцінки та чутливість банку за статтями балансу", A10B)
labs = ["Стаття", "Бік", "Гривня", "Валюта", "Частка переоцінки за 12 міс. (обрана)", "Внесок у розрив (обраний)", "Частка переоцінки за 12 міс. (договірна)",
        "Внесок у розрив (договірний)", "Сер. частка переоцінки за 1-й рік", "ΔЧПД за 1-й рік", "Дюрація, років", "ΔEVE"]
top = [(2, 3, "Залишок, млн грн")]
rr_ = header(S1, IT1_HDR, 1, labs, top=top, height=54)
assert rr_ == IT1_R0, (rr_, IT1_R0)
for k, it in enumerate(items):
    q = IT1_R0 + k; p4 = IT_R0 + k; code = it["item"]
    put(S1, q, 1, it["label"], al=LEFT_NW)
    put(S1, q, 2, "актив" if it["side"] == "A" else "зобов'язання", al=LEFT_NW)
    put(S1, q, 3, f"={U(code)}", fmt=NUM1, al=RIGHT)
    put(S1, q, 4, f"={Fx(code)}", fmt=NUM1, al=RIGHT)
    put(S1, q, 5, f"='12.4'!O{p4}", LINKF, NUM3, RIGHT)
    put(S1, q, 6, f"='12.4'!C{p4}*C{q}*E{q}", fmt=NUM1, al=RIGHT)
    put(S1, q, 7, f"='12.4'!M{p4}", LINKF, NUM3, RIGHT)
    put(S1, q, 8, f"='12.4'!C{p4}*C{q}*G{q}", fmt=NUM1, al=RIGHT)
    put(S1, q, 9, f"='12.4'!P{p4}", LINKF, NUM3, RIGHT)
    put(S1, q, 10, f"='12.4'!C{p4}*C{q}*I{q}*sw_shock/10000", fmt=NUM1, al=RIGHT)
    if code == "hh_dem":
        dur = f"=IF(sw_nmd=\"Поведінкова дюрація\",{row('D_hhdem_b')},'12.4'!F{p4})"
    elif code == "corp_dem":
        dur = f"=IF(sw_nmd=\"Поведінкова дюрація\",{row('D_corpdem_b')},'12.4'!F{p4})"
    else:
        dur = f"='12.4'!F{p4}"
    put(S1, q, 11, dur, fmt=NUM2, al=RIGHT)
    put(S1, q, 12, f"=-'12.4'!C{p4}*(C{q}+D{q})*K{q}*sw_eveshock/10000", fmt=NUM1, al=RIGHT)
q = IT1_R0 + NI
na = sum(1 for it in items if it["side"] == "A")
for lab, rng, bold in [("Разом активи", (IT1_R0, IT1_R0 + na - 1), False), ("Разом зобов'язання", (IT1_R0 + na, IT1_R0 + NI - 1), False), ("Розрив / разом", (IT1_R0, IT1_R0 + NI - 1), True)]:
    f = A10B if bold else A10
    put(S1, q, 1, lab, f, al=LEFT_NW, border=TOPB)
    for c in range(2, 13):
        put(S1, q, c, None, border=TOPB)
    for c in (3, 4) if not bold else ():
        put(S1, q, c, f"=SUM({CL(c)}{rng[0]}:{CL(c)}{rng[1]})", f, NUM1, RIGHT, TOPB)
    for c in (6, 8, 10, 12):
        put(S1, q, c, f"=SUM({CL(c)}{rng[0]}:{CL(c)}{rng[1]})", f, NUM1, RIGHT, TOPB)
    q += 1
put(S1, q, 1, "у % активів (розрив), % ЧПД (ΔЧПД), % капіталу (ΔEVE)", A10I, al=LEFT_NW)
put(S1, q, 6, f"=F{q - 1}/{row('TA')}", A10I, PCT1, RIGHT)
put(S1, q, 8, f"=H{q - 1}/{row('TA')}", A10I, PCT1, RIGHT)
put(S1, q, 10, f'=IFERROR(J{q - 1}/{row("NII12")},"…")', A10I, PCT1, RIGHT)
put(S1, q, 12, f'=IFERROR(L{q - 1}/{row("EQ")},"…")', A10I, PCT1, RIGHT)
IT1_END = q
# ---- table 12.1.2 products
r = IT1_END + 2
put(S1, r, 1, "12.1.2. Номінальна та економічна вартість депозитного фондування банку", A10B); r += 1
labs = ["Продукт", "Валюта", "Залишок, млн грн", "Ставка r", "Норматив ρ", "у т.ч. в ОВДП", "Збір ФГВФО f", "Податок: кор. рахунок", "Податок: ОВДП",
        "Економічна вартість", "Економічна вартість, грн-еквівалент", "Поріг SFC (грн-екв.)"]
P1_R0 = header(S1, r, 1, labs, height=54)
for k, (p, c) in enumerate(PRODS):
    q = P1_R0 + k; p4 = PR_R0 + k
    put(S1, q, 1, PROD_UA[p], al=LEFT_NW); put(S1, q, 2, CCY_UA[c], al=LEFT_NW)
    put(S1, q, 3, f"={row('S_' + p + '_' + c)}", fmt=NUM1, al=RIGHT)
    put(S1, q, 4, f"={row('R_' + p + '_' + c)}", fmt=PCT2, al=RIGHT)
    put(S1, q, 5, f"='12.4'!C{p4}", LINKF, PCT1, RIGHT)
    put(S1, q, 6, f"='12.4'!F{p4}", LINKF, PCT1, RIGHT)
    put(S1, q, 7, f"='12.4'!H{p4}", LINKF, PCT2, RIGHT)
    put(S1, q, 8, f"='12.4'!I{p4}", LINKF, PCT2, RIGHT)
    put(S1, q, 9, f"='12.4'!J{p4}", LINKF, PCT2, RIGHT)
    put(S1, q, 10, f"=D{q}+H{q}+I{q}+G{q}", fmt=PCT2, al=RIGHT)
    rueq = f"((1+D{q})*(1+sw_e)-1)" if c == "fx" else f"D{q}"
    put(S1, q, 11, f"={rueq}+H{q}+I{q}+G{q}", fmt=PCT2, al=RIGHT)
    put(S1, q, 12, f"=({rueq}+G{q}-F{q}*sw_y)/(1-E{q})", fmt=PCT2, al=RIGHT)
q = P1_R0 + NP
put(S1, q, 1, "Разом (зважено за залишками)", A10B, al=LEFT_NW, border=TOPB); put(S1, q, 2, None, border=TOPB)
put(S1, q, 3, f"=SUM(C{P1_R0}:C{q - 1})", A10B, NUM1, RIGHT, TOPB)
for c in range(4, 13):
    L = CL(c)
    put(S1, q, c, f'=IFERROR(SUMPRODUCT($C${P1_R0}:$C${q - 1},{L}{P1_R0}:{L}{q - 1})/$C${q},"…")', A10B, PCT2 if c not in (5, 6) else PCT1, RIGHT, TOPB)
P1_TOT = q
# fill key figures for 2.4
S1.cell(KF0 + 11, 2).value = f"=D{P1_TOT}"
S1.cell(KF0 + 12, 2).value = f"=J{P1_TOT}"
S1.cell(KF0 + 13, 2).value = f'=IFERROR(J{P1_TOT}-D{P1_TOT},"…")'
S1.cell(KF0 + 14, 2).value = f"=L{P1_TOT}"
S1.cell(KF0 + 15, 2).value = f'=IFERROR(sw_i-L{P1_TOT},"…")'
end = foot(S1, P1_TOT + 1, [
    "Розрив = Σ знак × залишок у гривні × частка, переоцінена за 12 міс.; обрана частка — β₁₂ передачі з п. 2.2 (виявлений) або 1 − (1 − λ)¹³ (договірний). Валютні статті на гривневу облікову не реагують і в розрив не входять.",
    "ΔЧПД = Σ знак × залишок × середня частка переоцінки за 1-й рік × шок; ΔEVE = − Σ знак × (гривня + валюта) × дюрація × шок.",
    "Економічна вартість = r + ρ_кор·i + ρ_ОВДП·(i − y) + f; гривневий еквівалент валюти: (1 + r)(1 + E[Δs]) − 1; поріг SFC = (r + f − ρ_ОВДП·y)/(1 − ρ) — мінімальна дохідність вільних коштів.",
])
widths = {"A": 46, "B": 24, "C": 12, "D": 12, "E": 12, "F": 12, "G": 12, "H": 12, "I": 12, "J": 12, "K": 12, "L": 12}
for c, w in widths.items():
    S1.column_dimensions[c].width = w
S1.column_dimensions["C"].width = 14
S1.freeze_panes = "A5"

# ------------------------------------------------------------------ 12.5 bank history (formulas on Д2)
r = titles(S5, ["12. Інструмент аналізу банку", "12.5. Динаміка показників обраного банку за останні 60 місяців"],
           "(%, в.п.; банк обирається на аркуші 12.1)")
put(S5, r, 1, "=\"Банк: \"&sw_bank", A10I); r += 1
labs = ["Період", "Виявлений розрив 12 міс., % активів", "Договірний розрив 12 міс., % активів", "Чиста процентна маржа, %", "Вартість депозитів (п. 2.1), %",
        "Облікова ставка НБУ, %", "Номінальна вартість депозитів (п. 2.4), %", "Економічна вартість депозитів, %"]
H5_R0 = header(S5, r + 1, 1, labs, height=54)
labs5 = labs
hk = f"'Д2'!$A$5:$A${H_LAST}"
for k, m in enumerate(MONTHS):
    q = H5_R0 + k
    put(S5, q, 1, dt.date.fromisoformat(m), fmt=MONF, al=LEFT_NW)
    for j, colk in enumerate(["RG12", "CG12", "nim", "cod4", "policy", "cod_nom", "ec"]):
        dc = CL(H_COLS.index(colk) + 1)
        f = f'=IFERROR(INDEX(\'Д2\'!${dc}$5:${dc}${H_LAST},MATCH(INDEX({d1rng("nkb")},sw_row)&"|"&A{q},{hk},0))*1,NA())'
        put(S5, q, 2 + j, f, fmt=NUM2, al=RIGHT)
H5_R1 = H5_R0 + len(MONTHS) - 1
S5.column_dimensions["A"].width = 10
for c in range(2, 9):
    S5.column_dimensions[CL(c)].width = 15
S5.freeze_panes = S5.cell(H5_R0, 2)
foot(S5, H5_R1 + 1, ["#Н/Д (порожні точки на графіках) — банк не звітував або показник не визначений у цьому місяці."])

# ------------------------------------------------------------------ 12.2 ranking of all banks
r = titles(S2, ["12. Інструмент аналізу банку", "12.2. Розрив переоцінки і вартість фондування всіх банків за обраних перемикачів"],
           "(перемикачі — на аркуші 12.1; фільтр і сортування — стрілками в шапці)")
labs = ["Код", "Банк", "Власність", "Активи, млн грн", "Розрив 12 міс. (договірний), % активів", "Розрив 12 міс. (обране джерело), % активів", "ΔЧПД за 1-й рік, % ЧПД",
        "ΔEVE, % капіталу", "SOT порушено", "Номінальна вартість депозитів, %", "Економічна вартість депозитів, %", "Неявний податок, в.п.", "Ранг за розривом (1 = найбільший)"]
R2_0 = header(S2, r + 1, 1, labs, height=66)
TAc, EQc, NIIc = CL(COL["TA"]), CL(COL["EQ"]), CL(COL["NII12"])
dh = (ITEM_CODES.index("hh_dem"), ITEM_CODES.index("corp_dem"))
for i in range(len(banks)):
    q = R2_0 + i; d = R1 + i
    Ur = f"'Д1'!${U0}{d}:${U1}{d}"; Fr = f"'Д1'!${F0}{d}:${F1}{d}"; Sr = f"'Д1'!${S0c}{d}:${S1c}{d}"; Rr = f"'Д1'!${Rr0}{d}:${Rr1}{d}"
    put(S2, q, 1, f"='Д1'!A{d}", LINKF, al=LEFT_NW)
    put(S2, q, 2, f"='Д1'!B{d}", LINKF, al=LEFT_NW)
    put(S2, q, 3, f"='Д1'!C{d}", LINKF, al=LEFT_NW)
    put(S2, q, 4, f"='Д1'!{TAc}{d}", LINKF, NUM0, RIGHT)
    put(S2, q, 5, f"=SUMPRODUCT({Ur},{VEC[1]})/D{q}", fmt=PCT1, al=RIGHT)
    put(S2, q, 6, f"=SUMPRODUCT({Ur},{VEC[0]})/D{q}", fmt=PCT1, al=RIGHT)
    put(S2, q, 7, f'=IFERROR(SUMPRODUCT({Ur},{VEC[2]})*sw_shock/10000/\'Д1\'!{NIIc}{d},"…")', fmt=PCT1, al=RIGHT)
    adj = " + ".join(
        f"'12.4'!$C${IT_R0 + j}*('Д1'!{CL(COL['U_' + ITEM_CODES[j]])}{d}+'Д1'!{CL(COL['F_' + ITEM_CODES[j]])}{d})*('Д1'!{CL(COL[dcol])}{d}-'12.4'!$F${IT_R0 + j})"
        for j, dcol in zip(dh, ("D_hhdem_b", "D_corpdem_b")))
    put(S2, q, 8, f'=IFERROR(-sw_eveshock/10000*(SUMPRODUCT({Ur}+{Fr},{VEC[3]})+IF(sw_nmd="Поведінкова дюрація",{adj},0))/\'Д1\'!{EQc}{d},"…")', fmt=PCT1, al=RIGHT)
    put(S2, q, 9, f'=IF(ISNUMBER(H{q}),IF(ABS(H{q})>sw_sot,"так","ні"),"…")', al=CEN)
    put(S2, q, 10, f'=IF(SUM({Sr})>0,SUMPRODUCT({Sr},{Rr})/SUM({Sr}),"…")', fmt=PCT2, al=RIGHT)
    put(S2, q, 11, f'=IF(SUM({Sr})>0,(SUMPRODUCT({Sr},{Rr})+SUMPRODUCT({Sr},{VEC["p_0"]}))/SUM({Sr}),"…")', fmt=PCT2, al=RIGHT)
    put(S2, q, 12, f'=IFERROR(K{q}-J{q},"…")', fmt=PCT2, al=RIGHT)
    put(S2, q, 13, f"=RANK(F{q},$F${R2_0}:$F${R2_0 + len(banks) - 1},0)", fmt=NUM0, al=CEN)
R2_1 = R2_0 + len(banks) - 1
q = R2_1 + 1
put(S2, q, 1, None, border=TOPB); put(S2, q, 2, "Система (зважено за активами / депозитами)", A10B, al=LEFT_NW, border=TOPB); put(S2, q, 3, None, border=TOPB)
put(S2, q, 4, f"=SUM(D{R2_0}:D{R2_1})", A10B, NUM0, RIGHT, TOPB)
for c in (5, 6):
    L = CL(c)
    put(S2, q, c, f"=SUMPRODUCT($D${R2_0}:$D${R2_1},{L}{R2_0}:{L}{R2_1})/$D${q}", A10B, PCT1, RIGHT, TOPB)
for c in range(7, 14):
    put(S2, q, c, None, border=TOPB)
S2.auto_filter.ref = f"A{R2_0 - 3}:M{R2_1}"
S2.freeze_panes = S2.cell(R2_0, 3)
for c, w in zip("ABCDEFGHIJKLM", [7, 36, 11, 12, 12, 12, 11, 11, 9, 12, 12, 11, 10]):
    S2.column_dimensions[c].width = w
foot(S2, q + 1, ["Ранг 1 — банк, маржа якого найбільше зростає за підвищення облікової ставки (найбільший додатний розрив). Неявний податок = економічна − номінальна вартість депозитів."])
# auto filter must sit on the label row: header() wrote labels at R2_0-3 (labels), R2_0-2 numbers, R2_0-1 spacer
S2.auto_filter.ref = f"A{R2_0 - 3}:M{R2_1}"

# ------------------------------------------------------------------ charts on 12.1
ch = BarChart(); ch.type = "bar"; ch.grouping = "clustered"; ch.overlap = -10
ch.title = "Внесок статей у розрив переоцінки 12 міс., млн грн"
ch.add_data(Reference(S1, min_col=6, min_row=IT1_R0, max_row=IT1_R0 + NI - 1), titles_from_data=False)
ch.add_data(Reference(S1, min_col=8, min_row=IT1_R0, max_row=IT1_R0 + NI - 1), titles_from_data=False)
ch.set_categories(Reference(S1, min_col=1, min_row=IT1_R0, max_row=IT1_R0 + NI - 1))
from openpyxl.chart.series import SeriesLabel
ch.series[0].tx = SeriesLabel(v="обраний"); ch.series[1].tx = SeriesLabel(v="договірний")
ch.series[0].graphicalProperties.solidFill = "1F4E79"; ch.series[1].graphicalProperties.solidFill = "A6A6A6"
ch.x_axis.scaling.orientation = "maxMin"; ch.x_axis.tickLblPos = "low"; ch.x_axis.tickLblSkip = 1; ch.y_axis.number_format = "#,##0"
ch.height = 9; ch.width = 17
axis_font(ch)
S1.add_chart(ch, "N4")

lc = LineChart(); lc.title = "Розрив переоцінки банку, % активів"
for cc in (2, 3):
    lc.series.append(Series(Reference(S5, min_col=cc, min_row=H5_R0, max_row=H5_R1), title=labs5[cc - 1]))
lc.set_categories(Reference(S5, min_col=1, min_row=H5_R0, max_row=H5_R1))
lc.series[0].graphicalProperties.line.solidFill = "1F4E79"; lc.series[1].graphicalProperties.line.solidFill = "A6A6A6"
for s in lc.series: s.graphicalProperties.line.width = 22000; s.smooth = False
lc.x_axis.number_format = "mm.yy"; lc.x_axis.tickLblPos = "low"; lc.x_axis.tickLblSkip = 6
lc.height = 8; lc.width = 17
axis_font(lc)
S1.add_chart(lc, "N23")

lc2 = LineChart(); lc2.title = "Вартість депозитів банку та облікова ставка, %"
for cc in (6, 7, 8):
    lc2.series.append(Series(Reference(S5, min_col=cc, min_row=H5_R0, max_row=H5_R1), title=labs5[cc - 1]))
lc2.set_categories(Reference(S5, min_col=1, min_row=H5_R0, max_row=H5_R1))
for s, colr, dash in zip(lc2.series, ["000000", "A6A6A6", "1F4E79"], ["dash", "solid", "solid"]):
    s.graphicalProperties.line.solidFill = colr; s.graphicalProperties.line.width = 22000; s.graphicalProperties.line.dashStyle = dash; s.smooth = False
lc2.x_axis.number_format = "mm.yy"; lc2.x_axis.tickLblPos = "low"; lc2.x_axis.tickLblSkip = 6
lc2.height = 8; lc2.width = 17
axis_font(lc2)
S1.add_chart(lc2, "N40")

# ------------------------------------------------------------------ contents sheet 12
S0.column_dimensions["A"].width = 100
r = 2
put(S0, r, 1, "12. Інструмент аналізу банку: процентний ризик балансу і тіньова вартість депозитного фондування", A10B); r += 2
put(S0, r, 1, "12.1. Аналіз окремого банку", A10B); r += 1
TOC = [("12.1", "12.1. Профіль банку: перемикачі, розрив переоцінки (п. 2.3) і вартість фондування (п. 2.4)"),
       ("12.5", "12.5. Динаміка показників обраного банку за останні 60 місяців")]
for sh, txt in TOC:
    c = put(S0, r, 1, txt, A10L, al=LEFT_NW); c.hyperlink = f"#'{sh}'!A1"; r += 1
r += 1
put(S0, r, 1, "12.2. Порівняння банків", A10B); r += 1
c = put(S0, r, 1, "12.2. Розрив переоцінки і вартість фондування всіх банків за обраних перемикачів", A10L); c.hyperlink = "#'12.2'!A1"; r += 2
put(S0, r, 1, "12.3–12.4. Припущення (редагуються)", A10B); r += 1
for sh, txt in [("12.3", "12.3. Нормативи обов'язкового резервування НБУ"), ("12.4", "12.4. Параметри переоцінки: β_h, λ, дюрації, неявний податок за продуктами")]:
    c = put(S0, r, 1, txt, A10L); c.hyperlink = f"#'{sh}'!A1"; r += 1
r += 1
put(S0, r, 1, "Дані (не редагувати)", A10B); r += 1
for sh, txt in [("Д1", f"Д1. Баланс, ЧПД, депозити і ставки банків на {latest_txt}"), ("Д2", "Д2. Історія показників банків")]:
    c = put(S0, r, 1, txt, A10L); c.hyperlink = f"#'{sh}'!A1"; r += 1
r += 1
put(S0, r, 1, "Як користуватися", A10B); r += 1
for t in ["1. На аркуші 12.1 оберіть банк і задайте перемикачі (блакитний шрифт на жовтому тлі); усі таблиці, графіки і рейтинг 12.2 перераховуються формулами.",
          "2. «Джерело переоцінки»: виявлений — фактична передача облікової ставки у ставки статей (панельні локальні проєкції, п. 2.2); договірний — лише строки інструментів.",
          "3. Для аналізу чутливості змінюйте β, λ і дюрації на аркуші 12.4 та нормативи на аркуші 12.3 (нові рішення НБУ додавайте в порожні рядки за датою).",
          "4. Кольори: блакитний шрифт — вхідні дані; зелений — посилання на інший аркуш; чорний — формули; сірий фон — службові проміжні величини.",
          f"5. Файл згенеровано скриптом 12_bank_excel.py {dt.date.today().strftime('%d.%m.%Y')}; для оновлення перезапустіть run_all_v6.R (MODELS включає \"10\", \"11\", \"12\")."]:
    put(S0, r, 1, t, al=LEFT); S0.row_dimensions[r].height = 27; r += 1
r += 1
put(S0, r, 1, "«–» — явища не було; «…» — даних немає або показник не визначений; «#Н/Д» на графіках — немає спостереження.", A9I); r += 1
put(S0, r, 1, "Методологія: Drechsler, Savov & Schnabl (2017, 2021); English, Van den Heuvel & Zakrajšek (2018); BCBS (2016) IRRBB; Reinhart & Reinhart (1999); Federico, Vegh & Vuletin (2014).", A9I)

# page setup
for ws in wb.worksheets:
    ws.page_setup.orientation = "landscape"; ws.page_setup.fitToWidth = 1; ws.page_setup.fitToHeight = 0
    ws.sheet_properties.pageSetUpPr.fitToPage = True
    ws.print_options.horizontalCentered = True
import re as _re
_pat = _re.compile(r"(?<![A-Za-z_'!$])(" + "|".join(sorted(map(_re.escape, NAMES), key=len, reverse=True)) + r")(?![A-Za-z0-9_(])")
for _ws in wb.worksheets:
    for _row in _ws.iter_rows():
        for _c in _row:
            if isinstance(_c.value, str) and _c.value.startswith("="):
                _c.value = _pat.sub(lambda m: NAMES[m.group(1)], _c.value)
    for _dv in _ws.data_validations.dataValidation:
        if _dv.formula1 and _dv.formula1.startswith("="):
            _dv.formula1 = _dv.formula1[1:]
wb.calculation.calcMode = "auto"
wb.active = wb.sheetnames.index("12.1")
wb.calculation.fullCalcOnLoad = True
wb.save(FILE)
print("OK:", FILE, f"банків: {len(banks)}, місяців історії: {len(MONTHS)}, рішень НБУ: {len(RRW)}")
