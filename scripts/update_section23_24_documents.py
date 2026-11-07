"""Insert regenerated section 2.3/2.4 figures into the reviewed documents.

Works on existing reviewed DOCX files and is safe to repeat. Native equations
and unrelated section text are preserved. Run after the R section scripts.
"""
from pathlib import Path
import json
import pandas as pd
from docx import Document
from docx.enum.style import WD_STYLE_TYPE
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Cm, Pt

ROOT = Path(__file__).resolve().parents[1]
PREFIX = "ChartUpdate"


def styles(d):
    for suffix, base in [("Text", "Normal"), ("Heading", "Heading 3"), ("Caption", "Caption")]:
        name = PREFIX + suffix
        if name not in d.styles:
            d.styles.add_style(name, WD_STYLE_TYPE.PARAGRAPH)
        s = d.styles[name]
        s.base_style = d.styles[base] if base in d.styles else d.styles["Normal"]
        s.font.name = "Times New Roman"
        s.font.size = Pt(11 if suffix == "Caption" else 12)
        s.paragraph_format.space_after = Pt(8)
        s.paragraph_format.line_spacing = 1.15
    for p in list(d.paragraphs):
        if p.style.name.startswith(PREFIX):
            p._p.getparent().remove(p._p)


def insert(anchor, text="", kind="Text"):
    return anchor.insert_paragraph_before(text, style=PREFIX + kind)


def figure(anchor, path, title, caption, interpretation, width=15.5):
    p = insert(anchor, title, "Heading")
    p.paragraph_format.page_break_before = True
    p.paragraph_format.keep_with_next = True
    p = insert(anchor)
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.keep_with_next = True
    shape = p.add_run().add_picture(str(path), width=Cm(width))
    shape._inline.docPr.set("descr", title)
    p = insert(anchor, caption, "Caption")
    p.paragraph_format.keep_with_next = True
    insert(anchor, interpretation)


def update(root=ROOT):
    root = Path(root)
    out = root / "output_nbu"
    a = out / "section_2_3"
    b = out / "section_2_4"
    s = pd.read_csv(a / "tables/S23_R3_system_monthly.csv").iloc[-1]
    risk = pd.read_csv(a / "tables/S23_R9_bank_risk_views_latest.csv")
    validation = pd.read_csv(a / "tables/S23_R4_validation_nim.csv")
    realised = pd.read_csv(a / "tables/S23_R6_bank_realised_nim_beta.csv")
    duration = pd.read_csv(root / "review/R23_duration_sensitivity_system.csv")
    carry = pd.read_csv(b / "tables/S24_R9_cd_spread.csv").iloc[-1]
    fx = pd.read_csv(b / "tables/S24_R9_fx_cost_gap.csv")
    fx = fx.loc[fx.month.eq(fx.month.max())].set_index("prod")
    date = pd.Timestamp(s.month).strftime("%m.%Y")
    source = f"Джерело — розрахунки автора за панеллю НБУ, баланс {date}."
    # Avoid long punctuation in the newly inserted text.
    source = source.replace(" — ", " ")
    scope = ("Модулі 10 і 11 повторно виконано в R на незміненій панелі з 7 206 банко-місяців. "
             "Оновлено розрахунки й графіки підрозділів 2.3 та 2.4, незалежні Python-перевірки та обидва Excel-інструменти. "
             "Коефіцієнти LP з модуля 07 залишаються результатами попереднього прогону. "
             "Моделі 2.1, 2.2 та OU повторно не оцінювалися; повний run_all_v6.R не виконувався. "
             "Прогнозна перевага виявленого розриву поза вибіркою не встановлена.")
    path = root / "docs/section2_reviewed.docx"
    d = Document(path)
    styles(d)
    for p in d.paragraphs:
        t = p.text
        if t.startswith("Базові оцінки нижче") or t.startswith("Модулі 10 і 11 повторно"):
            p.text = scope
        elif t.startswith("Редакція після оновлення капіталу"):
            p.text = "Редакція з оновленими графіками 07 листопада 2026 року"
        elif t.startswith("Розрив переоцінки (2.3)."):
            p.text = (f"Розрив переоцінки (2.3). У {date} договірний розрив 12 місяців становить {100*s.CG12:.1f}% активів, "
                      f"виявлений {100*s.RG12:.1f}%. За +100 б.п. модельна зміна річного ЧПД становить "
                      f"{s.dNII_c/1000:.2f} та +{s.dNII_r/1000:.2f} млрд грн відповідно. "
                      "Це умовна чутливість за постійного балансу. Коротка перевірка з коефіцієнтами, оціненими лише на минулих датах, не встановлює прогнозної переваги.")
        elif t.startswith("Стрес-тест IRRBB."):
            p.text = ("Стрес-тест IRRBB. Excel-інструмент перебудовано для 59 банків із фактичним Tier 1. "
                      "Поріг втрати EVE 15% застосовується до від’ємної зміни за відповідним сценарієм. "
                      "Сценарні частки залежать від дюрацій і передачі ставок та не встановлюють наглядових порушень.")
        elif t.startswith("Виявлений розрив прогнозує зміну маржі"):
            p.text = ("У повній вибірці виявлений розрив додатно пов’язаний зі зміною маржі. "
                      "Цей зв’язок не ототожнюється з прогнозною точністю чи причинним ефектом. "
                      "Незалежна перевірка з лагованими капіталом, LCR і розміром банку має лише 9 місяців та 516 спостережень. "
                      "Її коефіцієнт становить 0,783, стандартна похибка 0,634, p=0,252. "
                      "Повновибіркова валідація модуля 10 та ця перевірка мають різні специфікації й не є однаковими оцінками.")
        elif t.startswith("Поріг дохідності") or t.startswith("Ефективний норматив резервування"):
            p.text = ("Ефективний модельний норматив резервування становить 20,1%, а поріг дохідності вільних коштів у гривневому еквіваленті 5,72%. "
                      "У використаному ряді ставка ДС овернайт за останній місяць дорівнює 15,5%. "
                      "Ці показники залежать від припущень про резервну базу й використання покриття ОВДП. "
                      "Відношення фактичних коррахунків до розрахункових резервів саме по собі не встановлює, чи є вимога зв’язуючою для кожного банку.")
        elif t.startswith("Розрахунок - у файлі IRRBB_Stress_Test.xlsx"):
            p.text = ("Розрахунок наведено у файлі IRRBB_Stress_Test.xlsx для 59 банків із балансом на 08.2026. "
                      "У знаменнику сценаріїв використано фактичний Tier 1 зі звітності на 01.09.2026 без заміни балансовим капіталом. "
                      "Дюрації й строки переоцінки частково модельні. Результати є дослідницьким скринінгом, а не повним наглядовим тестом.")
        elif t.startswith("Медіанна найбільша втрата EVE") or t.startswith("Для узгодженої перевірки EVE"):
            p.text = (f"Для узгодженої перевірки EVE використано фактичний сукупний Tier 1 у розмірі {s.T1/1000:.2f} млрд грн. "
                      f"За лінійного шоку +200 б.п. системна договірна зміна EVE становить {s.dEVE_c_t1:.2f}%, поведінкова +{s.dEVE_b_t1:.2f}% Tier 1. "
                      "Зворотний шок змінює знак лінійної оцінки, тому висновок про ризик залежить від напрямку шоку.")
        elif t.startswith("Тест на викид EVE") or t.startswith("Серед 59 банків"):
            p.text = ("Серед 59 банків у сценарії +200 б.п. втрату понад 15% Tier 1 мають 17 банків за договірною та жоден за поведінковою оцінкою. "
                      "У поведінковій оцінці при −200 б.п. та δ=20% відповідну межу перевищують 25 банків із 59. "
                      "Ці частки стосуються заданих припущень і не є встановленими наглядовими порушеннями.")
        elif t.startswith(("Тест на викид ЧПД", "Найгірший сценарій BCBS", "За формою власності (середня зважена", "Медіанний ЧПД під ризиком за Monte-Carlo")):
            p._p.getparent().remove(p._p)
        elif t.startswith("Приклад: ПриватБанк"):
            p.text = "Чутливість EVE до дюрації коштів на вимогу"
        elif t.startswith("Історичне моделювання за всіма 6-місячними"):
            p.text = ("Таблиця показує лінійні паралельні шоки ±200 б.п. для трьох значень δ. "
                      "За меншої швидкості вибуття депозитів модельна дюрація ядра довша, тому зміна EVE сильніша. "
                      "Діапазон δ є аналізом припущень, а не статистичним довірчим інтервалом. Він не замінює 18 сценаріїв Excel-інструмента.")
        elif t.startswith("Модель коштів на вимогу - головне джерело різниці"):
            p.text = ("Модель коштів на вимогу є головним джерелом різниці оцінок. Договірний і поведінковий підходи задають різні строки переоцінки та дюрації. "
                      "Поведінкова модель узгоджується з механізмом депозитної франшизи, але наявна коротка перевірка не встановлює її прогнозної переваги.")
        elif t.startswith("Реформи передусім змінювали структуру"):
            p.text = ("Базові оцінки пов’язують резервні реформи зі зміною структури фондування й активів, проте є умовними асоціаціями. "
                      "Для строкових депозитів ФО оцінка після реформи 05.2023 змінюється з −8,93 до −2,37 логарифмічного пункту після поправки на попередній тренд, p=0,569. "
                      "Частина висновків також втрачає статистичну переконливість після поправки на множинні перевірки.")
        elif t.startswith("Доларизація."):
            p.text = (f"Доларизація. У {date} економічна різниця валютної та гривневої вартості строкових депозитів у гривневому еквіваленті становить "
                      f"{fx.loc['hh_term', 'gap_economic_ueq_pp']:.2f} в.п. для ФО та {fx.loc['corp_term', 'gap_economic_ueq_pp']:.2f} в.п. для СГ. "
                      "Графік показує, як очікувана девальвація змінює номінальну різницю. Зіставлення не враховує хеджування та саме по собі не встановлює паритету процентних ставок або причинного впливу на доларизацію.")
        elif t.startswith("Стрес-тест. Капітал - балансовий"):
            p.text = ("Стрес-тест. Використано фактичний Tier 1, але договірні строки переоцінки, опціонність і комерційні маржі спостерігаються неповно. "
                      "Результати залежать від модельних дюрацій і сценаріїв. Валютна крива USD не замінює повного набору кривих для кожної валюти банку.")
        elif t.startswith("Перед поданням дисертації перевірте кожне посилання"):
            p.text = ("Числа підрозділів 2.3 та 2.4 звірено з повторно виконаними модулями 10 і 11 та незалежними CSV перевірок. "
                      "Коефіцієнти LP і результати OU залишаються результатами попереднього прогону. Excel-файли перебудовано, але повний перерахунок їхніх формул у Microsoft Excel не виконувався.")
    for table in d.tables:
        text = " ".join(c.text for row in table.rows for c in row.cells)
        if "Найчіткіший ефект" in text:
            table.cell(0, 1).text = "Базова умовна асоціація"
        if "Кореляція з реалізованою бетою ЧПМ" in text:
            table.cell(3, 0).text = "ΔEVE при +200 б.п., % фактичного Tier 1"
            table.cell(3, 1).text = f"{s.dEVE_c_t1:.1f}"
            table.cell(3, 2).text = f"+{s.dEVE_b_t1:.1f}"
            table.cell(4, 0).text = "Повна вибірка модуля 10, γ (с.п.), 77 банків"
            for col, term in [(1, "x_con"), (2, "x_rev")]:
                v = validation.loc[validation.model.eq("(3) обидва") & validation.term.eq(term)].iloc[0]
                table.cell(4, col).text = f"{v.b:.2f} ({v.se:.2f})"
            table.cell(5, 1).text = f"{realised.nim_beta.corr(realised.CG12_mean):.2f}"
            table.cell(5, 2).text = f"{realised.nim_beta.corr(realised.RG12_mean):.2f}"
        elif any(c.text.startswith("Втрата EVE, %") for row in table.rows for c in row.cells) or "Шок і δ" in text:
            values = [["Шок і δ", "ΔEVE, % фактичного Tier 1", "Частка банків із втратою понад 15%"]]
            for row in duration.itertuples():
                values.append([f"{row.shock_bp:+d} б.п., δ={100*row.delta:.0f}%", f"{100*row.dEVE/row.tier1_mn:.1f}", f"{100*row.share_loss_above_15pct:.1f}%"])
            while len(table.rows) > len(values):
                el = table.rows[-1]._tr
                el.getparent().remove(el)
            for i, row in enumerate(values):
                for j, value in enumerate(row):
                    table.cell(i, j).text = value
    for p in d.paragraphs:
        if p.text.startswith("2.3.3. Стохастична модель"):
            p.paragraph_format.page_break_before = None
    anchor = next(p for p in d.paragraphs if p.text.startswith("2.3.3. Стохастична динаміка"))
    anchor.paragraph_format.page_break_before = True
    figure(anchor, a/"figures/S23_R5_scenarios.png", "Сценарії зміни чистого процентного доходу",
           "Рисунок 2.3-А. Шоки від −300 до +300 б.п., горизонт 24 місяці. " + source,
           "Панелі окремо показують договірну та виявлену моделі з асиметричною передачею. "
           "Сценарії використовують незмінний баланс і заданий шок ставки; вони не є прогнозними траєкторіями або довірчими інтервалами.")
    figure(anchor, a/"figures/S23_R9_nii_contributions.png", "Внески статей балансу в чутливість ЧПД",
           "Рисунок 2.3-Б. Внески за +100 б.п. за 12 місяців, млрд грн. " + source,
           f"Сума внесків дорівнює системній зміні ЧПД, {s.dNII_c/1000:.2f} млрд грн за договірною та +{s.dNII_r/1000:.2f} млрд грн за виявленою моделлю. "
           "Головна відмінність пов’язана з повільною передачею підвищення ставки у вартість коштів на вимогу. Враховано гривневі експозиції.")
    figure(anchor, a/"figures/S23_R9_nii_distribution_ownership.png", "Розподіл процентної чутливості між банками",
           f"Рисунок 2.3-В. Зміна ЧПД за +100 б.п. у відсотках річного ЧПД, {len(risk)} банків. " + source,
           "Прямокутники показують медіану та міжквартильний діапазон усередині груп власності, а точки позначають віддалені значення. "
           "Це опис розподілу банків, а не інтервали невизначеності моделі. Банк без повного додатного річного ЧПД виключено лише з цього нормованого порівняння.")
    figure(anchor, a/"figures/S23_R9_nii_eve_joint.png", "Спільна оцінка ризику доходу та вартості капіталу",
           f"Рисунок 2.3-Г. Зміна ЧПД і поведінкова зміна EVE за одного шоку +200 б.п., {len(risk)} банків. " + source,
           "Обидві осі відповідають однаковому шоку. ЧПД нормовано на додатний річний ЧПД, EVE на фактичний Tier 1. "
           "Додатне значення означає приріст; пунктир відповідає втраті 15% Tier 1 лише в цьому сценарії. "
           "Розташування банку вище цієї межі не встановлює відсутність ризику за зниження ставок чи іншої форми кривої.")
    anchor = next(p for p in d.paragraphs if p.text.startswith("Стрес-тест процентного ризику"))
    anchor.paragraph_format.page_break_before = True
    figure(anchor, b/"figures/S24_R9_product_costs.png", "Вартість фондування за продуктами та валютами",
           "Рисунок 2.4-А. Номінальна й економічна вартість депозитів. " + source,
           "Економічна вартість враховує модельну вартість резервів і збору ФГВФО. "
           "У цьому порівнянні очікувану девальвацію ще не включено. Тому валютну панель не слід безпосередньо трактувати як гривневу альтернативу фондування.")
    figure(anchor, b/"figures/S24_R9_fx_cost_gap.png", "Валютна різниця вартості фондування у часі",
           "Рисунок 2.4-Б. Валютна мінус гривнева вартість, в.п. Джерело розрахунки автора за місячною панеллю НБУ.",
           "Додатна різниця означає вищу вартість валютного ресурсу. Перехід до гривневого еквівалента використовує очікувану девальвацію та змінює висновок щодо дешевизни валюти. "
           "Це економічна вартість депозитів, а не поріг дохідності вільних коштів. Витрати хеджування не спостерігаються і не включені.", width=14)
    figure(anchor, b/"figures/S24_R9_cd_spread.png", "Спред розміщення залучених коштів у ДС",
           "Рисунок 2.4-В. Ставка ДС овернайт мінус номінальна вартість або скоригований поріг дохідності. Джерело розрахунки автора.",
           f"У {date} розрахунковий спред становить {carry.nominal_spread_pp:.2f} в.п. до коригувань і {carry.adjusted_spread_pp:.2f} в.п. після них. "
           "Це зіставлення ставок за модельними припущеннями. Операційні витрати, податки й вартість капіталу не включено, тому показник не є фактичним прибутком банку.")
    d.save(path)

    path = root / "docs/Thesis_code_review.docx"
    d = Document(path)
    styles(d)
    for p in d.paragraphs:
        if p.text.startswith("Незалежний Python-модуль 15 виконано"):
            p.text = scope + " Формули Excel записано, але їх повний перерахунок у Microsoft Excel не виконувався."
        elif p.text.startswith("Відтворюваність. Додано"):
            p.text = "Відтворюваність. R-модулі 10 і 11 виконано на фактичній панелі, Python-перевірки та обидва Excel-інструменти перебудовано. Повний драйвер і решта R-моделей не виконувалися повторно."
        elif p.text.startswith("Відсутні очікування як нуль."):
            p.text = "Відсутні очікування як нуль. Модуль 11 виконано повторно без підміни відсутньої девальвації нулем. Числові виходи S24 перебудовано."
        elif p.text.startswith("На дату балансу"):
            p.text = p.text.replace("успадкованого лінійного", "повторно розрахованого лінійного")
    anchor = next(p for p in d.paragraphs if p.text.startswith("Оновлення капіталу та ліквідності"))
    insert(anchor, "Оновлення графіків підрозділів 2.3 та 2.4", "Heading")
    insert(anchor, "Виправлено спільний чорно-білий стиль. Він підміняв типи ліній рівнями змінної кольору навіть тоді, коли ці змінні відрізнялися. Через це всі сценарні криві ставали невидимими. Виявлено і виправлено також приховування перших двох груп точок через порожні форми маркерів.")
    insert(anchor, "Сценарний графік відновлено у двох панелях. Додано по три рисунки до 2.3 та 2.4 з окремими таблицями вихідних значень, PNG, PDF і SVG. Суми внесків ЧПД звірено з системним підсумком; осі спільного графіка ЧПД та EVE використовують один шок +200 б.п. Застарілі EMF змінених рисунків вилучено.")
    insert(anchor, "R-тести перевіряють незалежні змінні кольору, типу лінії та форми точки. Повторний розрахунок залишив сценарні значення й базові показники S24 незмінними. 58 банків мають придатний додатний річний ЧПД для нормування, 59 мають фактичний Tier 1. Пропуски не замінено нулем.")
    d.save(path)
    (root/"review/chart_documents.json").write_text(json.dumps({
        "section23_new_figures": 3, "section24_new_figures": 3, "scenario_figure_repaired": True,
        "source": "R modules 10 and 11 rerun; upstream LP coefficients inherited",
        "documents": ["section2_reviewed.docx", "Thesis_code_review.docx"]}, indent=2))
    print("Updated section 2.3/2.4 figures in both reviewed documents")


if __name__ == "__main__":
    update()
