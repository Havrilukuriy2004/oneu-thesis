from pathlib import Path
from copy import deepcopy
import re,subprocess,json
import pandas as pd
from docx import Document
from docx.shared import Inches,Pt,RGBColor,Cm
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
ROOT=Path(__file__).resolve().parents[1];OUT=ROOT/'docs';OUT.mkdir(exist_ok=True);REV=ROOT/'review'
V=pd.read_csv(REV/'R23_validation.csv');E=pd.read_csv(REV/'R24_reforms_robustness.csv');D=pd.read_csv(REV/'R23_duration_sensitivity_system.csv')
# These statements qualify inherited estimates rather than silently deleting inconvenient results.
FIXES=[
('2.1','Змінний склад банків','Додано помісячне покриття та перевірку на банках, що дожили до кінця вибірки','Остання група є ретроспективно відібраною; це перевірка чутливості, а не виправлення відбору'),
('2.1','Календарні лаги','Лаги у нових перевірках визначено за календарем; у панелі немає внутрішніх розривів','Це не перевіряє правильність усіх вихідних форм НБУ'),
('2.1','Назви банків','Прибрано вкладені лапки у представленнях та Excel; коди НБУ збережено','Сирі назви збережено в джерелах'),
('2.1','Ціна та структура','Розклади первісного пакета збережено як історичні результати','Не виконано повний повторний розрахунок усіх R-моделей'),
('2.2','Ендогенність облікової ставки','Уточнено, що LP та довгі різниці є умовними реакціями; додано DK12 і поділ періодів','Немає зовнішнього інструмента або високочастотних монетарних сюрпризів'),
('2.2','Попит і пропозиція кредиту','Знято твердження, що агреговані BLS-контролі повністю розділяють ці механізми','Потрібні кредитні зв’язки банк–позичальник або окремий інструмент попиту'),
('2.2','Державні банки','Повторено довгі різниці, валідацію розриву та оцінки реформ без державних банків','Не усуває вплив державної політики на весь ринок'),
('2.2','Режими та опуклість','Додано довоєнні та воєнні оцінки; збережено невизначеність від одного циклу ставок','Екстраполяція поза історичною підтримкою не є підтвердженим прогнозом'),
('2.3','Знаменник валідації','У предикторі використано лаговані процентні активи, як у знаменнику ЧПМ','Коефіцієнт близький до одиниці сам собою не доводить структурну модель'),
('2.3','Підглядання в майбутнє','Додано expanding-window оцінку коефіцієнтів лише за минулими датами','Використано остаточну версію даних; це псевдопрогноз, а не перевірка в реальному часі'),
('2.3','Довжина перевірки прогнозу','Порівняно моделі на тій самій доступній вибірці','У строгому варіанті лише 9 місяців; прогнозна перевага не встановлена'),
('2.3','Дюрація коштів на вимогу','Перераховано +200 і −200 б.п. для δ 10%, 20%, 30%; додано фактичні зміни депозитів 2022 р.','Сценарний діапазон не є довірчим інтервалом; FX-обсяги у гривні містять переоцінку'),
('2.3','Приріст EVE як порушення','Для фіксованого сценарію поріг застосовано до втрати, а не до модуля зміни EVE','За різних напрямків шоку окремо перевіряються їхні втрати'),
('2.3','Договірні строки','Збережено чітку позначку наближення строків переоцінки через λ','Потрібні договірні дані, щоб замінити це наближення'),
('2.3','ROE замість лише ЧПМ','Повторено валідацію на зміні річного темпу прибутку до балансового капіталу','ROE включає податки, резерви та переоцінки; не є чистим процентним ефектом'),
('OU','Частота рідкісних стрибків','Додано пороги 2, 3, 5 в.п. і різні початки вибірки','Інтервали залежать від вибраного апріорного розподілу Gamma(.5, 1 рік)'),
('OU','Переломи параметрів','Додано sup-F сканування AR(1) з блочним бутстрепом','Це діагностика сталості, не встановлення структурного режиму'),
('OU','Малі банки і похибка θ','Збережено вже реалізоване звуження до групи та бутстреп у моделі 14','Похибка оцінки θ не зникає; новий повний вкладений бутстреп не виконано'),
('OU','Симетричні хвости','Збережено t-шоки, стрибки та альтернативний кроковий процес первісної моделі','Асиметрія хвостів не оцінена окремо; не можна вважати обмеження усунутим'),
('OU','Очікування та слабкі інструменти','Збережено групи агентів та наявні IV-діагностики; прибрано структурну інтерпретацію','Власні лаги очікувань не гарантують валідності інструментів'),
('OU','Динамічний баланс','Збережено симуляцію обсягів і потреби у фондуванні первісної моделі','Ендогенний вибір активів і кредитний попит не змодельовано'),
('OU','Валютна крива','Збережено окрему валютну компоненту в сценаріях стрес-тесту','Повної спільної стохастичної моделі гривневої та валютної кривих немає'),
('2.4','Передтренди реформ','Додано попередньо оцінені тренди банків та оцінки без держбанків','Доданий тренд не доводить паралельність контрфактичних трендів'),
('2.4','Множинні перевірки','Додано BH-корекцію для 9 результатів у межах кожної реформи та специфікації','Мала кількість часових спостережень і перетин реформ залишаються'),
('2.4','Відсутні очікування як нуль','У R-коді припинено підміну відсутньої девальвації нулем','Історичні числові таблиці S24 з попереднього R-прогону не перезаписано'),
('2.4','База резервування','Додано чутливість до частки коротких депозитів та використання покриття ОВДП','Немає фактичної резервної бази й залишків лише придатних бенчмарк-ОВДП'),
('2.4','Валютна економічна вартість','Додано сценарії очікуваної девальвації 0%, 5%, 15%, 30%','Непокрита валютна позиція і хеджоване фондування мають різну ціну'),
('IRRBB','Tier 1 та балансовий капітал','Excel зберігає поле ручного Tier 1; текст визначає результати як скринінг','Без наглядового Tier 1 не можна робити висновок про офіційне порушення'),
('IRRBB','Стандарти BCBS та EBA','Збережено сценарії й ліміти з окремою позначкою авторської UAH-калібрації','Офіційна застосовність до конкретного банку потребує перевірки; USD не замінює EUR'),
('Усі','Відтворюваність','Додано офлайн-запуск, залежності, CI, контроль ключів і походження виходів','R не встановлено у поточному середовищі; зміни R пройшли огляд коду, не виконання'),
('Усі','Формули на графіках','12 рисунків оновлено з векторним математичним набором; у R додано plotmath','Застарілі EMF змінених рисунків прибрано; використовуйте SVG або PDF'),
]
pd.DataFrame(FIXES,columns=['section','limitation','treatment','remaining']).to_csv(REV/'limitations_register.csv',index=False,encoding='utf-8-sig')
# Standalone publication figure, with t intervals respecting the 9-month validation span.
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':10,'mathtext.fontset':'stix'})
fig,ax=plt.subplots(figsize=(8,3.3));z=V.query("outcome=='dy_nim' and sample=='all'")
labels=[];est=[];err=[]
from scipy.stats import t
for spec,term,label in [('baseline','x_rg','Повна вибірка'),('baseline_on_past_sample','x_rg','Повна оцінка β на короткій вибірці'),('past_only_common_sample','x_rg_past','β лише за минулими даними')]:
 r=z.query('spec==@spec and term==@term').iloc[0];labels.append(label);est.append(r.b);err.append(t.ppf(.975,min(r.banks,r.months)-1)*r.se_two_way)
ax.errorbar(est,[2,1,0],xerr=err,fmt='o',color='black',capsize=4);ax.axvline(0,color='.6',lw=.6);ax.set_yticks([2,1,0],labels);ax.set_xlabel('Коефіцієнт при виявленому розриві, 95% інтервал');ax.spines[['top','right']].set_visible(False);fig.tight_layout();fig.savefig(REV/'validation_comparison.png',dpi=220);fig.savefig(REV/'validation_comparison.pdf');plt.close(fig)

def style(doc):
 for sec in doc.sections:
  sec.top_margin=Cm(2);sec.bottom_margin=Cm(2);sec.left_margin=Cm(2.2);sec.right_margin=Cm(2.2)
 for nm in ['Normal','Title','Heading 1','Heading 2','Heading 3']:
  st=doc.styles[nm];st.font.name='Times New Roman';st.font.color.rgb=RGBColor(0,0,0);st.font.size=Pt(11 if nm=='Normal' else 16 if nm=='Title' else 13)
  st.paragraph_format.space_after=Pt(6)
  for el in list(st.element.xpath('.//w:pBdr')):el.getparent().remove(el)
  fonts=st.element.get_or_add_rPr().find(qn('w:rFonts'))
  if fonts is not None:
   for a in ['asciiTheme','hAnsiTheme','eastAsiaTheme','cstheme']:
    fonts.attrib.pop(qn('w:'+a),None)
   fonts.set(qn('w:ascii'),'Times New Roman');fonts.set(qn('w:hAnsi'),'Times New Roman')
 doc.styles['Normal'].paragraph_format.line_spacing=1.1

def table(doc,heads,rows,widths=None):
 tb=doc.add_table(rows=1,cols=len(heads));tb.style='Table Grid'
 for c,t in zip(tb.rows[0].cells,heads):c.text=t
 for c in tb.rows[0].cells:
  for r in c.paragraphs[0].runs:r.bold=True
  sh=OxmlElement('w:shd');sh.set(qn('w:fill'),'EAEAEA');c._tc.get_or_add_tcPr().append(sh)
 hdr=OxmlElement('w:tblHeader');tb.rows[0]._tr.get_or_add_trPr().append(hdr)
 for row in rows:
  cells=tb.add_row().cells
  for c,t in zip(cells,row):c.text=str(t)
 for row in tb.rows:
  pr=row._tr.get_or_add_trPr();pr.append(OxmlElement('w:cantSplit'))
 return tb

report=Document();style(report)
report.add_paragraph('Перевірка коду та результатів дипломної роботи',style='Title')
report.add_paragraph('Юрій Гаврилюк • 5 листопада 2026 року')
report.add_paragraph('Охоплення: наданий код розділу 2, підрозділи 2.1–2.4, стохастична модель депозитів та інструменти IRRBB. Панель містить 7 206 банко-місяців, 85 банків, останній місяць балансів 08.2026, на цю дату 59 банків.')
report.add_paragraph('Висновок перевірки',style='Heading 1')
report.add_paragraph('Пакет отримав перевірки стійкості, виправлення знака тесту EVE, узгодження знаменника у валідації ЧПМ, очищення назв банків і математичне оформлення графіків. Висновки про прогнозну перевагу розриву та причинні ефекти резервних реформ потребують стриманішого формулювання. Наявні дані не дозволяють усунути всі структурні обмеження.')
report.add_paragraph('Що змінює інтерпретацію результатів',style='Heading 1')
rows=[]
for sp,term,lab in [('baseline','x_rg','Повна вибірка'),('past_only_common_sample','x_rg_past','Лише минулі дані')]:
 r=z.query('spec==@sp and term==@term').iloc[0];rows.append((lab,f'{r.b:.3f}',f'{r.se_two_way:.3f}',f'{r.p:.3g}',int(r.n),int(r.months)))
table(report,['Оцінка','γ','С.п.','p','N','Міс.'],rows)
report.add_paragraph('Нова валідація використовує однаковий знаменник, середні процентні активи, у ЧПМ і предикторі. На відміну від первісної таблиці, у незалежному розрахунку не відкинуто 1% крайніх змін ЧПМ. Тому нові коефіцієнти не є точним повторенням первісних 1,03 та −0,18. Коефіцієнт договірного розриву у новій повній вибірці −0,224, p=0,080.')
report.add_picture(str(REV/'validation_comparison.png'),width=Inches(6.45))
report.add_paragraph('Рисунок 1. Валідація виявленого розриву. Інтервали враховують малу кількість місяців. Дев’ять місяців псевдопрогнозу не встановлюють переваги моделі. Остаточно переглянуті дані також не є справжніми історичними вінтажами.')
report.add_paragraph('Резервні реформи',style='Heading 1')
report.add_paragraph('Для реформи 05.2023 зв’язок зі строковими депозитами ФО становить −8,93 логарифмічних пункту без тренду, але лише −2,37 після попередньо оціненого тренду, p=0,569. Логарифмічні пункти не тотожні точному відсотковому ефекту. Для ОВДП у 10.2024 базовий коефіцієнт −16,45, після BH-корекції p=0,297; за трендом −24,68, p BH=0,240. Від’ємний знак залишається, але його не слід пояснювати як встановлений наслідок реформи.')
report.add_paragraph('Дюрація та знак процентного ризику',style='Heading 1')
rows=[]
for r in D.query('shock_bp==-200').itertuples():rows.append((f'{100*r.delta:.0f}%',f'{r.dEVE/1000:.1f}',f'{100*r.share_loss_above_15pct:.1f}%'))
table(report,['δ на рік','ΔEVE, млрд грн','Частка банків із втратою >15%'],rows)
report.add_paragraph('Це лінійне наближення поведінкової дюрації при −200 б.п., зі знаменником балансового капіталу. Частки не є результатом повного наглядового тесту BCBS. Діапазон δ є аналізом припущень, а не статистичним довірчим інтервалом. При +200 б.п. у цій моделі система отримує приріст EVE, який не можна позначати як порушення порога втрат.')
report.add_paragraph('Формули та назви',style='Heading 1')
report.add_picture(str(ROOT/'output_nbu/ou_dynamics/figures/OU_F10_vasicek_decay.png'),width=Inches(6.3))
report.add_paragraph('Рисунок 2. Формула Васічека набрана векторною математикою. Оновлено 12 рисунків у PNG, PDF і SVG. Математичні підписи не залежать від підтримки Unicode-індексів у конкретному шрифті. У назвах банків прибрано вкладені лапки, коди НБУ залишаються ключами.')
report.add_paragraph('Відтворення та межі перевірки',style='Heading 1')
report.add_paragraph('Незалежний Python-модуль 15 виконано на наданих Parquet-файлах. Обидва Excel-інструменти перебудовано. Формули Excel записано у файли; повний перерахунок усіх клітинок у Microsoft Excel не виконано. R відсутній у поточному середовищі, тому повний run_all_v6.R після виправлень не виконувався. Первісні числові результати output_nbu позначено як успадковані; авторитетне джерело нових перевірок, каталог review. Зміни R потребують виконання перед остаточним поданням.')
report.add_paragraph('Брак Tier 1, договірних строків, фактичної резервної бази та валідного зовнішнього монетарного інструмента не можна виправити лише програмним кодом. Нижче для кожного обмеження окремо зазначено виконану дію та залишкову невизначеність.')
report.add_paragraph('Реєстр обмежень і виконаних змін',style='Heading 1')
for section in dict.fromkeys(x[0] for x in FIXES):
 report.add_paragraph(section,style='Heading 2')
 for _,lim,treat,remain in [x for x in FIXES if x[0]==section]:
  p=report.add_paragraph();p.add_run(lim+'. ').bold=True;p.add_run(treat+'. '+remain+'.')
report.add_paragraph('Джерела та межі літературної перевірки',style='Heading 1')
report.add_paragraph('SciSpace: Drechsler, Savov, Schnabl (2021), Banking on Deposits: Maturity Transformation without Interest Rate Risk. Запис і анотація підтверджують зв’язок депозитної франшизи з процентною чутливістю, але не доводять валідність українських оцінок. https://scispace.com/papers/banking-on-deposits-maturity-transformation-without-interest-3eyaq4i4q5')
report.add_paragraph('BCBS (2024), Recalibration of shocks for interest rate risk in the banking book. Перевірено зміну квантиля до 99,9%, шестимісячні зміни, округлення до 25 б.п. та дату впровадження 01.01.2026. Авторська гривнева калібрація не стає офіційним нормативом лише через подібність методу. https://www.bis.org/publications/202407-standards-recalibration-shocks-interest-rate-risk-banking-book')
report.add_paragraph('Bigdata.com: ProCredit Holding, Q1 2023 earnings call, 15.05.2023. Менеджмент описує швидше переоцінювання активів, ніж зобов’язань, і можливе подальше звуження маржі; це контекст групи ProCredit, а не вимірювання ефекту резервної реформи НБУ. https://app.bigdata.com/documents/DD6CE9FE7F02180D6DB5AAC30F945373')
report.add_paragraph('Інші бібліографічні записи первісного огляду не пройшли суцільної перевірки за повними текстами. Числа у цьому звіті походять з review/*.csv, а не із зовнішніх новин.')
# Replace long raw URLs with descriptive hyperlinks, keeping exact destinations.
from docx.opc.constants import RELATIONSHIP_TYPE as RT
for paragraph in report.paragraphs:
 match=re.search(r'https://\S+',paragraph.text)
 if match:
  url=match.group();paragraph.text=paragraph.text[:match.start()].rstrip()+' '
  link=OxmlElement('w:hyperlink');link.set(qn('r:id'),paragraph.part.relate_to(url,RT.HYPERLINK,is_external=True))
  run=OxmlElement('w:r');text=OxmlElement('w:t');text.text='Переглянути джерело';run.append(text);link.append(run);paragraph._p.append(link)
report.save(OUT/'Thesis_code_review.docx')
# Markdown companion provides exact code/data locations for repository reviewers.
md=['# Review of ONEU thesis code','','Status: needs revision before final submission. Numerical supplement completed; full R execution unavailable.','', '## New validation', '',z.to_csv(index=False),'','## Limitations register','',pd.DataFrame(FIXES,columns=['Section','Limitation','Treatment','Remaining']).to_csv(index=False)]
(ROOT/'REVIEW.md').write_text('\n'.join(md))

# Native editable Word equations via Pandoc, while preserving the supplied document structure.
from docx.text.paragraph import Paragraph
bankmap=pd.read_parquet(ROOT/'data_nbu/parquet/mart_bank_month_v6.parquet').fullname.dropna().unique()
def cleaned(t):
 for b in sorted(bankmap,key=len,reverse=True):t=t.replace(b,re.sub(r'["«»„“”]','',b))
 return t.replace('—','-')
math_count=0
for i,(pattern,name) in enumerate([('Розділ 2*','section2_reviewed.docx'),('Підрозділ 2.1*','section2_1_reviewed.docx'),('Огляд літератури*','literature_reviewed.docx')]):
 f=ROOT/'docs/source'/['section2_original.docx','section2_1_original.docx','literature_original.docx'][i];d=Document(f);style(d)
 paragraphs=list(d.paragraphs)+[p for tb in d.tables for row in tb.rows for c in row.cells for p in c.paragraphs]
 for p in paragraphs:
  text=p.text;new=cleaned(text)
  if new!=text:
   for run in p.runs:run.text=cleaned(run.text)
  if len(text)<1400 and '=' in text and text.count('\\')>=2 and not re.search('[А-Яа-яІіЇїЄєҐґ]',re.sub(r'\\text\{[^}]*\}','',text)):
   # Split long display equations at explicit spacing so Word cannot clip the line.
   chunks=re.split(r'\\qquad\s*,?\s*',text)
   for j,eq in enumerate(chunks):
    if not eq.strip():continue
    temp=REV/'equation.md';temp.write_text('$$\n'+eq.strip()+'\n$$');res=REV/'equation.docx'
    subprocess.run(['pandoc',str(temp),'-o',str(res)],check=True,capture_output=True)
    math=Document(res).element.xpath('//m:oMath')
    if not math:continue
    target=p if j==0 else Paragraph(OxmlElement('w:p'),p._parent)
    if j==0:
     for child in list(p._p):
      if child.tag!=qn('w:pPr'):p._p.remove(child)
    else:p._p.addnext(target._p)
    target.alignment=WD_ALIGN_PARAGRAPH.CENTER;target._p.append(deepcopy(math[0]));math_count+=1
 # A leading correction notice makes inherited numbers distinguishable.
 first=d.paragraphs[0]
 note=first.insert_paragraph_before('Редакція після перевірки 05 листопада 2026 року',style='Heading 1')
 body_note=note.insert_paragraph_before('Таблиці нижче містять результати первісного R-прогону. Нові перевірки у Thesis_code_review.docx та review/*.csv уточнюють їхню інтерпретацію. Перевага виявленого розриву поза вибіркою не встановлена; оцінки резервних реформ є умовними асоціаціями; стрес-тест із балансовим капіталом є дослідницьким скринінгом. Повний повторний R-прогін після змін не виконано.')
 body_note._p.addprevious(first._p);first._p.addnext(note._p)
 # Replace the most consequential unsupported sentences in the original body.
 for p in d.paragraphs:
  t=p.text
  if t.startswith('Література дає три перевірювані твердження'):
   p.text=t.replace('і дані НБУ підтверджують усі три','і надані оцінки узгоджуються з відповідними механізмами, проте не встановлюють їх причинно')
  if 'Новóго коду' in t or 'Нового коду для 2.2 не потрібно' in t:p.text=t.replace('Нового коду для 2.2 не потрібно.','Додано незалежні перевірки у модулі 15; структурна ідентифікація залишається відкритою.')
  if 'виявлений розрив прогнозує' in t.lower() or t.startswith('Валідація.'):
   p.text=t+' Новий expanding-window тест має лише 9 місяців, γ=0,351, с.п. 0,808, p=0,675; попередня валідація на повній вибірці не доводить прогнозної переваги.'
  if '2026-12 (санкційні обмеження)' in t:p.text=t.replace(', 2026-12 (санкційні обмеження)','; події після 05.11.2026 не належать до реалізованих природних експериментів')
  if t.startswith('Рішення НБУ майже не передбачувані') or 'Рішення НБУ майже не передбачувані' in t:p.text=t.replace('Рішення НБУ майже не передбачувані з публічної інформації','Наявна регресія не знаходить статистично переконливої передбачуваності рішень у використаному наборі змінних')
 d.add_paragraph('Результати нових перевірок',style='Heading 1')
 d.add_paragraph('Узгодження знаменників дало γ=0,935 (с.п. 0,145) для виявленого розриву у повній вибірці; у строгому псевдопрогнозі γ=0,351 (0,808), p=0,675. Для строкових депозитів ФО після реформи 05.2023 оцінка змінюється з −8,93 до −2,37 логарифмічних пункту після поправки на попередній тренд. Ці результати не підтримують безумовних причинних або прогнозних висновків.')
 d.save(OUT/name)
(REV/'document_edits.json').write_text(json.dumps({'native_equations':math_count,'outputs':[p.name for p in OUT.glob('*.docx')]},indent=2))
print('Documents built; native equations',math_count)
