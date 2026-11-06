"""Keep data marks unchanged; replace exact known mathematical labels by vector TeX.
SVG is the source of truth, PNG/PDF are regenerated from that same SVG.
"""
from pathlib import Path
import re,io,json,copy,sys
from lxml import etree as E
import matplotlib
matplotlib.use('Agg')
matplotlib.rcParams['mathtext.fontset']='stix'
from matplotlib.mathtext import math_to_image
from matplotlib.font_manager import FontProperties
from bank_names import clean_bank_name
import cairosvg
ROOT=Path(sys.argv[1]) if len(sys.argv)>1 else Path(__file__).resolve().parents[1]
NS='http://www.w3.org/2000/svg'
MAP={
 'β12':r'\beta_{12}','β0, в.п. на 1 в.п.':r'\beta_0',
 'Рухоме середнє OU μ(π^e)':r'\mu(\pi^e)',
 'Рухома ціль OU μ(i, π^e, E[Δs])':r'\mu(i,\pi^e,\mathrm{E}[\Delta s])',
 'Контроль очікувань (π^e, Δs^e)':r'\pi^e,\ \Delta s^e',

}
EQ={
 'D5_duration_nmd':r'D(i)=\frac{1}{i+\delta}-\frac{\beta(i)}{r(i)+\delta}',
 'D1_convexity_betas':r'\beta(i_0)=\gamma_1+\gamma_2 i_0',
 'OU_F10_vasicek_decay':r'\Delta R(\tau)=\Delta i\,\frac{1-e^{-\kappa\tau}}{\kappa\tau}',
 'S24_R3_nominal_vs_shadow':r'EC=r+\rho_{cash}i+\rho_{bond}(i-y_b)+f',
 'S23_R3_system_gap':r'RG_h=\frac{\sum_{a}A_a\beta_{a,h}-\sum_{l}L_l\beta_{l,h}}{TA}',
}
CACHE={};counter=0

def vector_math(tex,size=9):
 global counter
 key=(tex,size)
 if key not in CACHE:
  buf=io.BytesIO();math_to_image('$'+tex+'$',buf,prop=FontProperties(family='STIXGeneral',size=size),format='svg',color='black')
  el=E.fromstring(buf.getvalue());CACHE[key]=el
 el=copy.deepcopy(CACHE[key]);counter+=1
 for g in list(el.iter('{'+NS+'}g')):
  if g.get('id','').startswith('patch_'):g.getparent().remove(g)
 for path in el.iter('{'+NS+'}path'):path.set('style','fill:#000000;stroke:none')
 for style in list(el.iter('{'+NS+'}style')):style.getparent().remove(style)
 # Matplotlib references ids locally; make all ids unique within the final plot.
 ids={x.get('id'):f'math{counter}_{x.get("id")}' for x in el.iter() if x.get('id')}
 for x in el.iter():
  if x.get('id') in ids:x.set('id',ids[x.get('id')])
  for k,v in list(x.attrib.items()):
   for old,new in ids.items():v=v.replace('#'+old,'#'+new)
   x.set(k,v)
 return el

def replacement(s):
 if s in MAP:return MAP[s]
 if re.fullmatch(r'β = -?\d+\.\d+',s):return r'\beta='+s.split('=')[1].strip()
 if re.fullmatch(r'δ = \d+%',s):return r'\delta='+s.split('=')[1].strip().replace('%',r'\%')
 return None

def main():
 rows=[]
 for p in sorted((ROOT/'output_nbu').glob('**/svg/*.svg')):
  source=ROOT/'review/figure_source'/p.relative_to(ROOT/'output_nbu')
  if not source.exists():
   source.parent.mkdir(parents=True,exist_ok=True);source.write_bytes(p.read_bytes())
  tree=E.parse(str(source));r=tree.getroot();changed=0
  for el in list(r.iter('{'+NS+'}text')):
   s=''.join(el.itertext());tex=replacement(s)
   if s in ('corp_dem','hh_dem'):
    el.text='СГ' if s=='corp_dem' else 'ФО';changed+=1;continue
   if tex:
    m=vector_math(tex);vb=list(map(float,m.get('viewBox').split()));w,h=vb[2:];x=float(el.get('x','0'));y=float(el.get('y','0'))
    anchor=el.get('text-anchor','start');x-=w/2 if anchor=='middle' else w if anchor=='end' else 0
    m.set('x',str(x));m.set('y',str(y-h*.85));m.set('width',str(w));m.set('height',str(h));m.set('overflow','visible')
    if el.get('transform'):m.set('transform',el.get('transform'))
    el.getparent().replace(el,m);changed+=1
   elif any(q in s for q in ['"','«','»','“','”']) and (any(k in s.lower() for k in ['банк','bank','пумб']) or re.match(r'^(АТ|АБ|ПАТ|ПуАТ|АКБ) ',s)):
    el.text=clean_bank_name(s);changed+=1
  if p.stem in EQ:
   # A dedicated equation band keeps formulas clear of the data and legends.
   vb=list(map(float,r.get('viewBox').split()));w,h=vb[2:];m=vector_math(EQ[p.stem],10);mv=list(map(float,m.get('viewBox').split()));mh=mv[3];mw=mv[2]
   m.set('x',str((w-mw)/2));m.set('y',str(h+7));m.set('width',str(mw));m.set('height',str(mh));m.set('overflow','visible');r.append(m)
   newh=h+mh+15;r.set('viewBox',f'0 0 {w} {newh}');r.set('height',f'{newh}pt');changed+=1
  if changed:
   tree.write(str(p),encoding='utf-8',xml_declaration=True)
   fig=p.parent.parent;png=fig/(p.stem+'.png');pdf=fig/'pdf'/(p.stem+'.pdf')
   png.write_bytes(cairosvg.svg2png(url=str(p),scale=600/72))
   pdf.write_bytes(cairosvg.svg2pdf(url=str(p)))
   # Remove only stale EMF for this changed figure; SVG/PDF are vector replacements.
   emf=fig/'emf'/(p.stem+'.emf')
   if emf.exists():emf.unlink()
   rows.append(dict(file=str(p.relative_to(ROOT)),changes=changed))
 (ROOT/'review/math_rendering.json').write_text(json.dumps(rows,ensure_ascii=False,indent=2));print(len(rows),'figures updated')
if __name__=='__main__':main()
