"""Integrity checks on actual release inputs/outputs; no synthetic banking data."""
from pathlib import Path
import json,hashlib,sys
import numpy as np
import pandas as pd
from openpyxl import load_workbook
ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT))
from bank_names import clean_bank_name
checks=[]
def check(name,ok):
 checks.append({'check':name,'passed':bool(ok)})
 if not ok:raise AssertionError(name)
B=pd.read_parquet(ROOT/'data_nbu/parquet/mart_bank_month_v6.parquet')
check('unique bank-month source keys',not B.duplicated(['nkb','month']).any())
last=B.loc[B.month.eq(B.month.max())]
check('cleaned names preserve unique bank choices',last.fullname.map(clean_bank_name).nunique()==last.nkb.nunique())
manifest=json.loads((ROOT/'review/validation_manifest.json').read_text())
check('snapshot hashes match reviewed inputs',all(hashlib.sha256((ROOT/'data_nbu/parquet'/name).read_bytes()).hexdigest()==sha for name,sha in manifest['input_sha256'].items()))
V=pd.read_csv(ROOT/'review/R23_validation.csv')
q=V.query("outcome=='dy_nim' and sample=='all'")
a=q.query("spec=='past_only_common_sample'");b=q.query("spec=='baseline_on_past_sample'")
check('past-only and comparison use identical sample size',set(a.n)==set(b.n) and set(a.months)==set(b.months))
E=pd.read_csv(ROOT/'review/R24_reforms_robustness.csv')
check('BH adjusted p never below raw p',((E.p_BH+1e-12)>=E.p).all())
D=pd.read_csv(ROOT/'review/R23_duration_sensitivity_banks.csv')
check('loss flags never mark an EVE gain',not D.loc[D.dEVE.ge(0),'loss_exceeds_15pct'].any())
check('opposite parallel shocks reverse linear EVE',np.allclose(D.query('shock_bp==200').dEVE.to_numpy(),-D.query('shock_bp==-200').dEVE.to_numpy()))
for p in (ROOT/'output_nbu/bank_excel').glob('*.xlsx'):
 w=load_workbook(p,data_only=False,read_only=True)
 check(p.name+' contains formulas',any(c.data_type=='f' for s in w for row in s for c in row))
 # No editor error values are present; formula evaluation is a separate Excel task.
 check(p.name+' contains no literal Excel errors',not any(c.data_type=='e' for s in w for row in s for c in row))
 w.close()
from PIL import Image
for path in (ROOT/'output_nbu').glob('**/figures/*.png'):
 with Image.open(path) as img:img.load()
check('all raster figures decode completely',True)
math=json.loads((ROOT/'review/math_rendering.json').read_text())
for r in math:
 p=ROOT/r['file'];f=p.parent.parent
 check(p.stem+' has matching raster and vector files',p.is_file() and (f/(p.stem+'.png')).is_file() and (f/'pdf'/(p.stem+'.pdf')).is_file())
(ROOT/'review/release_checks.json').write_text(json.dumps(checks,indent=2))
print(len(checks),'release checks passed')
