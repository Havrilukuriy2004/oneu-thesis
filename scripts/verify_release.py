"""Integrity checks on actual release inputs/outputs; no synthetic banking data."""
from pathlib import Path
import json,hashlib,sys
import numpy as np
import pandas as pd
from openpyxl import load_workbook
ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT))
from bank_names import clean_bank_name
from capital_inputs import attach, ensure
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
cap=ensure(ROOT)
check('prudential keys unique',not cap.duplicated(['nkb','month']).any())
check('capital dates are next month reports',(cap.report_date==cap.month+pd.offsets.MonthBegin(1)).all())
check('legacy core is never relabelled Tier 1',cap.loc[cap.capital_regime.eq('legacy'),'tier1_mn'].isna().all())
check('wartime missing reports remain missing',not cap.report_date.isin(pd.to_datetime(['2022-03-01','2022-04-01','2022-05-01'])).any())
check('2018 compact code-name worksheets retained',set(pd.to_datetime(['2018-04-01','2018-05-01','2018-06-01'])).issubset(set(cap.month)))
check('source conflicts excluded from headroom',cap.loc[cap.capital_source_conflict,['headroom_total_min_pp','headroom_tier1_min_pp','headroom_cet1_min_pp']].isna().all().all())
check('minimum transition dates use economic month',all(np.allclose(cap.loc[cap.month.eq(m),'minimum_total_pct'],v) for m,v in [('2024-12-01',8.5),('2025-01-01',9.25),('2025-06-01',9.25),('2025-07-01',10.)]))
joined=attach(B,ROOT);latest_cap=joined.loc[joined.month.eq(joined.month.max())].set_index('nkb')
check('capital join preserves base observations',len(joined)==len(B))
check('all latest banks have positive observed Tier 1',latest_cap.tier1_mn.gt(0).all() and len(latest_cap)==59)
# Verify an independently readable source observation including the thousand/million conversion.
check('Ukreximbank exact-date Tier 1 and units',np.isclose(latest_cap.loc['2','tier1_mn'],17751.59696266))
check('duration screen uses positive Tier 1',D.tier1_mn.gt(0).all() and np.allclose(D.eve_pct_tier1,100*D.dEVE/D.tier1_mn))
check('loss flags match Tier 1 threshold',np.array_equal(D.loss_exceeds_15pct.to_numpy(),(D.dEVE<-.15*D.tier1_mn).to_numpy()))
CV=pd.read_csv(ROOT/'review/R23_capital_controlled_validation.csv')
check('controlled validation uses a common sample',all(g.n.nunique()==1 and g.months.nunique()==1 for _,g in CV.groupby('spec')))
CI=pd.read_csv(ROOT/'review/R22_capital_liquidity_interactions.csv')
valid=CI.dropna(subset=['p','p_BH'])
check('capital interaction multiplicity correction is conservative',(valid.p_BH+1e-12>=valid.p).all())
check('unavailable interaction inference is explicitly labelled',CI.loc[CI.se_two_way.isna(),'inference_status'].eq('unavailable_nonpositive_cluster_variance').all())
for p in (ROOT/'output_nbu/bank_excel').glob('*.xlsx'):
 w=load_workbook(p,data_only=False,read_only=True);ws=w['Д1'];headers=[c.value for c in ws[3]]
 t1col=headers.index('T1')
 for row in list(ws.iter_rows(min_row=5,max_row=63,values_only=True)):
  check(p.name+' Tier 1 bank '+str(row[0]),np.isclose(row[t1col],latest_cap.loc[str(row[0]),'tier1_mn']))
 w.close()
(ROOT/'review/release_checks.json').write_text(json.dumps(checks,indent=2))
print(len(checks),'release checks passed')
