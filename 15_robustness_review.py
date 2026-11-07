"""Offline, independently computed checks for sections 2.1–2.4.
No raw data updates. Inference is conditional on generated regressors.
Run: python 15_robustness_review.py [project_dir]
"""
from pathlib import Path
import sys,json,hashlib
import numpy as np
import pandas as pd
from scipy import stats
from bank_names import clean_bank_name
from capital_inputs import attach
ROOT=Path(sys.argv[1]) if len(sys.argv)>1 else Path(__file__).resolve().parent
OUT=ROOT/'review'; OUT.mkdir(exist_ok=True)
P=ROOT/'data_nbu/parquet'; T=ROOT/'output_nbu'
RNG=np.random.default_rng(20261105)
def read(n):
 d=pd.read_parquet(P/(n+'.parquet'));d['month']=pd.to_datetime(d.month)
 if 'nkb' in d:d['nkb']=d.nkb.astype(str)
 return d
B=read('mart_bank_month_v6');A=read('mart_bank_assets_v6');S=read('mart_system_month_v6')
B=B.sort_values(['nkb','month']).reset_index(drop=True)
assert not B.duplicated(['nkb','month']).any(), 'duplicate bank-month keys'
assert not A.duplicated(['nkb','month']).any()
B=B.merge(A[['nkb','month','avg_iea','nii_m','equity','assets','ii_le_m','ii_hh_m','ie_m','avg_loans_le','avg_loans_hh','avg_ibl']],on=['nkb','month'],validate='one_to_one')
B=B.merge(S[['month','policy_avg']],on='month',validate='many_to_one')
B=attach(B, ROOT)
B['mi']=B.month.dt.year*12+B.month.dt.month
B['bank']=B.fullname.map(clean_bank_name)
for c in ['nim','roe']:
 flow=B.nii_m if c=='nim' else B.fin_profit_m
 den=B.avg_iea if c=='nim' else B.fin_equity
 B[c]=100*B.ann*flow/den.where(den>1)
B.loc[~B.nim.between(-10,40),'nim']=np.nan
B['policy']=B.policy_avg*100
for k,flow,den in [('y_le','ii_le_m','avg_loans_le'),('y_hh','ii_hh_m','avg_loans_hh'),('c_ibl','ie_m','avg_ibl')]:
 B[k]=B.ann*B[flow]/B[den].where(B[den]>1)
B.loc[B.month.lt('2021-01-01'),'y_le']=np.nan

def lag(d,col,k=12):
 # exact calendar lookup, never row offset across a gap
 z=d.set_index(['nkb','mi'])[col]
 return z.reindex(pd.MultiIndex.from_arrays([d.nkb,d.mi-k])).to_numpy()

def save(d,name):
 d.to_csv(OUT/(name+'.csv'),index=False,encoding='utf-8-sig');return d

def fit_fe(d,y,x,trend=False,twoway=True):
 d=d.dropna(subset=[y]+x+['nkb','month']).copy()
 if len(d)<80:return None
 z=d[[y]+x].to_numpy(float)
 # Alternating projections on bank FE and month FE; bank trends are optional.
 bid,un=pd.factorize(d.nkb);tid,tn=pd.factorize(d.month);n=len(d)
 for _ in range(400):
  prev=z.copy()
  for codes in [bid,tid] if twoway else [bid]:
   count=np.bincount(codes);sums=np.zeros((len(count),z.shape[1]));np.add.at(sums,codes,z);z-=sums[codes]/count[codes,None]
  if trend:
   tt=d.mi.to_numpy(float);tt-=pd.Series(tt).groupby(bid).transform('mean').to_numpy()
   den=np.bincount(bid,weights=tt*tt);num=np.zeros((len(den),z.shape[1]));np.add.at(num,bid,z*tt[:,None]);z-=tt[:,None]*num[bid]/np.maximum(den[bid,None],1)
  if np.max(abs(z-prev))<1e-8:break
 X=z[:,1:];Y=z[:,0]
 if np.linalg.matrix_rank(X)<X.shape[1]:return None
 inv=np.linalg.inv(X.T@X);coef=inv@X.T@Y;u=Y-X@coef
 def meat(codes):
  G=int(codes.max()+1);h=np.zeros((G,X.shape[1]));np.add.at(h,codes,X*u[:,None]);return h.T@h*G/max(G-1,1)*(n-1)/max(n-X.shape[1]-len(un)-len(tn)+1,1)
 # bank clustering handles overlapping 12-month outcomes; month clustering
 # handles cross-bank residual dependence. Also retain bank-only SE.
 vb=inv@meat(bid)@inv
 pair=np.arange(n);v=inv@(meat(bid)+meat(tid)-meat(pair))@inv
 se=np.sqrt(np.where(np.diag(v)>0,np.diag(v),np.nan));seb=np.sqrt(np.where(np.diag(vb)>0,np.diag(vb),np.nan))
 p=2*stats.t.sf(np.abs(coef/se),max(min(len(un),len(tn))-1,1))
 return dict(b=coef,se=se,se_bank=seb,p=p,V=v,V_bank=vb,n=n,banks=len(un),months=len(tn),terms=x)

# 2.1: bank coverage and weights; changing membership is explicit.
co=[]
last=B.month.max();atlast=set(B.loc[B.month.eq(last),'nkb'])
for m,d in B.groupby('month'):
 co.append(dict(month=m,banks=d.nkb.nunique(),ex_post_survivors=d.nkb.isin(atlast).sum(),dep=d.dep_core4_total.sum(),dep_survivors=d.loc[d.nkb.isin(atlast),'dep_core4_total'].sum()))
save(pd.DataFrame(co),'R21_sample_coverage')
# 2.2: genuine calendar 12-month long differences, common shocks; DK SE over time.
RATE={'cod4':'cod4_uah','hh_demand':'r_hh_demand_uah','hh_term':'r_hh_term_uah','corp_demand':'r_corp_demand_uah','corp_term':'r_corp_term_uah','y_cd':'y_cd_uah','y_le':'y_le','y_hh':'y_hh','y_refin':'y_refin_uah','c_ibl':'c_ibl'}
B['di12']=B.policy-lag(B,'policy')
ld=[]
for key,c in RATE.items():
 B['dr_'+key]=100*(B[c]-lag(B,c))
 for variant,mask in [('all',np.ones(len(B),bool)),('without_state',B.owner.ne('State')),('prewar',B.month.lt('2022-02-01')),('wartime',B.month.ge('2022-02-01'))]:
  d=B.loc[mask,['nkb','month','di12','dr_'+key]].dropna().copy();d=d.loc[d['dr_'+key].abs()<60]
  if len(d)<100:continue
  X=d.di12-d.groupby('nkb').di12.transform('mean');Y=d['dr_'+key]-d.groupby('nkb')['dr_'+key].transform('mean')
  den=float(X@X)
  if den<1e-6:continue
  b=float(X@Y/den);u=Y-b*X
  h=pd.Series(X.to_numpy()*u.to_numpy(),index=d.month).groupby(level=0).sum().sort_index().to_numpy();L=min(12,len(h)-1)
  meat=float(h@h)+sum(2*(1-k/(L+1))*float(h[k:]@h[:-k]) for k in range(1,L+1))
  se=np.sqrt(max(meat,0))/den
  ld.append(dict(key=key,variant=variant,beta=b,se_DK12=se,n=len(d),banks=d.nkb.nunique(),months=d.month.nunique()))
save(pd.DataFrame(ld),'R22_long_difference_robustness')

# 2.3: same-sample original gap vs a past-only 12-month sensitivity.
ITEM_STOCK={
 'cd':['cd_nbu_uah','nbu_placements_uah'],'ib':['interbank_assets_uah'],
 'loan_corp':['loans_corp_all_uah'],'loan_hh':['loans_hh_all_uah'],
 'ovdp':['ovdp_uah','refin_other_uah'],'sec':['sec_other_all_uah','loans_gov_uah'],
 'res':['nbu_corr_reserves_uah'],'hh_dem':['dep_hh_demand_uah'],'hh_term':['dep_hh_term_uah'],
 'corp_dem':['dep_corp_demand_uah','dep_nbfi_demand_uah','dep_budget_uah'],
 'corp_term':['dep_corp_term_uah','dep_nbfi_term_uah'],'nbu':['liab_nbu_uah'],'ibl':['liab_interbank_uah'],
 'oth':['liab_ifi_uah','liab_own_sec_uah','liab_subdebt_uah','repo_clients_uah','savings_certs_uah']}
IT=pd.read_csv(T/'section_2_3/tables/S23_items_parameters.csv')
BP=pd.read_csv(T/'section_2_3/tables/S23_beta_paths.csv');beta=BP.loc[BP.h.eq(12)].set_index('beta_key').b
B['rg']=0.;B['cg']=0.;B['rg_past']=0.
train=[]
for row in IT.itertuples():
 sign=1 if row.side=='A' else -1;stock=B[ITEM_STOCK[row.item]].sum(axis=1,min_count=len(ITEM_STOCK[row.item]));B['U_'+row.item]=stock
 bv=beta.get(row.key,0.) if isinstance(row.key,str) else 0.
 B['rg']+=sign*stock*bv/B.fin_assets
 if row.item=='res':continue
 if row.key not in RATE:continue
 bymonth={}
 for cutoff in sorted(B.month.unique()):
  # rates and both endpoints must have been observed by cutoff; a 36-month minimum
  # prevents spurious estimates from nearly constant policy rates.
  d=B.loc[B.month.le(cutoff),['nkb','month','di12','dr_'+row.key]].dropna()
  if d.month.nunique()<36: bymonth[cutoff]=np.nan;continue
  x=d.di12-d.groupby('nkb').di12.transform('mean');y=d['dr_'+row.key]-d.groupby('nkb')['dr_'+row.key].transform('mean')
  v=float(x@y/(x@x)) if x@x>1 else np.nan;bymonth[cutoff]=v
  train.append(dict(item=row.item,cutoff=cutoff,beta=v,n=len(d),months=d.month.nunique()))
 B['rg_past']+=sign*stock*B.month.map(bymonth)/B.fin_assets
# correct contractual proxy, with month convention matching the supplied baseline (h+1).
B['cg']=0.
for _,r in IT.iterrows():B['cg']+=(1 if r.side=='A' else -1)*B['U_'+r['item']]*(1-(1-r['lambda'])**13)/B.fin_assets
save(pd.DataFrame(train),'R23_training_cutoffs')
B['dy_nim']=B.nim-lag(B,'nim');B['dy_roe']=B.roe-lag(B,'roe')
# NIM denominator is average interest-earning assets, whereas displayed gaps use total assets.
# Normalize the validation predictor to the same lagged IEA denominator.
B['gap_to_nim_denominator']=B.fin_assets/B.avg_iea.where(B.avg_iea>1)
for c in ['rg','cg','rg_past']:
 B['g_nim_'+c]=B[c]*B.gap_to_nim_denominator
 B['x_'+c]=lag(B,'g_nim_'+c)*B.di12
val=[]
for outcome in ['dy_nim','dy_roe']:
 for sample,mask in [('all',np.ones(len(B),bool)),('without_state',B.owner.ne('State')),('without_2022',~B.month.between('2022-02-01','2022-12-01')),('ex_post_survivors',B.nkb.isin(atlast))]:
  for spec,xs in [('baseline',['x_rg','x_cg']),('past_only_common_sample',['x_rg_past','x_cg']),('baseline_on_past_sample',['x_rg','x_cg'])]:
   d=B.loc[mask].copy()
   if 'past' in spec:d=d.dropna(subset=['x_rg_past'])
   f=fit_fe(d,outcome,xs)
   if f:
    for j,x in enumerate(xs):val.append(dict(outcome=outcome,sample=sample,spec=spec,term=x,b=f['b'][j],se_two_way=f['se'][j],se_bank=f['se_bank'][j],p=f['p'][j],n=f['n'],banks=f['banks'],months=f['months']))
save(pd.DataFrame(val),'R23_validation')
# expose generated-regressor / denominator mismatch, do not label this causal or true real-time
save(B[['nkb','bank','owner','month','nim','roe','rg','cg','rg_past','dy_nim','di12','x_rg','x_cg','x_rg_past']],'R23_validation_panel')
# Duration sensitivity including an observed severe monthly run-off, not annualising a two-month war loss.
D5=pd.read_csv(T/'section_2_1_v2/tables/D5_effective_duration_nmd.csv');tot=pd.read_csv(T/'section_2_3/tables/S23_bank_totals_latest.csv',dtype={'nkb':str});items=pd.read_csv(T/'section_2_3/tables/S23_bank_items_latest.csv',dtype={'nkb':str})
tot=attach(tot, ROOT)
dur=[]
for delta in [.1,.2,.3]:
 for shock in [-.02,.02]:
  per=[]
  for _,bank in tot.iterrows():
   z=items.loc[items.nkb.eq(bank.nkb)].merge(IT[['item','dur']],on='item',validate='many_to_one');dv=0
   for r in z.itertuples():
    dd=r.dur
    if r.item in ['hh_dem','corp_dem']:
     pr='hh_demand' if r.item=='hh_dem' else 'corp_demand';q=D5.loc[D5['product'].eq(pr)&np.isclose(D5.delta,delta)].sort_values('i0')
     dd=np.interp(bank.policy/100,q.i0,q.duration)
    dv+=(1 if r.side=='A' else -1)*(r.uah+r.fx)*dd
   eve=-shock*dv
   t1=bank.tier1_mn
   per.append(dict(nkb=bank.nkb,bank=bank.bank,month=bank.month,report_date=bank.report_date,delta=delta,shock_bp=round(shock*1e4),dEVE=eve,
                   tier1_mn=t1,eve_pct_tier1=100*eve/t1 if pd.notna(t1) and t1>0 else np.nan,
                   eve_pct_equity=100*eve/bank.EQ if bank.EQ>0 else np.nan,
                   loss_exceeds_15pct=(eve<-.15*t1) if pd.notna(t1) and t1>0 else np.nan,
                   equity_proxy_loss_exceeds_15pct=(eve<-.15*bank.EQ) if bank.EQ>0 else np.nan))
  dur+=per
save(pd.DataFrame(dur),'R23_duration_sensitivity_banks')
save(pd.DataFrame(dur).groupby(['delta','shock_bp']).agg(dEVE=('dEVE','sum'),tier1_mn=('tier1_mn','sum'),
     tier1_banks=('tier1_mn','count'),share_loss_above_15pct=('loss_exceeds_15pct','mean'),
     share_equity_proxy_loss_above_15pct=('equity_proxy_loss_exceeds_15pct','mean')).reset_index(),'R23_duration_sensitivity_system')
runoff=[]
for pr in ['hh_demand','corp_demand']:
 for cc in ['uah','fx']:
  c='dep_'+pr+'_'+cc;v=B.groupby('month')[c].sum().sort_index();ratio=v.reindex(pd.to_datetime(['2022-01-01','2022-03-01']))
  runoff.append(dict(product=pr,ccy=cc,jan2022=ratio.iloc[0],mar2022=ratio.iloc[1],two_month_change=ratio.iloc[1]/ratio.iloc[0]-1,note='FX stocks are UAH equivalents; exchange-rate valuation is not removed.'))
save(pd.DataFrame(runoff),'R23_wartime_deposit_changes')

# 2.4: reforms with pre-estimated bank trends; p-values adjusted for multiple outcomes.
# This diagnostic cannot prove parallel counterfactual trends after the event.
base=B.dep_core6_total+B.dep_budget_total
expo={'hh_dem':(B.dep_hh_demand_uah+B.dep_hh_demand_fx)/base,'hh_term':(B.dep_hh_term_uah+B.dep_hh_term_fx)/base,'corp_term':(B.dep_corp_term_uah+B.dep_nbfi_term_uah)/base,'all_res':(B.dep_hh_demand_uah+B.dep_corp_demand_uah+B.dep_corp_term_uah)/base}
for k,v in expo.items():B['ex_'+k]=v
outs={'hh_term_rate':100*B.r_hh_term_uah,'hh_demand_rate':100*B.r_hh_demand_uah,'corp_term_rate':100*B.r_corp_term_uah,'fx_term_rate':100*B.r_hh_term_fx}
for k,c in [('cd','cd_nbu_total'),('ovdp','ovdp_uah'),('hh_term_volume','dep_hh_term_uah'),('corp_term_volume','dep_corp_term_uah'),('corp_loans','loans_corp_all_uah')]:outs[k]=100*np.log(B[c].where(B[c]>1))
for k,v in outs.items():B['o_'+k]=v
ref=[]
for date,exp in [('2023-03-01','hh_dem'),('2023-05-01','hh_term'),('2023-09-01','corp_term'),('2024-10-01','all_res')]:
 r0=pd.Timestamp(date);mi=r0.year*12+r0.month
 for sample,mask in [('all',np.ones(len(B),bool)),('without_state',B.owner.ne('State'))]:
  for outcome in outs:
   d=B.loc[mask&B.mi.between(mi-6,mi+9)].copy();d['k']=d.mi-mi
   ex=d.loc[d.k.lt(0)].groupby('nkb')['ex_'+exp].mean();ex=(ex-ex.mean())/ex.std();d['E']=d.nkb.map(ex)
   full=d.groupby('nkb')['o_'+outcome].count();d=d.loc[d.nkb.isin(full[full>=13].index)].copy()
   for version in ['base','pretrend_adjusted']:
    q=d.copy();q['y']=q['o_'+outcome]
    if version=='pretrend_adjusted':
     for nk,pre in q.loc[q.k.lt(0)].groupby('nkb'):
      pre=pre.dropna(subset=['y'])
      if len(pre)<5:q.loc[q.nkb.eq(nk),'y']=np.nan;continue
      slope=np.polyfit(pre.k,pre.y,1)[0];sel=q.nkb.eq(nk);q.loc[sel,'y']-=slope*q.loc[sel,'k']
    ks=[k for k in range(-6,10) if k!=-1];xs=[]
    for k in ks:
     name='e'+str(k);q[name]=q.E*q.k.eq(k);xs.append(name)
    f=fit_fe(q,'y',xs)
    if not f:continue
    a=np.array([1/10 if k>=0 else 0 for k in ks]);b=float(a@f['b']);se=float(np.sqrt(max(a@f['V_bank']@a,0)))
    ix=[j for j,k in enumerate(ks) if k<-1];bp=f['b'][ix];V=f['V_bank'][np.ix_(ix,ix)];wald=float(bp@np.linalg.pinv(V)@bp)
    ref.append(dict(reform=date,outcome=outcome,sample=sample,version=version,post_avg=b,se=se,p=2*stats.t.sf(abs(b/se),max(f['banks']-1,1)) if se>0 else np.nan,pretrend_p=stats.f.sf(max(wald,0)/len(ix),len(ix),f['banks']-1),n=f['n'],banks=f['banks']))
REF=pd.DataFrame(ref)
# Benjamini-Hochberg within reform × sample × version across 9 outcomes
REF['p_BH']=np.nan
for _,idx in REF.groupby(['reform','sample','version']).groups.items():
 ps=REF.loc[idx,'p'];order=ps.sort_values().index;m=len(order);adj=REF.loc[order,'p'].to_numpy()*m/np.arange(1,m+1);REF.loc[order,'p_BH']=np.minimum(1,np.minimum.accumulate(adj[::-1])[::-1])
REF['interpretation']='conditional association; overlapping policies and generated exposure prevent a causal interpretation'
save(REF,'R24_reforms_robustness')
# Carry vs lending: association only, controls and lagged balance-sheet composition.
H=pd.read_csv(T/'section_2_4/tables/S24_bank_history_60m.csv');H['month']=pd.to_datetime(H.month);H['nkb']=H.nkb.astype(str)
# 2.4 parameter sensitivity at latest snapshot (book-rate alternatives are not hedged funding costs).
prod=pd.read_csv(T/'section_2_4/tables/S24_bank_products_latest.csv');sm=S.sort_values('month').iloc[-1];alt=sm.cd_on_rate_avg;yb=sm.zc_uah_3y_avg
params=[]
for short in [0,.15,.30,.50,1.]:
 for cov_use in [0,.5,1.]:
  for dep in [0,.05,.15,.30]:
   z=prod.copy();hh=z['prod'].eq('hh_term');z.loc[hh,'rho']=np.where(z.loc[hh,'ccy'].eq('uah'),.25*short,.35*short+.10*(1-short))
   nc=np.where(z['prod'].eq('hh_demand'),.10,np.where(hh,.10*short,0));z['rc']=np.minimum(.6*z.rho,np.maximum(z.rho-nc,0))*cov_use
   r=np.where(z.ccy.eq('fx'),(1+z.r)*(1+dep)-1,z.r);ec=r+(z.rho-z.rc)*alt+z.rc*(alt-yb)+z.dgf
   params.append(dict(short_share=short,coverage_utilisation=cov_use,expected_depreciation=dep,ec_pct=100*np.average(ec,weights=z.stock)))
save(pd.DataFrame(params),'R24_parameter_sensitivity')
# OU: jump-frequency sensitivity to regime and threshold, rather than only one calibration.
h=pd.read_csv(ROOT/'data_nbu/local_inputs/policy_rate_history.csv');h['from']=pd.to_datetime(h['from']);h=h.loc[h['from'].le('2026-11-05')].copy();h['change']=h.rate.diff()
jumps=[]
for start in ['2010-06-08','2016-01-01','2018-01-01','2022-02-01']:
 for th in [2,3,5]:
  q=h.loc[h['from'].ge(start)];years=(pd.Timestamp('2026-11-05')-pd.Timestamp(start)).days/365.25;n=int(q.change.abs().ge(th).sum());alpha=n+.5;rate=years+1
  jumps.append(dict(start=start,threshold_pp=th,jumps=n,years=years,lambda_post=alpha/rate,lo90=stats.gamma.ppf(.05,alpha,scale=1/rate),hi90=stats.gamma.ppf(.95,alpha,scale=1/rate),prior='Gamma(shape=.5, rate=1 year)'))
save(pd.DataFrame(jumps),'ROU_jump_frequency_sensitivity')
# Small-sample sup-F scan and residual moving-block bootstrap for a policy AR(1).
z=S[['month','policy_avg']].sort_values('month').dropna().copy();z['lag']=z.policy_avg.shift();z=z.dropna();y=z.policy_avg.to_numpy();X=np.column_stack([np.ones(len(z)),z['lag']]);n=len(y);candidates=range(24,n-24)
def supf(y):
 e=y-X@np.linalg.lstsq(X,y,rcond=None)[0];ss=e@e;vals=[]
 for k in candidates:
  a=y[:k]-X[:k]@np.linalg.lstsq(X[:k],y[:k],rcond=None)[0];b=y[k:]-X[k:]@np.linalg.lstsq(X[k:],y[k:],rcond=None)[0];rss=a@a+b@b;vals.append(((ss-rss)/2)/(rss/(n-4)))
 return max(vals),list(candidates)[int(np.argmax(vals))]
f,k=supf(y);beta0=np.linalg.lstsq(X,y,rcond=None)[0];res=y-X@beta0;res-=res.mean();boots=[]
for _ in range(199):
 e=np.concatenate([np.take(res,np.arange(j,j+6)%n) for j in RNG.integers(0,n,size=(n+5)//6)])[:n];boots.append(supf(X@beta0+e)[0])
save(pd.DataFrame([dict(supF=f,break_month=z.iloc[k].month,bootstrap_p=(1+sum(v>=f for v in boots))/(1+len(boots)),replications=len(boots),block_months=6,n=n,note='Fixed-design AR diagnostic, not a structural regime identification')]),'ROU_break_diagnostic')
# New capital/liquidity checks use the same exact-calendar lag function and inference.
from capital_review import run as capital_review
capital_review(B, ROOT, fit_fe, lag)
from capital_refresh import run as refresh_capital
refresh_capital(ROOT)
# independent coverage and leakage checks
manifest={'data_end':str(B.month.max().date()),'bank_months':len(B),'banks':B.nkb.nunique(),'latest_banks':len(atlast),'duplicate_bank_months':int(B.duplicated(['nkb','month']).sum()),'calendar_gaps':int(B.groupby('nkb').mi.diff().gt(1).sum()),'seed':20261105,'inference':'two-way bank/month clusters for gap validation; bank clusters for short-window reforms; DK12 for common-shock long differences','forecast_scope':'past-only coefficients use vintage-final data, so pseudo out of sample, not real-time','remaining':'No external instrument, contract repricing dates, Tier 1 capital series, actual reserve base or hedging terms supplied. Conditional sensitivities do not resolve these identification/data gaps.'}
manifest['remaining']='No external instrument, contract repricing dates, actual reserve base or hedging terms supplied. Observed Tier 1 is available from August 2024 economic month; earlier core capital is a separate regime. Full bank-specific capital requirements and historical publication vintages are unavailable.'
manifest['capital']=json.loads((T/'capital/tables/capital_manifest.json').read_text())
latest_cap=B.loc[B.month.eq(last)]
manifest['capital']['latest_panel_tier1_banks']=int(latest_cap.tier1_mn.gt(0).sum())
manifest['input_sha256']={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in P.glob('mart*.parquet')}
(OUT/'validation_manifest.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2))
print(json.dumps({k:v for k,v in manifest.items() if k!='input_sha256'},ensure_ascii=False,indent=2))
