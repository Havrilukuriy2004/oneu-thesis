"""Capital-aware supplementary estimates; does not relabel inherited R estimates."""
from pathlib import Path
import json
import numpy as np
import pandas as pd


def run(data, root, fit_fe, lag):
    root=Path(root); out=root/'review'; d=data.copy().sort_values(['nkb','month']).reset_index(drop=True)
    def save(x,name):
        x.to_csv(out/(name+'.csv'),index=False,encoding='utf-8-sig')
    cols=['nkb','month','bank','owner','report_date','capital_regime','reg_capital_mn','legacy_core_mn','tier1_mn',
          'cet1_mn','total_ratio_pct','tier1_ratio_pct','headroom_total_min_pp','headroom_tier1_min_pp',
          'lcr_all_pct','lcr_fx_pct','nsfr_pct','capital_source_conflict']
    save(d[cols],'R21_capital_panel_coverage')
    monthly=d.groupby('month').agg(banks=('nkb','size'),capital_banks=('reg_capital_mn','count'),
        tier1_banks=('tier1_mn','count'),lcr_banks=('lcr_all_pct','count'),nsfr_banks=('nsfr_pct','count'),
        median_headroom_min_pp=('headroom_total_min_pp','median'),median_lcr=('lcr_all_pct','median'),
        reg_capital_mn=('reg_capital_mn','sum'),tier1_mn=('tier1_mn','sum')).reset_index()
    save(monthly,'R21_capital_monthly')
    d['size_l1']=lag(d,'fin_assets',1);d['size_l1']=np.log(d.size_l1.where(d.size_l1>0))
    d['policy_change']=d.policy-lag(d,'policy',1)
    characteristics=['headroom_total_min_pp','headroom_tier1_min_pp','lcr_all_pct','nsfr_pct']
    for char in characteristics:
        v=pd.Series(lag(d,char,1),index=d.index)
        mean=v.groupby(d.month).transform('mean');sd=v.groupby(d.month).transform('std')
        d['z_'+char]=(v-mean)/sd.where(sd>0)
    estimates=[]
    # Different regulatory definitions are estimated separately; short new regime is visible in N/months.
    for regime in ['legacy','three_tier']:
      for char in characteristics:
        for key,col,mult in [('cod4','cod4_uah',100),('hh_term','r_hh_term_uah',100),('y_le','y_le',100),('nim','nim',1)]:
          for h in [3,6,12]:
            d['y_capital']=mult*(lag(d,col,-h)-lag(d,col,1))
            d['z_level']=d['z_'+char];d['shock_interaction']=d.policy_change*d.z_level
            # Outcomes must end in the same capital regime as their originating observation.
            end_regime=pd.Series(lag(d,'capital_regime',-h),index=d.index)
            prior_regime=pd.Series(lag(d,'capital_regime',1),index=d.index)
            for sample,mask in [('all',d.owner.notna()),('without_state',d.owner.ne('State'))]:
              q=d.loc[mask & d.capital_regime.eq(regime) & end_regime.eq(regime) & prior_regime.eq(regime)].copy()
              f=fit_fe(q,'y_capital',['shock_interaction','z_level','size_l1'])
              if f:
                estimates.append(dict(outcome=key,h=h,characteristic=char,regime=regime,sample=sample,
                     b=f['b'][0],se_two_way=f['se'][0],p=f['p'][0],n=f['n'],banks=f['banks'],months=f['months'],
                     inference_status='available' if np.isfinite(f['se'][0]) else 'unavailable_nonpositive_cluster_variance',
                     interpretation='conditional interaction per 1 SD of lagged characteristic; not causal identification'))
    est=pd.DataFrame(estimates);est['p_BH']=np.nan
    for _,idx in est.groupby(['outcome','regime','sample']).groups.items():
        valid=est.loc[idx,'p'].dropna().sort_values();n=len(valid)
        if n:
            adjusted=valid.to_numpy()*n/np.arange(1,n+1)
            est.loc[valid.index,'p_BH']=np.minimum(1,np.minimum.accumulate(adjusted[::-1])[::-1])
    save(est,'R22_capital_liquidity_interactions')
    # Same-sample validation with observed, lagged prudential characteristics.
    validations=[]
    d['delta_headroom_l1']=lag(d,'headroom_total_min_pp',1)
    d['lcr_l1']=lag(d,'lcr_all_pct',1)
    for spec,xs in [('baseline',['x_rg','x_cg']),('past_only',['x_rg_past','x_cg'])]:
      common=d.dropna(subset=xs+['delta_headroom_l1','lcr_l1','size_l1']).copy()
      for controls in [False,True]:
        terms=xs+(['delta_headroom_l1','lcr_l1','size_l1'] if controls else [])
        f=fit_fe(common,'dy_nim',terms)
        if f:
          for j,term in enumerate(terms):
            validations.append(dict(spec=spec,controls=controls,term=term,b=f['b'][j],se_two_way=f['se'][j],p=f['p'][j],
                                    n=f['n'],banks=f['banks'],months=f['months']))
    save(pd.DataFrame(validations),'R23_capital_controlled_validation')
    # Descriptive SFC comparison, not an identification of the effect of capital on funding costs.
    history=pd.read_csv(root/'output_nbu/section_2_4/tables/S24_bank_history_60m.csv',dtype={'nkb':str})
    history['month']=pd.to_datetime(history.month)
    history=history.merge(d[cols],on=['nkb','month'],how='left',suffixes=('','_cap'),validate='many_to_one')
    save(history,'R24_shadow_cost_with_capital')
    summary=dict(interaction_estimates=len(estimates),inference_unavailable=sum(r['inference_status']!='available' for r in estimates),validation_coefficients=len(validations),
                 baseline_data_end=str(d.month.max().date()),
                 provenance='New Python supplement; inherited R estimates remain separately labelled',
                 headroom='General minimum benchmark only; bank-specific SREP requirements and buffers unknown',
                 vintage='Final-vintage prudential data; lagging balances does not establish historical publication availability')
    (out/'capital_analysis_manifest.json').write_text(json.dumps(summary,indent=2))
