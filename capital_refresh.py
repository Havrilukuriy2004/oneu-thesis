"""Refresh inherited EVE denominators and presentations, without rerunning R numerators."""
from pathlib import Path
import json
import shutil
import numpy as np
import pandas as pd
from capital_inputs import attach


def run(root):
    root=Path(root); tab=root/'output_nbu/section_2_3/tables'
    backup=root/'review/inherited_capital_baseline';backup.mkdir(exist_ok=True)
    changed=[]
    def write(d,name):
        path=tab/name
        if path.exists() and not (backup/name).exists():shutil.copy2(path,backup/name)
        d.to_csv(path,index=False,encoding='utf-8-sig',date_format='%Y-%m-%d');changed.append(name)
    totals=pd.read_csv(tab/'S23_bank_totals_latest.csv',dtype={'nkb':str})
    totals=attach(totals,root)
    totals['T1']=totals.tier1_mn
    totals['capital_report_date']=totals.report_date
    write(totals,'S23_bank_totals_latest.csv')
    gap=pd.read_csv(tab/'S23_R3_bank_gaps_latest.csv',dtype={'nkb':str})
    if 'month' not in gap:gap['month']=totals.month.max()
    gap=attach(gap,root)
    gap['T1_mn']=gap.tier1_mn
    gap['capital_report_date']=gap.report_date
    # The inherited numerator is recovered from its book-equity percentage.
    # Preserve book-equity percentages as such; only new Tier 1 ratios are screenings.
    for kind in ['c','b']:
        eve=gap['dEVE_'+kind+'_eq']*gap.EQ_bn*1000/100
        gap['dEVE_'+kind+'_t1']=100*eve/gap.tier1_mn.where(gap.tier1_mn>0)
        gap['SOT_'+kind]=pd.array(np.where(gap.tier1_mn.gt(0),-gap['dEVE_'+kind+'_t1']>15,None),dtype='boolean')
    write(gap,'S23_R3_bank_gaps_latest.csv')
    cols=['nkb','month','report_date','reg_capital_mn','tier1_mn','cet1_mn','headroom_total_min_pp',
          'headroom_tier1_min_pp','lcr_all_pct','nsfr_pct','capital_source_conflict']
    write(totals[cols],'S23_capital_latest.csv')
    own=pd.read_csv(tab/'S23_R3_by_ownership_latest.csv')
    for idx,row in own.iterrows():
        sample=gap.loc[gap.owner.eq(row.owner)&gap.tier1_mn.gt(0)]
        own.loc[idx,'Tier1_mn']=sample.tier1_mn.sum()
        own.loc[idx,'Tier1_banks']=len(sample)
        own.loc[idx,'dEVE_b_t1']=np.average(sample.dEVE_b_t1,weights=sample.tier1_mn) if len(sample) else np.nan
        own.loc[idx,'share_SOT_b']=100*sample.SOT_b.mean() if len(sample) else np.nan
    write(own,'S23_R3_by_ownership_latest.csv')
    # Cover the historical system ratio only where every underlying bank has Tier 1.
    B=pd.read_parquet(root/'data_nbu/parquet/mart_bank_month_v6.parquet')
    B=attach(B[['nkb','month']],root)
    cov=B.groupby('month').agg(tier1_banks=('tier1_mn','count'),capital_panel_banks=('nkb','size'),T1=('tier1_mn','sum')).reset_index()
    system=pd.read_csv(tab/'S23_R3_system_monthly.csv',parse_dates=['month'])
    system=system.drop(columns=[c for c in cov if c in system and c!='month'])
    system=system.merge(cov,on='month',how='left',validate='one_to_one')
    den=system.T1.where(system.tier1_banks.eq(system.capital_panel_banks)&system.T1.gt(0))
    for kind in ['c','b']:system['dEVE_'+kind+'_t1']=100*system['dEVE_'+kind]/den
    write(system,'S23_R3_system_monthly.csv')
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    plt.rcParams.update({'font.family':'DejaVu Sans','mathtext.fontset':'stix','svg.fonttype':'path','font.size':10})
    fig,ax=plt.subplots(figsize=(8.5,5.2))
    for owner,group in gap.groupby('owner'):
        ax.scatter(group.dEVE_c_t1,group.dEVE_b_t1,label=owner,s=24,alpha=.8)
    ax.axhline(-15,color='.4',ls='--',lw=.8);ax.axvline(-15,color='.4',ls='--',lw=.8)
    ax.set_xlabel(r'Договірна оцінка $100\,\frac{\Delta EVE}{Tier\,1}$, %')
    ax.set_ylabel(r'Поведінкова оцінка $100\,\frac{\Delta EVE}{Tier\,1}$, %')
    ax.set_title('Паралельний шок +200 б.п. та фактичний капітал першого рівня')
    ax.legend(frameon=False);ax.spines[['top','right']].set_visible(False)
    fig.text(.01,.015,'Баланс 08.2026, капітал на 01.09.2026. Пунктир - втрата 15% Tier 1. Дослідницький скринінг.',fontsize=8)
    fig.tight_layout(rect=(0,.05,1,1))
    base=root/'output_nbu/section_2_3/figures';stem='S23_R3_eve_outlier_test'
    for fmt,folder in [('png',base),('pdf',base/'pdf'),('svg',base/'svg')]:
        folder.mkdir(exist_ok=True);fig.savefig(folder/(stem+'.'+fmt),dpi=220)
    plt.close(fig)
    old_emf=base/'emf'/(stem+'.emf')
    if old_emf.exists():old_emf.unlink()
    origin=root/'output_nbu/origin'
    meta_path=origin/'meta'/(stem+'.json')
    meta=json.loads(meta_path.read_text()) if meta_path.exists() else {}
    meta.update(title='Паралельний шок +200 б.п. та фактичний Tier 1',
                subtitle='Пунктир - втрата 15% фактичного Tier 1 на ту саму дату балансу',
                x_title='Договірна оцінка, % Tier 1',y_title='Поведінкова оцінка, % Tier 1',
                provenance='Inherited R EVE numerator; exact-date NBU Tier 1 denominator; capital_refresh.py')
    meta_path.write_text(json.dumps(meta,ensure_ascii=False,indent=2))
    points=gap[['owner','dEVE_c_t1','dEVE_b_t1']].rename(columns={'owner':'series','dEVE_c_t1':'x','dEVE_b_t1':'y'})
    points['panel']='';points['layer']=3;points['geom']='point'
    lines=pd.DataFrame([dict(panel='',layer=1,geom='hline',series='',y=-15),dict(panel='',layer=2,geom='vline',series='',xline=-15)])
    pd.concat([lines,points],ignore_index=True).to_csv(origin/'data'/(stem+'.csv'),index=False,encoding='utf-8-sig')
    captions=base/'figure_captions.csv'
    if captions.exists():
        cc=pd.read_csv(captions)
        for col in ['title','subtitle','x_title','y_title']:
            if col in cc:
                mask=cc.astype(str).apply(lambda s:s.str.contains(stem,regex=False)).any(axis=1)
                cc.loc[mask,col]=meta[col]
        cc.to_csv(captions,index=False,encoding='utf-8-sig')
    mp=root/'review/math_rendering.json'
    mr=json.loads(mp.read_text());file=str((base/'svg'/(stem+'.svg')).relative_to(root))
    mr=[r for r in mr if r['file']!=file]
    mr.append(dict(file=file,changes=2,renderer='capital_refresh.py; mathematical fraction axes'))
    mp.write_text(json.dumps(mr,ensure_ascii=False,indent=2))
    manifest=dict(refreshed_files=changed,numerators='Inherited R results; no new contractual repricing data',
         denominator='Observed exact-date Tier 1, million UAH; no book-equity substitution',
         positive_shock_screen='Loss under +200 bp only; workbook 13 covers additional scenarios',
         latest_banks=len(gap),tier1_banks=int(gap.tier1_mn.gt(0).sum()),
         loss_flags_contract=int(gap.SOT_c.sum()),loss_flags_behavioural=int(gap.SOT_b.sum()),
         total_tier1_mn=float(gap.tier1_mn.sum()))
    (root/'review/capital_refresh_manifest.json').write_text(json.dumps(manifest,indent=2))
    return manifest


if __name__=='__main__':
    print(json.dumps(run(Path(__file__).resolve().parent),indent=2))
