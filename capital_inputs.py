"""Read NBU prudential workbooks without mixing report dates or capital regimes.

Amounts in source workbooks are thousand UAH; output amounts are million UAH.
Report date 01.(m+1) describes the balance at the end of economic month m.
No interpolation, carrying forward, name-based matching or Tier 1 substitution.
"""
from pathlib import Path
from datetime import datetime
import hashlib
import json
import re
import numpy as np
import pandas as pd
from openpyxl import load_workbook
from bank_names import clean_bank_name

VERSION = 2
FILES = ("Skladovi_RK_2026-10-01.xlsx", "Ratios_by_Banks_2026-10-01.xlsx")
NUMERIC = ["reg_capital_mn", "legacy_core_mn", "tier1_mn", "cet1_mn",
           "total_ratio_pct", "legacy_core_ratio_pct", "tier1_ratio_pct", "cet1_ratio_pct",
           "lcr_all_pct", "lcr_fx_pct", "nsfr_pct", "leverage_pct", "credit_rwa_mn",
           "operational_risk_equivalent_mn", "market_risk_equivalent_mn"]


def number(x):
    if x is None or isinstance(x, bool):
        return np.nan
    try:
        return float(str(x).replace("\xa0", "").replace(" ", "").replace(",", "."))
    except ValueError:
        return np.nan


def report_date(title):
    for fmt in ("%d.%m.%Y", "%d%m%Y"):
        try:
            return pd.Timestamp(datetime.strptime(title.strip(), fmt))
        except ValueError:
            pass
    return None


def code(x):
    m = re.match(r"^\s*(\d+)", str(x))
    return str(int(m[1])) if m else None


def read_book(path, kind):
    records, sheets = [], []
    wb = load_workbook(path, read_only=True, data_only=True)
    for ws in wb:
        date = report_date(ws.title)
        if date is None:
            continue
        # Some legacy sheets advertise 16,384 formatted columns; only data columns matter.
        rows = list(ws.iter_rows(max_row=180, max_col=130, values_only=True))
        header = next((i for i, r in enumerate(rows[:12]) if
                       any("найменування банку" in str(v).lower() or str(v).strip() == "Банк" for v in r)), None)
        if header is None:
            raise ValueError(f"Unrecognised bank header in {path.name}/{ws.title}")
        has_code = any(str(v).strip().upper() == "NKB" for v in rows[header])
        idx_name = 2 if has_code else 1
        count = 0
        for row in rows[header + 1:]:
            if not isinstance(row[idx_name], str):
                continue
            nkb = code(row[1])
            if nkb is None:
                continue
            raw = row[idx_name].strip()
            # Exclude aggregate/footer rows, whose serial/code fields are not bank identifiers.
            if not has_code:
                raw = re.sub(r"^\s*\d+\s*", "", raw)
            rec = dict(nkb=nkb, report_date=date, month=date-pd.offsets.MonthBegin(1),
                       bank_source=clean_bank_name(raw), source_file=path.name, source_sheet=ws.title)
            rec.update({k: np.nan for k in NUMERIC})
            if kind == "components":
                new = "капітал 1 рівня" in str(rows[header][3]).lower()
                rec["capital_regime"] = "three_tier" if new else "legacy"
                rec["reg_capital_mn"] = number(row[2]) / 1000
                if new:
                    rec["tier1_mn"], rec["cet1_mn"] = number(row[3])/1000, number(row[4])/1000
                else:
                    rec["legacy_core_mn"] = number(row[3])/1000
                for j in range(130):
                    text = " ".join(str(r[j]).lower() for r in rows[header:10] if r[j] is not None)
                    actual = next((str(r[j]).lower() for r in rows[header:10] if
                                   r[j] is not None and str(r[j]).lower().strip().startswith("фактичне значення нормативу")), "")
                    if actual:
                        if "основного капіталу 1" in actual:
                            key = "cet1_ratio_pct"
                        elif "капіталу 1" in actual:
                            key = "tier1_ratio_pct"
                        elif "н3" in actual:
                            key = "legacy_core_ratio_pct"
                        else:
                            key = "total_ratio_pct"
                        rec[key] = number(row[j])
                    for word, key in [("сукупний розмір активів, зважених", "credit_rwa_mn"),
                                      ("мінімальний розмір операційного ризику", "operational_risk_equivalent_mn"),
                                      ("мінімальний розмір ринкового ризику", "market_risk_equivalent_mn")]:
                        if word in text:
                            rec[key] = number(row[j])/1000
            else:
                mapping = {"Н1": "reg_capital_mn", "Н2": "total_ratio_pct", "НРК": "total_ratio_pct",
                           "Н3": "legacy_core_ratio_pct", "НК1": "tier1_ratio_pct", "НОК1": "cet1_ratio_pct",
                           "LCRВВ": "lcr_all_pct", "LCRІВ": "lcr_fx_pct", "NSFR": "nsfr_pct", "LR": "leverage_pct"}
                rec["capital_regime"] = "three_tier" if date >= pd.Timestamp("2024-09-01") else "legacy"
                for j, label in enumerate(rows[header]):
                    token = re.sub(r"\s+", "", str(label)).upper()
                    token = re.split(r"[,，]", token)[0]
                    # Historical long headers start with H1/H2 followed by a comma.
                    if token in mapping:
                        key = mapping[token]
                        rec[key] = number(row[j])/(1000 if key == "reg_capital_mn" else 1)
                rec["systemic_flag"] = int(has_code and "свб" in str(row[3]).lower()) if has_code else np.nan
            records.append(rec)
            count += 1
        if not count:
            raise ValueError(f"No bank rows parsed in dated worksheet {path.name}/{ws.title}")
        sheets.append(dict(file=path.name, sheet=ws.title, report_date=date, rows=count))
    wb.close()
    df = pd.DataFrame(records)
    if df.empty or df.duplicated(["nkb", "month"]).any():
        raise ValueError(f"Empty or duplicate bank-month records in {path}")
    return df, sheets


def build(root):
    root = Path(root)
    local = root/"data_nbu/local_inputs"
    paths = [local/f for f in FILES]
    for p in paths:
        if not p.is_file():
            raise FileNotFoundError(f"Place the supplied workbook at {p}")
    frames, sheet_rows = [], []
    for path, kind in zip(paths, ("components", "ratios")):
        frame, sheets = read_book(path, kind)
        frames.append(frame)
        sheet_rows += sheets
    comp, ratios = frames
    keys = ["nkb", "month"]
    overlap = comp.merge(ratios, on=keys, suffixes=("_components", "_ratios"), validate="one_to_one")
    reconciliation, conflicts = [], []
    for key in ("reg_capital_mn", "total_ratio_pct", "tier1_ratio_pct", "cet1_ratio_pct"):
        x, y = overlap[key+"_components"], overlap[key+"_ratios"]
        valid = x.notna() & y.notna()
        # Source precision differs slightly; never silently resolve material contradictions.
        bad = valid & ~np.isclose(x, y, rtol=1e-6, atol=1e-4)
        reconciliation.append(dict(field=key, compared=int(valid.sum()), conflicts=int(bad.sum()),
                                   max_abs_diff=float((x[valid]-y[valid]).abs().max()) if valid.any() else None))
        if bad.any():
            detail = overlap.loc[bad, keys+[key+"_components", key+"_ratios"]].copy()
            detail.columns = keys+["components_value", "ratios_value"]
            detail["field"] = key
            conflicts.append(detail)
    panel = comp.set_index(keys).combine_first(ratios.set_index(keys)).reset_index()
    for key in ("reg_capital_mn", "total_ratio_pct", "tier1_ratio_pct", "cet1_ratio_pct"):
        panel = panel.merge(ratios[keys+[key]].rename(columns={key:"ratios_"+key}), on=keys, how="left", validate="one_to_one")
    conflicts = pd.concat(conflicts, ignore_index=True) if conflicts else pd.DataFrame(columns=keys+["components_value","ratios_value","field"])
    conflict_keys = set(map(tuple, conflicts[keys].itertuples(index=False, name=None)))
    panel["capital_source_conflict"] = [tuple(r) in conflict_keys for r in panel[keys].itertuples(index=False, name=None)]
    panel["capital_regime"] = np.where(panel.report_date.ge("2024-09-01"), "three_tier", "legacy")
    panel["minimum_total_pct"] = np.select(
        [panel.report_date.between("2024-09-01", "2025-01-01"),
         panel.report_date.between("2025-02-01", "2025-07-01")], [8.5, 9.25], default=10.)
    # Values describe end-of-previous-month positions, including the 1 January report.
    panel["headroom_total_min_pp"] = panel.total_ratio_pct-panel.minimum_total_pct
    panel["headroom_tier1_min_pp"] = panel.tier1_ratio_pct-7.5
    panel["headroom_cet1_min_pp"] = panel.cet1_ratio_pct-5.625
    # Preserve both source values; disputed observations are excluded from headroom estimation.
    panel.loc[panel.capital_source_conflict, ["headroom_total_min_pp", "headroom_tier1_min_pp", "headroom_cet1_min_pp"]] = np.nan
    panel["headroom_scope"] = "general_minimum_only_no_bank_specific_requirements_or_buffers"
    panel["risk_exposure_implied_mn"] = panel.reg_capital_mn/(panel.total_ratio_pct/100).where(panel.total_ratio_pct.gt(0))
    panel["tier1_from_ratio_check_mn"] = panel.risk_exposure_implied_mn*panel.tier1_ratio_pct/100
    panel["tier1_ratio_check_diff_mn"] = panel.tier1_mn-panel.tier1_from_ratio_check_mn
    panel = panel.sort_values(["nkb", "month"]).reset_index(drop=True)
    target = root/"data_nbu/parquet/mart_bank_prudential_v7.csv"
    target.parent.mkdir(parents=True, exist_ok=True)
    panel.to_csv(target, index=False, encoding="utf-8-sig", date_format="%Y-%m-%d")
    panel.to_parquet(target.with_suffix(".parquet"), index=False)
    out = root/"output_nbu/capital/tables"
    out.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(reconciliation).to_csv(out/"capital_source_reconciliation.csv", index=False)
    conflicts.to_csv(out/"capital_source_conflicts.csv", index=False, date_format="%Y-%m-%d")
    pd.DataFrame(sheet_rows).to_csv(out/"capital_sheet_coverage.csv", index=False, date_format="%Y-%m-%d")
    manifest = dict(parser_version=VERSION, rows=len(panel), banks=int(panel.nkb.nunique()),
                    report_start=str(panel.report_date.min().date()), report_end=str(panel.report_date.max().date()),
                    economic_month_end=str(panel.month.max().date()),
                    missing_report_dates=[str(d.date()) for d in pd.date_range(panel.report_date.min(),panel.report_date.max(),freq="MS")
                                          if d not in set(panel.report_date)],
                    tier1_rows=int(panel.tier1_mn.notna().sum()), conflicting_bank_months=len(conflict_keys),
                    tier1_economic_start=str(panel.loc[panel.tier1_mn.notna(), "month"].min().date()),
                    matching="exact NBU bank code and economic month; report date is next month start; no fill",
                    units="million UAH for capital and risk exposure; percentage points for ratios/headroom",
                    headroom_scope="general minimum benchmark, not full binding or supervisory headroom",
                    remaining="Legacy core is not relabelled Tier 1. Components workbook supplies capital levels; source conflicts retained and excluded from headroom estimation. Bank-specific requirements, buffers, new-bank exceptions and publication vintages are not supplied.",
                    input_sha256={p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in paths})
    (out/"capital_manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=2))
    return panel


def ensure(root):
    root = Path(root)
    out = root/"output_nbu/capital/tables/capital_manifest.json"
    paths = [root/"data_nbu/local_inputs"/f for f in FILES]
    cache = root/"data_nbu/parquet/mart_bank_prudential_v7.csv"
    if not all(p.is_file() for p in paths):
        raise FileNotFoundError("Both supplied capital workbooks are required in data_nbu/local_inputs")
    expected = {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}
    if not cache.exists() or not out.exists() or json.loads(out.read_text()).get("input_sha256") != expected or json.loads(out.read_text()).get("parser_version") != VERSION:
        return build(root)
    return pd.read_csv(cache, dtype={"nkb": str}, parse_dates=["month", "report_date"])


def attach(data, root):
    panel = ensure(root)
    data = data.copy()
    data["nkb"] = data.nkb.astype(str).str.replace(r"\.0$", "", regex=True)
    data["month"] = pd.to_datetime(data.month)
    data = data.drop(columns=[c for c in panel if c in data and c not in ("nkb", "month")])
    return data.merge(panel, on=["nkb", "month"], how="left", validate="many_to_one")


def enrich_totals(rows, root):
    """Workbook inputs use exact-date Tier 1 and never substitute book equity."""
    merged = attach(pd.DataFrame(rows), root)
    for col in ["month", "report_date"]:
        merged[col] = pd.to_datetime(merged[col]).dt.strftime("%Y-%m-%d")
    return merged.replace({np.nan: None}).to_dict("records")


if __name__ == "__main__":
    import sys
    frame = build(Path(sys.argv[1]) if len(sys.argv)>1 else Path(__file__).resolve().parent)
    print(f"Capital inputs ready, {len(frame)} bank-months")
