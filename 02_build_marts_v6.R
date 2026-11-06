# ============================================================================
# 02_build_marts_v6.R — OFFLINE build: raw -> staging -> marts (no network)
# ----------------------------------------------------------------------------
# Inputs  (data_nbu/parquet): raw_osb, raw_banksfinrep, raw_key, raw_mir,
#          raw_deposit, raw_inflation, raw_liquidity, local_nss_lc_params,
#          local_ns_fc_params, [raw_fx], [raw_uonia], [raw_ovdp]
# Outputs (data_nbu/parquet): stg_osb_v6, mart_bank_month_v6,
#          mart_system_month_v6, mart_system_product_v6, macro_monthly_v6
#          + output_nbu/logs/build_v6_*.csv diagnostics
#
# Methodological corrections relative to v5.2
#   (1) NET turnovers for flows (debit - credit for expenses; credit - debit for
#       income). Gross debit turnover double-counts storno entries.
#   (2) Day-count annualisation: r = (days_in_year / days_in_month) * flow / avg.
#   (3) Average stock uses the CALENDAR lag (t-1 month), not the previous row.
#   (4) System rates aggregate flows and average stocks over a MATCHED sample
#       (bank-months where both the flow and the average stock exist).
#   (5) Complete account groups 260/262 (+2606, 2621, 2624), NBFI deposits,
#       wholesale funding, loans, securities (account_map_v6.R).
#   (6) Economic month = report date - 1 month for every monthly NBU block.
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "account_map_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_local_inputs_v6.R"), encoding = "UTF-8")

MIN_AVG_STOCK <- 1      # mn UAH; smaller average stocks give meaningless ratios
RATE_BOUNDS   <- c(-0.02, 0.60)  # bank-level rate outside -> flagged (not dropped from system sums)

econ_month <- function(dt) add_months(month_floor(parse_nbu_date(dt)), -1L)

# =========================== 1. OSB STAGING ==================================
log_msg("BUILD v6: reading raw_osb")
osb <- store_read("raw_osb")
if (!nrow(osb)) stop("raw_osb is empty — run 01_nbu_download_v6.R first")
osb <- osb[, .(dt = as.character(dt), nkb = as.character(nkb),
               r020 = sprintf("%04d", as.integer(r020)),
               r034 = as.integer(r034), t025 = as.integer(t025), value = num(value))]
osb <- osb[r034 %in% c(1L, 2L) & t025 %in% c(1L, 2L, 5L, 6L)]
osb[, month := econ_month(dt)]
osb <- osb[month >= START_MONTH]

w <- dcast(osb, month + nkb + r020 + r034 ~ t025, value.var = "value", fun.aggregate = sum, fill = 0)
setnames(w, c("1", "2", "5", "6"), c("bal_a", "bal_p", "turn_d", "turn_c"), skip_absent = TRUE)
for (v in c("bal_a", "bal_p", "turn_d", "turn_c")) if (!v %in% names(w)) w[, (v) := 0]

# Diagnostic: gross vs net turnover on mapped flow accounts.
flow_acc <- ACCOUNT_MAP_V6[rule %in% c("EXP", "INC"), account]
storno <- w[r020 %in% flow_acc, .(gross_debit = sum(turn_d), gross_credit = sum(turn_c)), by = .(month, r020)]
fwrite(storno, file.path(LOG_DIR, "build_v6_flow_turnovers_by_account.csv"))

m <- merge(w, ACCOUNT_MAP_V6[, .(r020 = account, variable, rule)], by = "r020", allow.cartesian = TRUE)
m[, value := fcase(rule == "P",    bal_p,
                   rule == "A",    bal_a,
                   rule == "NETA", bal_a - bal_p,
                   rule == "EXP",  turn_d - turn_c,
                   rule == "INC",  turn_c - turn_d,
                   rule == "C",    turn_c)]
m[, ccy := fifelse(r034 == 1L, "uah", "fx")]
stg <- m[, .(value = sum(value)), by = .(month, nkb, variable, ccy)]
store_write(stg, "stg_osb_v6")

# Account coverage diagnostic: which mapped accounts actually exist in raw_osb.
cov <- merge(ACCOUNT_MAP_V6, osb[, .(rows = .N, first = min(month), last = max(month)), by = .(account = r020)],
             by = "account", all.x = TRUE)
fwrite(cov, file.path(LOG_DIR, "build_v6_account_coverage.csv"))
miss_vars <- cov[, .(present = any(!is.na(rows))), by = variable][present == FALSE, variable]
if (length(miss_vars)) log_msg("Variables with NO downloaded accounts yet (run 01_*): ", paste(miss_vars, collapse = ", "))

# =========================== 2. BANK x MONTH PANEL ===========================
# Complete calendar grid within each bank's observed span; missing account
# rows inside the span are true zeros (OSB omits zero balances/turnovers).
span <- osb[, .(first = min(month), last = max(month)), by = nkb]
grid <- span[, .(month = seq(first, last, by = "month")), by = nkb]

wide <- dcast(stg, month + nkb ~ variable + ccy, value.var = "value", fill = 0)
all_vars <- unique(ACCOUNT_MAP_V6$variable)
for (v in all_vars) for (cc in c("uah", "fx")) {
  nm <- paste0(v, "_", cc); if (!nm %in% names(wide)) wide[, (nm) := NA_real_]   # NA = account not downloaded
}
present <- unique(stg$variable)
bank <- merge(grid, wide, by = c("nkb", "month"), all.x = TRUE)
for (v in present) for (cc in c("uah", "fx")) {
  nm <- paste0(v, "_", cc); set(bank, which(is.na(bank[[nm]])), nm, 0)
}
for (v in all_vars) bank[, (paste0(v, "_total")) := get(paste0(v, "_uah")) + get(paste0(v, "_fx"))]
# NB: for class-6/7 flows *_uah holds the full amount (P&L is kept in UAH);
#     totals of flows are recomputed in section 2b.
setorder(bank, nkb, month)

# ---- 2b. Currency split of interest expense ---------------------------------
# P&L accounts exist only in UAH (r034 = 1): exp_*_uah currently holds the
# WHOLE expense. Split it using the currency structure of the accrual of
# interest payable (credit turnover on 2608/2618/2628/2638/2658), corrected
# for the revaluation of FX accrued-interest balances.
fxr <- store_read("raw_fx")
fx_growth <- if (nrow(fxr)) {
  g <- fxr[cc %in% c("USD", "EUR"), .(eom = rate[which.max(as.Date(date))]), by = .(month = month_floor(as.Date(date)), cc)]
  g <- dcast(g, month ~ cc, value.var = "eom"); setorder(g, month)
  g[, fx_g := 0.7 * (USD / shift(USD) - 1) + 0.3 * (EUR / shift(EUR) - 1)]
  g[, .(month, fx_g)]
} else data.table(month = as.Date(character()), fx_g = numeric())
bank <- merge(bank, fx_growth, by = "month", all.x = TRUE)
setorder(bank, nkb, month)
for (i in seq_len(nrow(FUNDING_PRODUCTS))) {
  p <- FUNDING_PRODUCTS[i]
  fe <- p$flow; ac <- paste0("accrC_", p$accr); ab <- paste0("accrB_", p$accr)
  total_exp <- bank[[paste0(fe, "_uah")]] + fcoalesce(bank[[paste0(fe, "_fx")]], 0)
  bank[, (paste0(fe, "_total")) := total_exp]
  if (all(paste0(c(ac, ab), "_fx") %in% names(bank)) && !all(is.na(bank[[paste0(ac, "_fx")]]))) {
    bank[, reval_tmp := pmax(0, fcoalesce(shift(get(paste0(ab, "_fx"))), 0) * fcoalesce(fx_g, 0)), by = nkb]
    bank[, c_fx_tmp := pmax(0, get(paste0(ac, "_fx")) - reval_tmp)]
    bank[, fxw_tmp := safe_div(c_fx_tmp, c_fx_tmp + pmax(0, get(paste0(ac, "_uah"))))]
    bank[, (paste0(fe, "_fx"))  := fxw_tmp * get(paste0(fe, "_total"))]
    bank[, (paste0(fe, "_uah")) := get(paste0(fe, "_total")) - get(paste0(fe, "_fx"))]
    bank[, (paste0("fxsplit_", p$product)) := fxw_tmp]
    bank[, c("reval_tmp", "c_fx_tmp", "fxw_tmp") := NULL]
  } else {
    bank[, (paste0(fe, "_fx")) := NA_real_]
    bank[, (paste0(fe, "_uah")) := NA_real_]
  }
}
# Remaining expense/income variables are total-only by construction.
for (v in setdiff(grep("^(exp_|inc_)", all_vars, value = TRUE), FUNDING_PRODUCTS$flow)) {
  bank[, (paste0(v, "_total")) := get(paste0(v, "_uah")) + fcoalesce(get(paste0(v, "_fx")), 0)]
}
CCY_SPLIT_AVAILABLE <- !all(is.na(bank$exp_hh_term_fx))
# ---- 2c. Product reclassification events -----------------------------------
# A bank sometimes moves balances between demand and term accounts of the same
# sector (e.g. nkb 46, 2023-07: ~23 bn UAH of household savings accounts moved
# from 2630 to 2620 and the month's interest booked on 7040 instead of 7041).
# Totals are unaffected, but product rates spike. Detection: opposite stock
# moves within a sector, each > 5% of the sector stock, roughly offsetting,
# AND a jump (> 30 pp) in the share of the sector's interest booked on demand.
# In event months the sector's interest expense is re-allocated between the
# two products with the bank's previous-month product rates (sector total kept).
setorder(bank, nkb, month)
RECLASS <- list()
for (sec in c("corp", "hh")) {
  sd <- paste0("dep_", sec, "_demand_total"); st <- paste0("dep_", sec, "_term_total")
  ed <- paste0("exp_", sec, "_demand_total"); et <- paste0("exp_", sec, "_term_total")
  bank[, `:=`(dd_ = get(sd) - shift(get(sd)), dt_ = get(st) - shift(get(st)), base_ = shift(get(sd)) + shift(get(st)),
              rd_l = shift(get(ed)) / ((shift(get(sd)) + shift(get(sd), 2)) / 2),
              rt_l = shift(get(et)) / ((shift(get(st)) + shift(get(st), 2)) / 2),
              fs_  = get(ed) / (get(ed) + get(et)), fs_l = shift(get(ed) / (get(ed) + get(et)))), by = nkb]
  bank[, ev_ := is.finite(dd_ * dt_) & dd_ * dt_ < 0 & pmin(abs(dd_), abs(dt_)) > 0.05 * base_ &
                abs(dd_ + dt_) < 0.5 * pmin(abs(dd_), abs(dt_)) & base_ > 1000 &
                is.finite(fs_ - fs_l) & abs(fs_ - fs_l) > 0.30]   # booking of interest jumps between products
  ev <- bank[ev_ == TRUE, .(nkb, month, sector = sec, d_demand = dd_, d_term = dt_, sector_stock = base_,
                            exp_demand_raw = get(ed), exp_term_raw = get(et))]
  if (nrow(ev)) {
    bank[ev_ == TRUE & is.finite(rd_l) & is.finite(rt_l), tot_ := get(ed) + get(et)]
    bank[ev_ == TRUE & is.finite(rd_l) & is.finite(rt_l), `:=`(
      share_d_ = (rd_l * (get(sd) + (get(sd) - dd_)) / 2) /
                 (rd_l * (get(sd) + (get(sd) - dd_)) / 2 + rt_l * (get(st) + (get(st) - dt_)) / 2))]
    bank[ev_ == TRUE & is.finite(share_d_), (ed) := share_d_ * tot_]
    bank[ev_ == TRUE & is.finite(share_d_), (et) := (1 - share_d_) * tot_]
    for (cc in c("uah", "fx")) for (v in c(ed, et)) {
      vv <- sub("_total$", paste0("_", cc), v)
      if (vv %in% names(bank)) bank[ev_ == TRUE, (vv) := NA_real_]   # currency split unreliable in event months
    }
    RECLASS[[sec]] <- ev
  }
  bank[, intersect(c("dd_", "dt_", "base_", "rd_l", "rt_l", "fs_", "fs_l", "ev_", "tot_", "share_d_"), names(bank)) := NULL]
}
RECLASS <- rbindlist(RECLASS)
fwrite(RECLASS, file.path(LOG_DIR, "build_v6_reclassification_events.csv"))
log_msg("Product reclassification events detected: ", nrow(RECLASS))

log_msg("Currency split of deposit interest expense available: ", CCY_SPLIT_AVAILABLE)

# Derived stock aggregates.
# Sum of components; components whose accounts were never downloaded are NA
# and skipped; the result is NA only if all components are NA.
sum_vars <- function(dt, vars, out) {
  for (cc in c("uah", "fx", "total")) {
    cols <- paste0(vars, "_", cc)
    dt[, (paste0(out, "_", cc)) := {
      m <- as.matrix(.SD); r <- rowSums(m, na.rm = TRUE); r[rowSums(!is.na(m)) == 0] <- NA_real_; r
    }, .SDcols = cols]
  }
}
sum_vars(bank, c("dep_corp_demand", "dep_corp_term"), "dep_corp")
sum_vars(bank, c("dep_hh_demand", "dep_hh_term"), "dep_hh")
sum_vars(bank, c("dep_corp_demand", "dep_hh_demand"), "dep_demand4")
sum_vars(bank, c("dep_corp_term", "dep_hh_term"), "dep_term4")
sum_vars(bank, c("dep_corp", "dep_hh"), "dep_core4")
sum_vars(bank, c("dep_nbfi_demand", "dep_nbfi_term"), "dep_nbfi")
sum_vars(bank, c("dep_core4", "dep_nbfi"), "dep_core6")
sum_vars(bank, c("exp_corp_demand", "exp_corp_term", "exp_hh_demand", "exp_hh_term"), "exp_core4")
sum_vars(bank, c("exp_core4", "exp_nbfi_demand", "exp_nbfi_term"), "exp_core6")
sum_vars(bank, c("ovdp", "refin_other", "refin_adj"), "refin_sec")
sum_vars(bank, c("loans_corp", "loans_corp_od", "loans_nbfi_od"), "loans_corp_all")
sum_vars(bank, c("loans_hh", "loans_hh_od"), "loans_hh_all")
sum_vars(bank, c("sec_other", "sec_other_adj"), "sec_other_all")
sum_vars(bank, c("inc_loans_corp", "inc_loans_nbfi_od"), "inc_loans_corp_all")

# Average stocks with CALENDAR lag.
stock_bases <- c(paste0("dep_", c("corp_demand", "corp_term", "hh_demand", "hh_term", "nbfi_demand", "nbfi_term",
                                  "corp", "hh", "demand4", "term4", "core4", "nbfi", "core6", "budget")),
                 "cd_nbu", "ovdp", "refin_sec", "loans_corp_all", "loans_hh_all", "loans_gov",
                 "interbank_assets", "nbu_placements", "sec_other_all",
                 "liab_nbu", "liab_interbank", "liab_ifi", "liab_own_sec", "liab_subdebt", "repo_clients", "savings_certs")
bank[, month_l1 := add_months(month, -1L)]
lag_tab <- bank[, c("nkb", "month", unlist(lapply(stock_bases, paste0, c("_uah", "_fx", "_total")))), with = FALSE]
setnames(lag_tab, names(lag_tab)[-(1:2)], paste0(names(lag_tab)[-(1:2)], "_l1"))
setnames(lag_tab, "month", "month_l1")
bank <- merge(bank, lag_tab, by = c("nkb", "month_l1"), all.x = TRUE)
for (b in stock_bases) for (cc in c("uah", "fx", "total")) {
  v <- paste0(b, "_", cc)
  bank[, (paste0("avg_", v)) := (get(v) + get(paste0(v, "_l1"))) / 2]
}
bank[, (grep("_l1$", names(bank), value = TRUE)) := NULL]
bank[, ann := ann_factor(month)]

rate_fun <- function(flow, avg) ifelse(is.finite(flow) & is.finite(avg) & avg >= MIN_AVG_STOCK, flow / avg, NA_real_)

# Funding product rates (6 products x 3 currency cuts) and aggregates.
for (i in seq_len(nrow(FUNDING_PRODUCTS))) {
  p <- FUNDING_PRODUCTS[i]
  for (cc in c("uah", "fx", "total"))
    bank[, (paste0("r_", p$product, "_", cc)) := ann * rate_fun(get(paste0(p$flow, "_", cc)), get(paste0("avg_", p$stock, "_", cc)))]
}
for (cc in c("uah", "fx", "total")) {
  bank[, (paste0("cod4_", cc)) := ann * rate_fun(get(paste0("exp_core4_", cc)), get(paste0("avg_dep_core4_", cc)))]
  bank[, (paste0("cod6_", cc)) := ann * rate_fun(get(paste0("exp_core6_", cc)), get(paste0("avg_dep_core6_", cc)))]
}
# Asset yields.
yield_pairs <- list(cd = c("inc_cd_nbu", "cd_nbu"), refin = c("inc_refin_sec", "refin_sec"),
                    loans_corp = c("inc_loans_corp_all", "loans_corp_all"), loans_hh = c("inc_loans_hh", "loans_hh_all"),
                    loans_gov = c("inc_loans_gov", "loans_gov"), interbank = c("inc_interbank", "interbank_assets"),
                    nbu_dep = c("inc_nbu", "nbu_placements"), sec_other = c("inc_sec_other", "sec_other_all"))
# A yield is computed only if the CORE principal accounts were downloaded
# (otherwise e.g. loan income would be divided by overdrafts only).
core_needed <- list(cd = "cd_nbu", refin = "ovdp", loans_corp = "loans_corp", loans_hh = "loans_hh",
                    loans_gov = "loans_gov", interbank = "interbank_assets", nbu_dep = "nbu_placements", sec_other = "sec_other")
yield_pairs <- yield_pairs[vapply(names(yield_pairs), function(nm) core_needed[[nm]] %in% present, logical(1))]
log_msg("Asset yields computed for: ", paste(names(yield_pairs), collapse = ", "))
for (nm in names(yield_pairs)) for (cc in c("uah", "fx", "total")) {
  f <- paste0(yield_pairs[[nm]][1], "_", cc); s <- paste0("avg_", yield_pairs[[nm]][2], "_", cc)
  bank[, (paste0("y_", nm, "_", cc)) := ann * rate_fun(get(f), get(s))]
}

# Structure ratios (month-end stocks).
bank[, `:=`(
  demand_share   = safe_div(dep_demand4_total, dep_core4_total),
  term_share     = safe_div(dep_term4_total,   dep_core4_total),
  fx_share       = safe_div(dep_core4_fx,      dep_core4_total),
  hh_share       = safe_div(dep_hh_total,      dep_core4_total),
  cd_to_dep      = safe_div(cd_nbu_total,      dep_core4_total),
  ovdp_to_dep    = safe_div(ovdp_total,        dep_core4_total),
  refin_to_dep   = safe_div(refin_sec_total,   dep_core4_total),
  loans_to_dep   = safe_div(loans_corp_all_total + loans_hh_all_total, dep_core4_total)
)]
# Quality flags.
rate_cols <- grep("^(r_|cod[46]_|y_)", names(bank), value = TRUE)
bank[, dq_rate_out_of_bounds := Reduce(`|`, lapply(.SD, function(z) is.finite(z) & (z < RATE_BOUNDS[1] | z > RATE_BOUNDS[2]))),
     .SDcols = rate_cols]
bank[, dq_negative_flow := (exp_core4_total < -0.05)]

# =========================== 3. BANK METADATA (banksfinrep) ==================
fin <- store_read("raw_banksfinrep")
if (nrow(fin)) {
  fin <- fin[, .(dt = as.character(dt), nkb = as.character(nkb), id_api, value = num(value),
                 fullname = as.character(fullname), gr_bank = as.character(gr_bank))]
  fin[, month := econ_month(dt)]
  meta <- fin[, .(fullname = na.omit(fullname)[1], gr_bank = na.omit(gr_bank[nzchar(gr_bank)])[1]), by = .(month, nkb)]
  keep_ids <- c(assets = "BS1_AssetsTotal", equity = "BS1_CapitalTotal", liab_cust = "BS1_LiabCust",
                loans_net = "BS1_AssetsLoansClients", nii_ytd = "BS1_NetInterIncomeCosts",
                int_inc_ytd = "BS1_InterestIncome", int_exp_ytd = "BS1_InterestCosts", profit_ytd = "BS1_ProfitLossAfterTax")
  fv <- fin[id_api %in% keep_ids, .(value = sum(value, na.rm = TRUE)), by = .(month, nkb, id_api)]
  fv[, var := names(keep_ids)[match(id_api, keep_ids)]]
  fv <- dcast(fv, month + nkb ~ var, value.var = "value")
  setnames(fv, setdiff(names(fv), c("month", "nkb")), paste0("fin_", setdiff(names(fv), c("month", "nkb"))))
  # YTD P&L -> monthly flows
  setorder(fv, nkb, month)
  for (v in intersect(c("fin_nii_ytd", "fin_int_inc_ytd", "fin_int_exp_ytd", "fin_profit_ytd"), names(fv))) {
    mv <- sub("_ytd$", "_m", v)
    fv[, (mv) := fifelse(month(month) == 1L, get(v), get(v) - shift(get(v))), by = nkb]
    fv[month(month) != 1L & shift(month, type = "lag") != add_months(month, -1L), (mv) := NA_real_, by = nkb]
  }
  bank <- merge(bank, merge(meta, fv, by = c("month", "nkb"), all = TRUE), by = c("month", "nkb"), all.x = TRUE)
  bank[, owner := fcase(gr_bank == "A", "State", gr_bank == "B", "Foreign", gr_bank == "E", "Private", default = NA_character_)]
}

# Size quartiles by total assets (fallback: core deposits), within month.
if (!"fin_assets" %in% names(bank)) bank[, fin_assets := NA_real_]
bank[, size_base := fifelse(is.finite(fin_assets) & fin_assets > 0, fin_assets, dep_core4_total)]
bank[, size_q := {
  r <- frank(size_base, ties.method = "average", na.last = "keep"); n <- sum(!is.na(r))
  if (n >= 8) as.integer(pmin(4, ceiling(4 * r / n))) else NA_integer_
}, by = month]

# =========================== 4. SYSTEM AGGREGATES (matched sample) ===========
sys_rate <- function(flow, stock, ann) {
  ok <- is.finite(flow) & is.finite(stock) & stock >= MIN_AVG_STOCK
  list(flow = sum(flow[ok]), avg_stock = sum(stock[ok]), n = sum(ok), rate = ann[1] * sum(flow[ok]) / sum(stock[ok]))
}
prod_long <- rbindlist(lapply(seq_len(nrow(FUNDING_PRODUCTS)), function(i) {
  p <- FUNDING_PRODUCTS[i]
  rbindlist(lapply(c("uah", "fx", "total"), function(cc) {
    bank[, sys_rate(get(paste0(p$flow, "_", cc)), get(paste0("avg_", p$stock, "_", cc)), ann), by = month][
      , `:=`(product = p$product, ccy = cc, sector = p$sector, maturity = p$maturity, core4 = p$core4)]
  }))
}))
# Month-end stocks (all banks) for structure.
prod_stock <- rbindlist(lapply(seq_len(nrow(FUNDING_PRODUCTS)), function(i) {
  p <- FUNDING_PRODUCTS[i]
  rbindlist(lapply(c("uah", "fx", "total"), function(cc)
    bank[, .(stock_eom = sum(get(paste0(p$stock, "_", cc)), na.rm = TRUE)), by = month][, `:=`(product = p$product, ccy = cc)]))
}))
prod_long <- merge(prod_long, prod_stock, by = c("month", "product", "ccy"), all = TRUE)
setorder(prod_long, product, ccy, month)
store_write(prod_long, "mart_system_product_v6")

system <- bank[, .(n_banks = uniqueN(nkb)), by = month]
agg_pairs <- c(list(cod4 = c("exp_core4", "dep_core4"), cod6 = c("exp_core6", "dep_core6")), yield_pairs)
for (nm in names(agg_pairs)) for (cc in c("uah", "fx", "total")) {
  f <- paste0(agg_pairs[[nm]][1], "_", cc); s <- paste0("avg_", agg_pairs[[nm]][2], "_", cc)
  z <- bank[, sys_rate(get(f), get(s), ann), by = month]
  pre <- if (nm %in% c("cod4", "cod6")) nm else paste0("y_", nm)
  setnames(z, c("flow", "avg_stock", "n", "rate"), paste0(pre, "_", cc, c("_flow", "_avgstock", "_nbanks", "")))
  system <- merge(system, z, by = "month", all.x = TRUE)
}
stock_cols <- c(paste0(rep(c("dep_corp_demand", "dep_corp_term", "dep_hh_demand", "dep_hh_term", "dep_nbfi_demand",
                             "dep_nbfi_term", "dep_core4", "dep_demand4", "dep_term4", "dep_hh", "dep_corp", "dep_budget",
                             "cd_nbu", "ovdp", "refin_sec", "loans_corp_all", "loans_hh_all", "loans_gov",
                             "liab_nbu", "liab_interbank", "liab_own_sec", "liab_subdebt", "nbu_corr_reserves"), each = 3),
                  c("_uah", "_fx", "_total")))
stock_cols <- intersect(stock_cols, names(bank))
system <- merge(system, bank[, lapply(.SD, sum, na.rm = TRUE), by = month, .SDcols = stock_cols], by = "month")
system[, `:=`(demand_share = dep_demand4_total / dep_core4_total, term_share = dep_term4_total / dep_core4_total,
              fx_share = dep_core4_fx / dep_core4_total, hh_share = dep_hh_total / dep_core4_total,
              cd_to_dep = cd_nbu_total / dep_core4_total, ovdp_to_dep = ovdp_total / dep_core4_total)]
# Concentration.
conc <- bank[dep_core4_total > 0, {
  s <- dep_core4_total / sum(dep_core4_total)
  .(hhi_dep = sum(s^2), top5_dep = sum(sort(s, decreasing = TRUE)[1:min(5, .N)]),
    n_eff = 1 / sum(s^2))
}, by = month]
system <- merge(system, conc, by = "month", all.x = TRUE)

# Reconciliation with financial statements (coverage of OSB deposit mapping).
if ("fin_liab_cust" %in% names(bank)) {
  rec <- bank[, .(osb_core6 = sum(dep_core6_total, na.rm = TRUE), osb_core4 = sum(dep_core4_total, na.rm = TRUE),
                  fin_liab_cust = sum(fin_liab_cust, na.rm = TRUE)), by = month]
  rec[, `:=`(coverage_core4 = osb_core4 / fin_liab_cust, coverage_core6 = osb_core6 / fin_liab_cust)]
  fwrite(rec, file.path(LOG_DIR, "build_v6_reconciliation_osb_vs_finrep.csv"))
  system <- merge(system, rec[, .(month, fin_liab_cust, coverage_core4, coverage_core6)], by = "month", all.x = TRUE)
}

# =========================== 5. MACRO / MARKET MONTHLY =======================
macro <- data.table(month = seq(add_months(START_MONTH, -1L), month_floor(Sys.Date()), by = "month"))

# 5.1 NBU rates (daily -> monthly; day-weighted average and end-of-month).
key <- store_read("raw_key")
if (nrow(key)) {
  key <- key[, .(date = parse_nbu_date(dt), id_api, value = num(value))][is.finite(value)]
  key <- key[, .(value = value[.N]), by = .(date, id_api)]
  # forward-fill to calendar days
  cal <- CJ(date = seq(min(key$date), max(key$date), by = "day"), id_api = unique(key$id_api))
  key <- key[cal, on = .(date, id_api), roll = TRUE]
  key[, month := month_floor(date)]
  kmap <- c(KEY_PolicyRate = "policy", KEY_RateOvernightCD = "cd_on_rate", KEY_RateOvernightLoans = "refi_on_rate")
  km <- key[id_api %in% names(kmap), .(avg = mean(value) / 100, eom = value[which.max(date)] / 100), by = .(month, id_api)]
  km[, v := kmap[id_api]]
  km <- dcast(km, month ~ v, value.var = c("avg", "eom"))
  setnames(km, names(km), sub("^(avg|eom)_(.*)$", "\\2_\\1", names(km)))
  macro <- merge(macro, km, by = "month", all.x = TRUE)
}

# 5.2 UONIA (API table from 01; if empty — local NBU xlsx "Money market: the dynamics of UONIA").
uon <- store_read("raw_uonia")
if (!nrow(uon)) { ul <- tryCatch(read_uonia_local(), error = function(e) NULL); if (!is.null(ul)) uon <- ul[, .(date = as.character(date), value, id_api)] }
if (nrow(uon)) {
  # API: keep the UONIA rate itself (other ids are volumes / counts); local xlsx rows are the rate (UONIA / Pre-UONIA)
  if ("id_api" %in% names(uon) && any(grepl("UnsecLoansDepo|local_xlsx|UONIA_api", uon$id_api))) uon <- uon[grepl("UnsecLoansDepo|local_xlsx|UONIA_api", id_api)]
  uon <- unique(uon, by = "date")
  uon[, date := parse_nbu_date(date)]
  um <- uon[is.finite(num(value)), .(uonia_avg = mean(num(value)) / 100, uonia_eom = num(value)[which.max(date)] / 100),
            by = .(month = month_floor(date))]
  macro <- merge(macro, um, by = "month", all.x = TRUE)
}

# 5.3 Official FX rates (month-end, average) and an FX-deposit basket index.
fx <- store_read("raw_fx")
if (nrow(fx)) {
  fx[, date := as.Date(date)]
  fx[cc %in% c("USD", "EUR") & rate > 500, rate := rate / 100]       # some API/xlsx dumps quote per 100 units
  fm <- fx[cc %in% c("USD", "EUR"), .(avg = mean(rate), eom = rate[which.max(date)]), by = .(month = month_floor(date), cc)]
  fm <- dcast(fm, month ~ cc, value.var = c("eom", "avg"))
  setnames(fm, names(fm), tolower(sub("^(eom|avg)_(.*)$", "\\2_\\1", names(fm))))
  macro <- merge(macro, fm, by = "month", all.x = TRUE)
}

# 5.4 Currency composition of FX deposits (monetary statistics) for valuation.
dep <- unique(store_read("raw_deposit"))
if (nrow(dep)) {
  dz <- dep[id_api %in% c("Deposits_Households", "Deposits_NFinCorp") & odkodter == "total" & odk111 == "total" &
              odk051 == "total" & ods183dd == "total" & odr030 %in% c("01", "02", "03", "04", "99", "total")]
  dz <- dz[, .(value = sum(num(value))), by = .(month = econ_month(dt), sector = id_api, odr030)]
  dzw <- dcast(dz[, .(value = sum(value)), by = .(month, odr030)], month ~ odr030, value.var = "value")
  setnames(dzw, c("01", "02", "03", "04", "99", "total"), c("ms_dep_uah", "ms_dep_fx", "ms_dep_usd", "ms_dep_eur", "ms_dep_oth", "ms_dep_total"), skip_absent = TRUE)
  dzw[, `:=`(ms_usd_in_fx = ms_dep_usd / ms_dep_fx, ms_eur_in_fx = ms_dep_eur / ms_dep_fx)]
  # Maturity structure of HH + NFC deposits (all currencies): 10 = demand, 20 = <1y, 30 = 1-2y, 40 = >2y.
  dm <- dep[id_api %in% c("Deposits_Households", "Deposits_NFinCorp") & odkodter == "total" & odk111 == "total" &
              odk051 == "total" & odr030 == "total" & ods183dd %in% c("10", "20", "30", "40", "total")]
  dm <- dcast(dm[, .(value = sum(num(value))), by = .(month = econ_month(dt), ods183dd)], month ~ ods183dd, value.var = "value")
  setnames(dm, c("10", "20", "30", "40", "total"), c("ms_mat_demand", "ms_mat_lt1y", "ms_mat_1_2y", "ms_mat_gt2y", "ms_mat_total"), skip_absent = TRUE)
  macro <- merge(macro, merge(dzw, dm, by = "month", all = TRUE), by = "month", all.x = TRUE)
}
if (all(c("usd_eom", "eur_eom", "ms_usd_in_fx") %in% names(macro))) {
  # Basket valuation index of FX deposits in UAH, weights = USD/EUR shares in FX deposits (lagged).
  setorder(macro, month)
  macro[, `:=`(w_usd = nafill(shift(ms_usd_in_fx), "locf"), w_eur = nafill(shift(ms_eur_in_fx), "locf"))]
  macro[, `:=`(w_usd = fcoalesce(w_usd, 0.7), w_eur = fcoalesce(w_eur, 0.3))]
  macro[, fx_basket_dlog := w_usd / (w_usd + w_eur) * log(usd_eom / shift(usd_eom)) +
                            w_eur / (w_usd + w_eur) * log(eur_eom / shift(eur_eom))]
}

# 5.5 CPI (y/y and m/m), core CPI.
inf <- store_read("raw_inflation")
if (nrow(inf)) {
  ci <- inf[id_api %in% c("prices_price_cpi_", "prices_price_ci_") & (is.na(ku) | ku %in% c("", "NULL")) &
              mcrd081 == "Total" & tzep %in% c("PCCM_", "PCPM_")]
  ci <- ci[, .(value = num(value)[1] / 100), by = .(month = econ_month(dt), id_api, tzep)]
  ci[, v := paste0(fifelse(id_api == "prices_price_cpi_", "cpi", "core_cpi"), fifelse(tzep == "PCCM_", "_yoy", "_mom"))]
  macro <- merge(macro, dcast(ci, month ~ v, value.var = "value"), by = "month", all.x = TRUE)
}

# 5.6 MIR new-business rates (marginal pricing).  ods180: 10 demand, 20 <1y, 30 1-2y, 40 >2y.
mir <- unique(store_read("raw_mir"))
if (nrow(mir)) {
  mz <- mir[id_api %in% c("New_Deposits_Households", "New_Deposits_NFinCorp", "New_Loans_Households", "New_Loans_NFinCorp") &
              odkodter == "total" & odk111 == "total" & odf074 == "total" & odr030 %in% c("01", "02", "total")]
  # k140 splits new loans by borrower size ("#" = all borrowers); deposits carry only "#".
  # Earlier versions took the first row of the group, i.e. a random size class for ~1/3 of loan cells.
  if ("k140" %in% names(mz)) mz <- mz[is.na(k140) | k140 %in% c("#", "total", "")]
  mz <- unique(mz, by = c("dt", "id_api", "odr030", "ods180", "tzep"))
  mz <- mz[, .(value = num(value)[1]), by = .(month = econ_month(dt), id_api, odr030, ods180, tzep)]
  mz <- dcast(mz, month + id_api + odr030 + ods180 ~ tzep, value.var = "value")
  setnames(mz, c("f_m_ir", "f_m_v"), c("rate", "vol"), skip_absent = TRUE)
  mz[, rate := rate / 100]
  sec <- c(New_Deposits_Households = "nb_dep_hh", New_Deposits_NFinCorp = "nb_dep_nfc",
           New_Loans_Households = "nb_loan_hh", New_Loans_NFinCorp = "nb_loan_nfc")
  cur <- c("01" = "uah", "02" = "fx", total = "total")
  # Deposits: demand (10) and volume-weighted term (20,30,40).
  dd <- mz[grepl("Deposits", id_api)]
  nb <- rbind(
    dd[ods180 == "10", .(month, id_api, odr030, bucket = "demand", rate)],
    dd[ods180 %in% c("20", "30", "40"), .(rate = wmean(rate, vol), bucket = "term"), by = .(month, id_api, odr030)],
    dd[ods180 == "20", .(month, id_api, odr030, bucket = "lt1y", rate)],
    mz[grepl("Loans", id_api) & ods180 == "total", .(month, id_api, odr030, bucket = "all", rate)],
    fill = TRUE)
  nb[, v := paste(sec[id_api], bucket, cur[odr030], sep = "_")]
  macro <- merge(macro, dcast(nb, month ~ v, value.var = "rate"), by = "month", all.x = TRUE)
}

# 5.7 OVDP primary auctions (UAH, volume-weighted by month; maturity buckets).
ov <- store_read("raw_ovdp")
if (nrow(ov)) {
  pick <- function(cands) intersect(cands, names(ov))[1]
  dcol <- pick(c("AuctionDate", "auctiondate")); ycol <- pick(c("AvgLevel", "avglevel", "IncomeLevel"))
  vcol <- pick(c("Attraction", "VolumeSold", "VolumeSoldN")); mcol <- pick(c("DaysToRepay", "daystorepay"))
  ccol <- pick(c("ValCode", "valcode"))
  if (!is.na(dcol) && !is.na(ycol)) {
    o <- ov[, .(date = parse_nbu_date(get(dcol)), y = num(get(ycol)) / 100, v = num(get(vcol)),
                days = num(get(mcol)), ccy = toupper(get(ccol)))][is.finite(y) & y > 0]
    o[, bucket := fcase(days <= 365, "lt1y", days <= 3 * 365, "1_3y", default = "gt3y")]
    o[, month := month_floor(date)]
    om <- rbind(o[, .(y = wmean(y, pmax(v, 1)), v = sum(v, na.rm = TRUE), bucket = "all"), by = .(month, ccy)],
                o[, .(y = wmean(y, pmax(v, 1)), v = sum(v, na.rm = TRUE)), by = .(month, ccy, bucket)])
    om[, var := paste0("ovdp_", tolower(ccy), "_", bucket)]
    macro <- merge(macro, dcast(om, month ~ var, value.var = "y"), by = "month", all.x = TRUE)
    macro <- merge(macro, dcast(om[bucket == "all"], month ~ paste0(var, "_vol"), value.var = "v"), by = "month", all.x = TRUE)
  }
}

# 5.8 NBU zero-coupon curves (NSS for UAH, NS for FX). Units detected automatically.
ns_l1 <- function(p, tau) { z <- p / tau; ifelse(abs(z) < 1e-10, 1, (1 - exp(-z)) / z) }
ns_l2 <- function(p, tau) ns_l1(p, tau) - exp(-p / tau)
read_curve_params <- function(name, xlsx, ncol_expected) {
  x <- store_read(name)
  f <- list.files(c(LOCAL_DIR, PROJECT_DIR), pattern = paste0("^", xlsx, "$"), full.names = TRUE)
  if (length(f)) {
    z <- as.data.table(openxlsx::read.xlsx(f[1], sheet = 1, detectDates = FALSE))[, 1:ncol_expected]
    nm <- if (ncol_expected == 7) c("date", "beta0", "beta1", "beta2", "tau", "beta3", "tau1") else c("date", "beta0", "beta1", "beta2", "tau")
    setnames(z, nm)
    z[, date := { d <- suppressWarnings(as.numeric(as.character(date)))
                  fifelse(is.finite(d), as.Date(d, origin = "1899-12-30"), parse_nbu_date(date)) }]
    for (v in nm[-1]) z[, (v) := num(get(v))]
    z <- z[!is.na(date) & is.finite(beta0)]
    if (nrow(z) > nrow(x)) { x <- z; store_write(x, name) }
  }
  x
}
curve_from <- function(par, model, pref) {
  if (!nrow(par)) return(data.table())
  par <- copy(par); par[, date := parse_nbu_date(date)]
  for (v in intersect(c("beta0", "beta1", "beta2", "tau", "beta3", "tau1"), names(par))) par[, (v) := num(get(v))]
  mats <- c("3m" = .25, "6m" = .5, "1y" = 1, "2y" = 2, "3y" = 3, "5y" = 5, "7y" = 7, "10y" = 10)
  unit <- if (stats::median(abs(par$beta0 + par$beta1), na.rm = TRUE) > 1) 100 else 1   # percent vs decimal parameters
  for (k in names(mats)) {
    p <- mats[[k]]
    par[, (paste0(pref, "_", k)) := (beta0 + beta1 * ns_l1(p, tau) + beta2 * ns_l2(p, tau) +
                                       if (model == "NSS") beta3 * ns_l2(p, tau1) else 0) / unit]
  }
  par[, month := month_floor(date)]
  cols <- paste0(pref, "_", names(mats))
  eom <- par[order(date), .SD[.N], by = month, .SDcols = cols]
  avg <- par[, lapply(.SD, mean), by = month, .SDcols = cols]
  setnames(avg, cols, paste0(cols, "_avg"))
  merge(eom, avg, by = "month")
}
lc <- curve_from(read_curve_params("local_nss_lc_params", "NSS_parameters_LC.xlsx", 7), "NSS", "zc_uah")
fc <- curve_from(read_curve_params("local_ns_fc_params", "N_S_parameters_FC.xlsx", 5), "NS", "zc_fx")
if (nrow(lc)) macro <- merge(macro, lc, by = "month", all.x = TRUE)
if (nrow(fc)) macro <- merge(macro, fc, by = "month", all.x = TRUE)

# 5.9 System liquidity: CD stock and required reserves (daily -> monthly average).
liq <- unique(store_read("raw_liquidity"))
if (nrow(liq)) {
  lq <- liq[id_api %in% c("LIQ_CertDepNBU", "LIQ_BanksCorAcc", "LIQ_ResReq", "LIQ_RefBanks"),
            .(value = mean(num(value), na.rm = TRUE)), by = .(month = month_floor(parse_nbu_date(dt)), id_api)]
  lq[, id_api := tolower(sub("^LIQ_", "liq_", id_api))]
  macro <- merge(macro, dcast(lq, month ~ id_api, value.var = "value"), by = "month", all.x = TRUE)
}

# 5.9 UIRD (local file data_nbu/local_inputs/UIRD.xls from bank.gov.ua): daily index of posted
#     household deposit rates of the largest banks; no gap in 2023-02…2024-04 (unlike MIR).
uw <- uird_wide(read_uird_local())
if (!is.null(uw)) macro <- merge(macro, uw, by = "month", all.x = TRUE)

# 5.10 Full NBU interest-rate statistics (local "4-Financial_markets.xlsx"): new and outstanding
#      UAH/FX rates by sector and maturity, interbank, OVDP primary. Complete 2006–… (no MIR gap).
FMR <- tryCatch(read_fm_local(), error = function(e) { log_msg("FM: ", conditionMessage(e)); NULL })
if (!is.null(FMR) && nrow(FMR)) { store_write(FMR, "mart_fm_rates_long_v6"); macro <- merge(macro, fm_wide(FMR), by = "month", all.x = TRUE) }
# OVDP primary market: if the auction API returned nothing, take the monthly volume-weighted yields of
# table 4.2.3 (UAH OVDP, funds raised to the budget: all maturities and up to 1 year).
for (pr in list(c("ovdp_uah_all", "fm_ovdp_prim"), c("ovdp_uah_lt1y", "fm_ovdp_prim_1y")))
  if (pr[2] %in% names(macro) && (!pr[1] %in% names(macro) || !any(is.finite(macro[[pr[1]]])))) {
    macro[, (pr[1]) := get(pr[2])]; log_msg(sprintf("%s: з таблиці 4.2.3 статистики НБУ (%d міс.)", pr[1], sum(is.finite(macro[[pr[1]]])))) }
# MIR (API) and section 4 of the NBU statistics are the same survey: identical on the overlap
# (demand, <1y, loans: max |diff| < 0.01 pp; "term" ≈ <1y, max |diff| 0.5 pp). Months missing in MIR
# (e.g. a failed download) are filled from the published tables; the flag records how many.
FILL_FM <- c(nb_dep_hh_demand_uah = "fm_dep_hh_uah_dem", nb_dep_hh_lt1y_uah = "fm_dep_hh_uah_1y", nb_dep_hh_term_uah = "fm_dep_hh_uah_1y",
             nb_dep_nfc_demand_uah = "fm_dep_nfc_uah_dem", nb_dep_nfc_lt1y_uah = "fm_dep_nfc_uah_1y", nb_dep_nfc_term_uah = "fm_dep_nfc_uah_1y",
             nb_loan_nfc_all_uah = "fm_loan_nfc_uah", nb_loan_hh_all_uah = "fm_loan_hh_uah")
for (v in names(FILL_FM)) if (FILL_FM[[v]] %in% names(macro)) {
  if (!v %in% names(macro)) macro[, (v) := NA_real_]
  n0 <- macro[!is.finite(get(v)) & is.finite(get(FILL_FM[[v]])), .N]
  if (n0) { macro[!is.finite(get(v)), (v) := get(FILL_FM[[v]])]; log_msg(sprintf("MIR %s: %d months filled from Financial_markets", v, n0)) }
}

# 5.11 Expectations of economic agents (NBU surveys): inflation 12 m ahead (% -> share) and UAH/USD
#      12 m ahead, for banks, firms (quarterly), households (monthly) and financial analysts.
#      Quarterly groups are carried forward at most 2 months (to the next survey), never further.
locf2 <- function(x, k = 2L) { out <- x; last <- NA_real_; age <- Inf
  for (i in seq_along(x)) { if (is.finite(x[i])) { last <- x[i]; age <- 0 } else { age <- age + 1; if (age <= k) out[i] <- last } }; out }
for (spec in list(c("surveys?_price.*\\.xlsx$", "pie"), c("surveys?_rate.*\\.xlsx$", "fxe"))) {
  e <- tryCatch(read_survey_expect(spec[1], spec[2]), error = function(err) NULL)
  if (!is.null(e)) { macro <- merge(macro, e, by = "month", all.x = TRUE); setorder(macro, month)
    for (v in setdiff(names(e), "month")) macro[, (v) := locf2(get(v))] }
}
for (g in c("banks", "firms", "hh", "analysts")) {
  pv <- paste0("pie_", g); fv <- paste0("fxe_", g)
  if (pv %in% names(macro)) macro[, (pv) := get(pv) / 100]
  if (fv %in% names(macro) && "usd_avg" %in% names(macro)) macro[, (paste0("edep_", g)) := get(fv) / usd_avg - 1]   # expected 12-m depreciation
}
# consensus measures: mean over available groups (households weighted as one group)
pcols <- intersect(paste0("pie_", c("banks", "firms", "hh", "analysts")), names(macro))
if (length(pcols)) macro[, pie_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = pcols]
dcols <- intersect(paste0("edep_", c("banks", "firms", "hh", "analysts")), names(macro))
if (length(dcols)) macro[, edep_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = dcols]
for (v in c("pie_mean", "edep_mean")) if (v %in% names(macro)) macro[!is.finite(get(v)), (v) := NA_real_]

# 5.12 Bank Lending Survey (local BLS_*.xlsx; quarter -> its three months)
blw <- tryCatch(bls_wide(read_bls_local()), error = function(e) { log_msg("BLS: ", conditionMessage(e)); NULL })
if (!is.null(blw) && nrow(blw)) macro <- merge(macro, blw, by = "month", all.x = TRUE)

# 5.13 Monetary aggregates (API "monetary"; fallback: local *Monetary_FX*.xlsx, sheet monetary_raw)
mon <- store_read("raw_monetary"); if (!nrow(mon)) mon <- tryCatch(read_monetary_fallback(), error = function(e) NULL)
mw <- tryCatch(monetary_wide(mon), error = function(e) { log_msg("monetary: ", conditionMessage(e)); NULL })
if (!is.null(mw) && nrow(mw)) macro <- merge(macro, mw, by = "month", all.x = TRUE)

# 5.14 Business surveys (API): IODA monthly, BOS quarterly (fallback: local *Surveys_Expectations*.xlsx)
srv <- store_read("raw_survey"); if (!nrow(srv)) srv <- tryCatch(read_survey_fallback("monthly_raw"), error = function(e) NULL)
qsv <- store_read("raw_q_survey"); if (!nrow(qsv)) qsv <- tryCatch(read_survey_fallback("quarterly_raw"), error = function(e) NULL)
svw <- tryCatch(survey_wide(srv, qsv), error = function(e) NULL)
if (!is.null(svw) && nrow(svw)) macro <- merge(macro, svw, by = "month", all.x = TRUE)

# 5.15 Derived expectation-based measures (all in shares; see 09_expectations_money_v6.R)
setorder(macro, month)
if (all(c("policy_avg", "pie_analysts") %in% names(macro))) macro[, rr_policy := policy_avg - pie_analysts]          # ex-ante real policy rate
if (all(c("fm_dep_hh_uah_1y", "pie_hh") %in% names(macro))) macro[, rr_dep_hh := fm_dep_hh_uah_1y - pie_hh]         # households' ex-ante real deposit rate
if (all(c("fm_dep_hh_uah_1y", "fm_dep_hh_fx_1y", "edep_hh") %in% names(macro)))
  macro[, uip_hh := fm_dep_hh_uah_1y - fm_dep_hh_fx_1y - edep_hh]                                                     # expected excess return UAH vs FX deposit
if (all(c("zc_uah_1y_avg", "policy_avg") %in% names(macro))) macro[, exp_path_1y := zc_uah_1y_avg - policy_avg]     # market-implied expected policy change (+ term premium)
if (all(c("zc_uah_3m_avg", "policy_avg") %in% names(macro))) macro[, exp_path_3m := zc_uah_3m_avg - policy_avg]

setorder(macro, month)
macro <- macro[month >= add_months(START_MONTH, -1L)]

# =========================== 6. SAVE =========================================
system <- merge(system, macro, by = "month", all.x = TRUE)
store_write(bank, "mart_bank_month_v6")
store_write(system, "mart_system_month_v6")
store_write(macro, "macro_monthly_v6")

summary_tab <- data.table(
  item = c("bank_month_rows", "banks", "first_month", "last_month", "system_months",
           "share_bank_months_rate_flag", "macro_columns", "last_policy_month", "last_uonia_month",
           "last_ovdp_month", "last_curve_month", "last_fx_month", "last_mir_month"),
  value = c(nrow(bank), uniqueN(bank$nkb), as.character(min(bank$month)), as.character(max(bank$month)), nrow(system),
            round(mean(bank$dq_rate_out_of_bounds, na.rm = TRUE), 4), ncol(macro),
            if ("policy_eom" %in% names(macro)) as.character(macro[is.finite(policy_eom), max(month)]) else "MISSING",
            if ("uonia_avg" %in% names(macro)) as.character(macro[is.finite(uonia_avg), max(month)]) else "MISSING",
            if ("ovdp_uah_all" %in% names(macro)) as.character(macro[is.finite(ovdp_uah_all), max(month)]) else "MISSING",
            if ("zc_uah_1y" %in% names(macro)) as.character(macro[is.finite(zc_uah_1y), max(month)]) else "MISSING",
            if ("usd_eom" %in% names(macro)) as.character(macro[is.finite(usd_eom), max(month)]) else "MISSING",
            if ("nb_dep_hh_term_uah" %in% names(macro)) as.character(macro[is.finite(nb_dep_hh_term_uah), max(month)]) else "MISSING"))
fwrite(summary_tab, file.path(LOG_DIR, "build_v6_summary.csv"))
print(summary_tab)
log_msg("BUILD v6 COMPLETE. Next: source('03_section_2_1_analysis_v2.R')")
