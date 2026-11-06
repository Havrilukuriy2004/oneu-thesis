# ============================================================================
# 10_section_2_3_repricing_gap_v6.R — ПІДРОЗДІЛ 2.3
# Revealed Repricing Gap: процентний ризик банківської книги, виявлений з поведінки ставок
# ----------------------------------------------------------------------------
# Reads marts (02) and results of 2.1–2.2 (04: D5/D6, 07: P1–P5 / P8a, 09: EX21_E2b).
# Output: output_nbu/section_2_3/ (tables/S23_*, figures/S23_*, Section_2_3.xlsx) and the bank-level
# inputs of the dynamic Excel (tables/S23_bank_items_latest.csv, S23_beta_paths.csv, S23_parameters.csv).
#
# Idea. The contractual (Basel "income gap") view treats every demand deposit as repricing at once and
# every repriced item as moving one-for-one with the policy rate. 2.2 shows neither is true: demand
# deposits barely move (β ≈ 0.05), term deposits and loans reprice partially and slowly. Replacing
# contractual repricing shares by the REVEALED cumulative pass-through β_k(h) of each balance-sheet
# item gives the revealed repricing gap and the NII / EVE sensitivity that banks actually face
# (Flannery & James 1984; English, Van den Heuvel & Zakrajšek 2018; Drechsler, Savov & Schnabl 2021;
# Hoffmann et al. 2019; Gomez et al. 2021 "income gap"; Basel IRRBB 2016 for the regulatory benchmark).
#
# Blocks
#  R1  balance sheet of each bank by repricing class (UAH items carry policy-rate sensitivity,
#      FX items do not), system structure over time
#  R2  sensitivities per bank-month:
#        contractual  S^c(h) = Σ_A A·(1 − (1−λ_A)^h) − Σ_L L·(1 − (1−λ_L)^h)          (/ total assets)
#        revealed     S^r(h) = Σ_A A·β_A(h)       − Σ_L L·β_L(h)                      (/ total assets)
#        long-run     S^θ    = Σ_A A·θ_A − Σ_L L·θ_L (θ: bank ECM, shrunk to the mean group)
#      ΔNII over 12 months for a permanent ±100 bp: Σ_{m=1..12} S(m)·TA·Δi/12 (UAH, % NII, % equity)
#      ΔEVE for ±200 bp: −Δi·(Σ D_A·A − Σ D_L·L); contractual (demand deposits D = 0) vs behavioural
#      (deposit franchise duration, 04/D5); supervisory outlier test |ΔEVE| > 15% of equity
#  R3  system aggregates over time; cross-section and distribution by ownership/size
#  R4  validation: does the measure predict the REALISED change in a bank's NIM?
#        Δ12 NIM_bt = α_b + τ_t + γ·(Gap_b,t−12 × Δ12 i_t) + ε  (bank + month FE, s.e. clustered by bank)
#      γ = 1 → unbiased; horse race contractual vs revealed (main claim of 2.3)
#  R5  scenarios: NII path 1–24 months for −300…+300 bp; asymmetric betas (hikes ≠ cuts) from 2.2
#  R6  bank-level realised NIM beta (time series per bank) vs constructed revealed gap
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_models_v6.R"), encoding = "UTF-8")
.need(c("sandwich"))
init_output("S23", folder = "section_2_3")
CAP <- "Джерело: розрахунки автора за даними НБУ (OSB, фінзвітність банків, статистика процентних ставок) і результатами 2.2."
col_or_na <- function(dt, v) if (v %in% names(dt)) dt[[v]] else rep(NA_real_, nrow(dt))
z0 <- function(x) fifelse(is.finite(x), x, 0)
blk <- function(name, expr) { t0 <- Sys.time()
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) { status(name, "error", conditionMessage(e)); message("  [", name, "] ", conditionMessage(e)); FALSE })
  if (ok) status(name, "ok", sprintf("%.1f с", as.numeric(difftime(Sys.time(), t0, units = "secs")))) }
rd <- function(folder, f) { p <- file.path(OUTPUT_DIR, folder, "tables", f); if (file.exists(p)) fread(p, encoding = "UTF-8") else data.table() }
t0_all <- Sys.time()

# ------------------------------ parameters (also exported to the Excel) --------
PAR <- list(
  ovdp_maturity_y = 2.5,      # average remaining maturity of the OVDP book (fixed coupons), years
  ovdp_duration_y = 2.0,      # modified duration of the OVDP / securities book, years
  cd_duration_y = 0.08,       # NBU CDs: overnight and 3M
  ib_duration_y = 0.05,       # interbank
  oth_liab_lambda = 1 / 12,   # IFI loans, own securities, subordinated debt: ~1 year repricing
  oth_liab_duration_y = 1.0,
  nmd_delta = 0.2,            # run-off rate of non-maturity deposits for the franchise duration (D5)
  theta_shrink_k = 24,        # shrinkage of bank ECM θ to the mean group: weight n/(n + k)
  sot_threshold = 0.15,       # supervisory outlier test: |ΔEVE| / equity
  shock_bp = 100)

# ------------------------------ inputs ---------------------------------------
B <- store_read("mart_bank_month_v6"); B[, `:=`(month = as.Date(month), nkb = as.character(nkb))]
A <- store_read("mart_bank_assets_v6"); A[, `:=`(month = as.Date(month), nkb = as.character(nkb))]
S <- store_read("mart_system_month_v6"); S[, month := as.Date(month)]
B <- merge(B, A[, intersect(c("nkb", "month", "nii_m", "avg_iea", "ann", "equity", "assets"), names(A)), with = FALSE], by = c("nkb", "month"), all.x = TRUE, suffixes = c("", "_a"))
B <- merge(B, S[, .(month, policy = 100 * policy_avg)], by = "month", all.x = TRUE)
setorder(B, nkb, month)

# betas: cumulative pass-through paths from 2.2 (pp per pp), h = 0…24; symmetric and asymmetric
LP <- rd("section_2_2", "S22_P1_P5_panel_local_projections.csv")
if (!nrow(LP)) stop("немає результатів 2.2 (S22_P1_P5) — спочатку запустіть 07")
BETA <- LP[spec == "base" & term == ".s", .(key, h, b)]
BETA_UP <- LP[spec == "asym" & term == ".s_up", .(key, h, b_up = b)]; BETA_DN <- LP[spec == "asym" & term == ".s_dn", .(key, h, b_dn = b)]
BETA <- Reduce(function(x, y) merge(x, y, by = c("key", "h"), all.x = TRUE), list(BETA, BETA_UP, BETA_DN))
# a few keys are mechanical: NBU refinancing / interbank follow the policy rate one-for-one (CD yield path as a proxy)
HMAX <- max(BETA$h)
beta_of <- function(k, hh, col = "b") { v <- BETA[BETA$key == k & BETA$h == hh][[col]]; if (length(v) && is.finite(v[1])) v[1] else NA_real_ }
MG <- rd("section_2_2", "S22_P8a_ecm_bank_level.csv"); if (nrow(MG)) MG[, nkb := as.character(nkb)]
MGS <- rd("section_2_2", "S22_P8b_ecm_mean_group.csv")
D5 <- rd("section_2_1_v2", "D5_effective_duration_nmd.csv"); D6 <- rd("section_2_1_v2", "D6_stock_flow_lambda.csv")
SF <- rd("expectations_money", "EX21_E2b_stock_flow_official_rates.csv")
lam <- function(tab, keycol, keyval, col, default) { if (!nrow(tab)) return(default); v <- tab[get(keycol) == keyval & period == "all"][[col]]; if (length(v) && is.finite(v[1]) && v[1] > 0 && v[1] <= 1) v[1] else default }
LAMBDA <- c(hh_term = lam(D6, "product", "hh_term", "l", 1 / 6), corp_term = lam(D6, "product", "corp_term", "l", 1 / 6),
            loan_corp = lam(SF, "series", "loan_nfc", "lambda", 1 / 12), loan_hh = lam(SF, "series", "loan_hh", "lambda", 1 / 12))

# ------------------------------ R1. balance sheet by repricing class ----------
# item, side, UAH stock(s), FX stock(s), β key (2.2), contractual monthly repricing λ, contractual duration (years)
ITEMS <- rbindlist(list(
  list("cd",        "A", "cd_nbu_uah+nbu_placements_uah",                 "cd_nbu_fx+nbu_placements_fx",                 "y_cd",        1,                           PAR$cd_duration_y,  "Депсертифікати та розміщення в НБУ"),
  list("ib",        "A", "interbank_assets_uah",                          "interbank_assets_fx",                         "y_cd",        1,                           PAR$ib_duration_y,  "Кошти в інших банках"),
  list("loan_corp", "A", "loans_corp_all_uah",                            "loans_corp_all_fx",                           "y_le",        LAMBDA[["loan_corp"]],       NA,                  "Кредити СГ (з овердрафтами)"),
  list("loan_hh",   "A", "loans_hh_all_uah",                              "loans_hh_all_fx",                             "y_hh",        LAMBDA[["loan_hh"]],         NA,                  "Кредити ФО"),
  list("ovdp",      "A", "ovdp_uah+refin_other_uah",                      "ovdp_fx+refin_other_fx",                      "y_refin",     1 / (12 * PAR$ovdp_maturity_y), PAR$ovdp_duration_y, "ОВДП та інші ЦП, що рефінансуються"),
  list("sec",       "A", "sec_other_all_uah+loans_gov_uah",               "sec_other_all_fx+loans_gov_fx",               "y_refin",     1 / (12 * PAR$ovdp_maturity_y), PAR$ovdp_duration_y, "Інші цінні папери, кредити держсектору"),
  list("res",       "A", "nbu_corr_reserves_uah",                         "nbu_corr_reserves_fx",                        NA,            0,                           0,                   "Кореспондентський рахунок у НБУ (резерви)"),
  list("hh_dem",    "L", "dep_hh_demand_uah",                             "dep_hh_demand_fx",                            "hh_demand",   1,                           0,                   "Кошти ФО на вимогу"),
  list("hh_term",   "L", "dep_hh_term_uah",                               "dep_hh_term_fx",                              "hh_term",     LAMBDA[["hh_term"]],         NA,                  "Строкові депозити ФО"),
  list("corp_dem",  "L", "dep_corp_demand_uah+dep_nbfi_demand_uah+dep_budget_uah", "dep_corp_demand_fx+dep_nbfi_demand_fx+dep_budget_fx", "corp_demand", 1, 0,                   "Кошти СГ, НБФУ і бюджету на вимогу"),
  list("corp_term", "L", "dep_corp_term_uah+dep_nbfi_term_uah",           "dep_corp_term_fx+dep_nbfi_term_fx",           "corp_term",   LAMBDA[["corp_term"]],       NA,                  "Строкові депозити СГ і НБФУ"),
  list("nbu",       "L", "liab_nbu_uah",                                  "liab_nbu_fx",                                 "y_cd",        1,                           PAR$cd_duration_y,   "Кредити НБУ (рефінансування)"),
  list("ibl",       "L", "liab_interbank_uah",                            "liab_interbank_fx",                           "y_cd",        1,                           PAR$ib_duration_y,   "Кошти інших банків"),
  list("oth",       "L", "liab_ifi_uah+liab_own_sec_uah+liab_subdebt_uah+repo_clients_uah+savings_certs_uah",
                         "liab_ifi_fx+liab_own_sec_fx+liab_subdebt_fx+repo_clients_fx+savings_certs_fx", "c_ibl",  PAR$oth_liab_lambda,         PAR$oth_liab_duration_y, "МФО, власні ЦП, субборг, ощадні сертифікати")))
setnames(ITEMS, c("item", "side", "uah", "fx", "key", "lambda", "dur", "label"))
ITEMS[is.na(dur), dur := fifelse(lambda > 0, 1 / (12 * lambda), 0)]           # repricing horizon as duration proxy
sumcols <- function(D, spec) { vs <- strsplit(spec, "+", fixed = TRUE)[[1]]; vs <- intersect(vs, names(D)); if (!length(vs)) return(rep(NA_real_, nrow(D))); Reduce(`+`, lapply(vs, function(v) z0(D[[v]]))) }
for (k in seq_len(nrow(ITEMS))) { it <- ITEMS[k]; B[, (paste0("U_", it$item)) := sumcols(B, it$uah)]; B[, (paste0("F_", it$item)) := sumcols(B, it$fx)] }
B[, TA := fcoalesce(col_or_na(B, "fin_assets"), col_or_na(B, "assets"))]
B[, EQ := fcoalesce(col_or_na(B, "fin_equity"), col_or_na(B, "equity"))]
B[, .nii_m := fcoalesce(col_or_na(B, "fin_nii_m"), col_or_na(B, "nii_m"))]
B[, NII12 := frollsum(.nii_m, 12, na.rm = TRUE), by = nkb][, .nii_m := NULL]
B[, mapped_A := Reduce(`+`, lapply(ITEMS[side == "A", item], function(i) get(paste0("U_", i)) + get(paste0("F_", i))))]
B[, mapped_L := Reduce(`+`, lapply(ITEMS[side == "L", item], function(i) get(paste0("U_", i)) + get(paste0("F_", i))))]
B <- B[is.finite(TA) & TA > 0]
status("R1 баланс", "ok", sprintf("%d банків, %d банко-місяців; покриття активів статтями: медіана %.0f%%", uniqueN(B$nkb), nrow(B), 100 * median(B$mapped_A / B$TA, na.rm = TRUE)))

blk("R1 структура", {
  latest <- B[, max(month)]
  st <- rbindlist(lapply(seq_len(nrow(ITEMS)), function(k) { it <- ITEMS[k]
    B[, .(item = it$item, label = it$label, side = it$side, uah = sum(get(paste0("U_", it$item)), na.rm = TRUE), fx = sum(get(paste0("F_", it$item)), na.rm = TRUE)), by = month] }))
  st <- merge(st, B[, .(TA = sum(TA, na.rm = TRUE)), by = month], by = "month")
  st[, `:=`(share_uah = uah / TA, share_fx = fx / TA)]
  save_tab(st, "S23_R1_structure_monthly.csv", "R1_structure")
  sl <- st[month == latest][, .(`Стаття` = label, `Бік` = fifelse(side == "A", "Активи", "Зобов'язання"), `Гривня, млрд грн` = round(uah / 1e3, 1),
                                `Валюта, млрд грн` = round(fx / 1e3, 1), `Частка в активах, %` = round(100 * (uah + fx) / TA, 1))]
  save_tab(sl, "S23_R1_structure_latest.csv", "R1_latest")
})

# ------------------------------ R2. sensitivities ----------------------------
bpath <- function(k, col = "b") if (is.na(k)) rep(0, HMAX + 1) else vapply(0:HMAX, function(h) { v <- beta_of(k, h, col); if (is.finite(v)) v else 0 }, 0)
BP <- lapply(setNames(ITEMS$key, ITEMS$item), bpath); BPU <- lapply(setNames(ITEMS$key, ITEMS$item), bpath, col = "b_up"); BPD <- lapply(setNames(ITEMS$key, ITEMS$item), bpath, col = "b_dn")
CP <- lapply(setNames(ITEMS$lambda, ITEMS$item), function(l) 1 - (1 - l)^(0:HMAX + 1))     # share repriced by month h+1
sgn <- setNames(fifelse(ITEMS$side == "A", 1, -1), ITEMS$item)
sens_path <- function(D, P) {                                                        # (n × HMAX+1) matrix of S(h) in UAH per 1 pp
  Reduce(`+`, lapply(ITEMS$item, function(i) outer(sgn[[i]] * D[[paste0("U_", i)]], P[[i]])))
}
Sr <- sens_path(B, BP); Sc <- sens_path(B, CP); Su <- sens_path(B, BPU); Sd <- sens_path(B, BPD)
h12 <- min(12, HMAX) + 1
B[, `:=`(RG12 = Sr[, h12] / TA, CG12 = Sc[, h12] / TA, RG3 = Sr[, min(3, HMAX) + 1] / TA, CG3 = Sc[, min(3, HMAX) + 1] / TA,
         dNII_r = rowSums(Sr[, 2:h12, drop = FALSE]) / 12 * PAR$shock_bp / 1e4,           # UAH mn per +100 bp, first year (S is per 1 pp = 0.01)
         dNII_c = rowSums(Sc[, 2:h12, drop = FALSE]) / 12 * PAR$shock_bp / 1e4,
         dNII_up = rowSums(Su[, 2:h12, drop = FALSE]) / 12 / 100, dNII_dn = -rowSums(Sd[, 2:h12, drop = FALSE]) / 12 / 100)]   # +100 bp hike / −100 bp cut
B[, `:=`(dNII_r_pct = 100 * dNII_r / NII12, dNII_c_pct = 100 * dNII_c / NII12, dNII_r_eq = 100 * dNII_r / EQ, dNII_c_eq = 100 * dNII_c / EQ)]
for (v in c("dNII_r_pct", "dNII_c_pct", "dNII_r_eq", "dNII_c_eq")) B[!is.finite(get(v)) | abs(get(v)) > 1e3, (v) := NA_real_]

# long-run θ (bank ECM, shrunk to the mean group); keys without bank θ use the 24-month β
theta_mg <- if (nrow(MGS)) setNames(MGS$theta_mg, MGS$key) else c()
th_bank <- function(nkb_vec, k) {
  mg <- if (!is.na(k) && k %in% names(theta_mg)) theta_mg[[k]] else if (!is.na(k)) beta_of(k, HMAX) else 0
  if (is.na(k) || !nrow(MG) || !k %in% MG$key) return(rep(mg, length(nkb_vec)))
  z <- MG[key == k, .(nkb, theta, n)]; w <- z$n / (z$n + PAR$theta_shrink_k); z[, th := w * theta + (1 - w) * mg]
  out <- z$th[match(nkb_vec, z$nkb)]; fifelse(is.finite(out), out, mg) }
B[, RGLR := Reduce(`+`, lapply(ITEMS$item, function(i) sgn[[i]] * get(paste0("U_", i)) * th_bank(nkb, ITEMS[item == i, key]))) / TA]

# EVE: durations; behavioural duration of demand deposits = deposit-franchise duration at the current rate (D5)
dur_nmd <- function(prod, i0) { z <- D5[product == prod & abs(delta - PAR$nmd_delta) < 1e-9]; if (!nrow(z)) return(rep(0, length(i0)))
  approx(z$i0, z$duration, xout = pmin(pmax(i0, min(z$i0)), max(z$i0)), rule = 2)$y }
B[, `:=`(D_hhdem_b = dur_nmd("hh_demand", policy / 100), D_corpdem_b = dur_nmd("corp_demand", policy / 100))]
dur_c <- setNames(ITEMS$dur, ITEMS$item)
eve <- function(D, behavioural) { tot <- 0
  for (i in ITEMS$item) { d <- dur_c[[i]]
    if (behavioural && i == "hh_dem") d <- D$D_hhdem_b
    if (behavioural && i == "corp_dem") d <- D$D_corpdem_b
    tot <- tot + sgn[[i]] * (D[[paste0("U_", i)]] + D[[paste0("F_", i)]]) * d }   # FX items too: EVE is measured in UAH
  -0.02 * tot }                                                                    # +200 bp
B[, `:=`(dEVE_c = eve(B, FALSE), dEVE_b = eve(B, TRUE))]
B[, `:=`(dEVE_c_eq = 100 * dEVE_c / EQ, dEVE_b_eq = 100 * dEVE_b / EQ)]
B[, `:=`(SOT_c = -dEVE_c_eq > 100 * PAR$sot_threshold, SOT_b = -dEVE_b_eq > 100 * PAR$sot_threshold)]
for (v in c("dEVE_c_eq", "dEVE_b_eq")) B[!is.finite(get(v)) | abs(get(v)) > 1e3, (v) := NA_real_]
status("R2 чутливості", "ok", sprintf("β-шляхи з 2.2 до h = %d; λ: депозити ФО %.3f, СГ %.3f, кредити СГ %.3f, ФО %.3f", HMAX, LAMBDA[["hh_term"]], LAMBDA[["corp_term"]], LAMBDA[["loan_corp"]], LAMBDA[["loan_hh"]]))

# ------------------------------ R3. system, cross-section ---------------------
latest <- B[, max(month)]
blk("R3 система і банки", {
  SYS <- B[, .(banks = .N, TA = sum(TA), RG12 = sum(RG12 * TA) / sum(TA), CG12 = sum(CG12 * TA) / sum(TA), RGLR = sum(RGLR * TA, na.rm = TRUE) / sum(TA),
               dNII_r = sum(dNII_r), dNII_c = sum(dNII_c), NII12 = sum(NII12, na.rm = TRUE), EQ = sum(EQ, na.rm = TRUE),
               dEVE_c = sum(dEVE_c), dEVE_b = sum(dEVE_b), share_asset_sensitive = mean(RG12 > 0)), by = month][order(month)]
  SYS[, `:=`(dNII_r_pct = 100 * dNII_r / NII12, dNII_c_pct = 100 * dNII_c / NII12, dEVE_c_eq = 100 * dEVE_c / EQ, dEVE_b_eq = 100 * dEVE_b / EQ)]
  save_tab(SYS, "S23_R3_system_monthly.csv", "R3_system")
  L <- B[month == latest, .(nkb, bank = fullname, owner, TA_bn = TA / 1e3, EQ_bn = EQ / 1e3, CG12_pp = 100 * CG12, RG12_pp = 100 * RG12, RGLR_pp = 100 * RGLR,
                           dNII_c_mn = dNII_c, dNII_r_mn = dNII_r, dNII_up_mn = dNII_up, dNII_dn_mn = dNII_dn, dNII_c_pct, dNII_r_pct, dNII_r_eq,
                           dEVE_c_eq, dEVE_b_eq, SOT_c, SOT_b, demand_share = 100 * col_or_na(B[month == latest], "demand_share"))][order(-TA_bn)]
  save_tab(L, "S23_R3_bank_gaps_latest.csv", "R3_banks_latest")
  OWN <- B[month == latest & !is.na(owner), .(banks = .N, TA_bn = sum(TA) / 1e3, CG12_pp = 100 * sum(CG12 * TA) / sum(TA), RG12_pp = 100 * sum(RG12 * TA) / sum(TA),
             dNII_r_pct = 100 * sum(dNII_r) / sum(NII12, na.rm = TRUE), dEVE_b_eq = 100 * sum(dEVE_b) / sum(EQ, na.rm = TRUE),
             share_asset_sensitive = 100 * mean(RG12 > 0), share_SOT_b = 100 * mean(SOT_b, na.rm = TRUE)), by = owner]
  save_tab(OWN, "S23_R3_by_ownership_latest.csv", "R3_ownership")
  fact("R3_system_latest", sprintf("%s: договірний розрив 12 міс. %.2f%% активів, виявлений %.2f%%; ΔЧПД за рік при +100 б.п.: договірний %+.1f%%, виявлений %+.1f%% ЧПД; ΔEVE(+200) договірний %+.1f%%, поведінковий %+.1f%% капіталу",
       format(latest, "%m.%Y"), 100 * SYS[month == latest, CG12], 100 * SYS[month == latest, RG12], SYS[month == latest, dNII_c_pct], SYS[month == latest, dNII_r_pct],
       SYS[month == latest, dEVE_c_eq], SYS[month == latest, dEVE_b_eq]))
  fact("R3_asset_sensitive", sprintf("частка банків з додатним виявленим розривом (маржа зростає при підвищенні): %.0f%%; договірним: %.0f%%",
       100 * mean(B[month == latest, RG12] > 0), 100 * mean(B[month == latest, CG12] > 0)))
  # figures
  sl <- melt(SYS[, .(month, `Договірний розрив (12 міс.)` = 100 * CG12, `Виявлений розрив (12 міс., β з 2.2)` = 100 * RG12, `Довгостроковий (θ)` = 100 * RGLR)], id.vars = "month")
  p <- ggplot(sl, aes(month, value, colour = variable, linetype = variable)) + geom_hline(yintercept = 0, linewidth = .3) + geom_line(linewidth = .7) +
    labs(title = "Розрив переоцінки банківської системи: договірний vs виявлений", subtitle = "Зміна чистої процентної маржі, % активів, на 1 в.п. облікової ставки",
         x = NULL, y = "% активів на 1 в.п.", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "S23_R3_system_gap.png", 8.5, 4.6)
  p <- ggplot(B[month == latest], aes(100 * CG12, 100 * RG12, size = TA / 1e3)) + geom_hline(yintercept = 0, linewidth = .3) + geom_vline(xintercept = 0, linewidth = .3) +
    geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = .3) + geom_point(shape = 21, fill = "grey70", alpha = .8) +
    scale_size_area(max_size = 9, name = "Активи, млрд грн") +
    labs(title = "Договірний і виявлений розрив переоцінки банків", subtitle = sprintf("%s; пунктир — збіг оцінок", format(latest, "%m.%Y")),
         x = "Договірний розрив, % активів на 1 в.п.", y = "Виявлений розрив, % активів на 1 в.п.", caption = CAP) + theme_thesis()
  save_fig(p, "S23_R3_contract_vs_revealed.png", 8.5, 5.5)
  p <- ggplot(B[month == latest & is.finite(dEVE_b_eq)], aes(dEVE_c_eq, dEVE_b_eq)) + geom_hline(yintercept = c(-15, 15), linetype = "22", linewidth = .3) +
    geom_vline(xintercept = c(-15, 15), linetype = "22", linewidth = .3) + geom_point(shape = 21, fill = "grey60") +
    labs(title = "Зміна економічної вартості капіталу при +200 б.п.", subtitle = "Пунктир — поріг тесту на викид (±15% капіталу, Базель IRRBB)",
         x = "Договірні строки (кошти на вимогу: D = 0), % капіталу", y = "Поведінкові строки (депозитна франшиза), % капіталу", caption = CAP) + theme_thesis()
  save_fig(p, "S23_R3_eve_outlier_test.png", 8.5, 5.2)
})

# ------------------------------ R4. validation on realised NIM ----------------
fe2_cl <- function(d, y, x, tol = 1e-9, maxit = 500) {
  d <- d[complete.cases(d[, c(y, x), with = FALSE])]; if (nrow(d) < 100) return(NULL)
  Z <- as.matrix(d[, c(y, x), with = FALSE]); g1 <- d$nkb; g2 <- as.character(d$month)
  n1 <- table(g1); n2 <- table(g2)
  for (it in seq_len(maxit)) { Z0 <- Z
    Z <- Z - (rowsum(Z, g1) / as.vector(n1[sort(unique(g1))]))[g1, , drop = FALSE]
    Z <- Z - (rowsum(Z, g2) / as.vector(n2[sort(unique(g2))]))[g2, , drop = FALSE]
    if (max(abs(Z - Z0)) < tol) break }
  yv <- Z[, 1]; X <- Z[, -1, drop = FALSE]; XtX <- crossprod(X); Bi <- solve(XtX); b <- Bi %*% crossprod(X, yv); u <- as.vector(yv - X %*% b)
  hb <- rowsum(X * u, g1); G <- nrow(hb); Sm <- crossprod(hb) * G / (G - 1) * (nrow(X) - 1) / (nrow(X) - ncol(X))
  V <- Bi %*% Sm %*% Bi; data.table(term = colnames(X), b = as.vector(b), se = sqrt(diag(V)), n = nrow(X), banks = G) }
blk("R4 валідація", {
  B[, nim := fifelse(is.finite(nii_m) & is.finite(avg_iea) & avg_iea > 1, 100 * fcoalesce(ann, ann_factor(month)) * nii_m / avg_iea, NA_real_)]
  B[!is.finite(nim) | nim < -10 | nim > 40, nim := NA_real_]
  B[, `:=`(d12nim = nim - calendar_shift(nim, month, 12), d12i = policy - calendar_shift(policy, month, 12), RG12_l = calendar_shift(RG12, month, 12), CG12_l = calendar_shift(CG12, month, 12), RGLR_l = calendar_shift(RGLR, month, 12)), by = nkb]
  q <- B[, quantile(d12nim, c(.01, .99), na.rm = TRUE)]; B[d12nim < q[1] | d12nim > q[2], d12nim := NA_real_]
  B[, `:=`(x_rev = RG12_l * d12i * calendar_shift(TA / avg_iea, month, 12), x_con = CG12_l * d12i * calendar_shift(TA / avg_iea, month, 12), x_lr = RGLR_l * d12i * calendar_shift(TA / avg_iea, month, 12))]    # predicted Δ NIM, pp (gap share × Δi in pp)
  R <- rbindlist(list(fe2_cl(B, "d12nim", "x_rev")[, model := "(1) виявлений"], fe2_cl(B, "d12nim", "x_con")[, model := "(2) договірний"],
                      fe2_cl(B, "d12nim", c("x_rev", "x_con"))[, model := "(3) обидва"], fe2_cl(B, "d12nim", "x_lr")[, model := "(4) довгостроковий θ"]), fill = TRUE)
  R[, p := 2 * pnorm(-abs(b / se))]
  R[, term_lab := c(x_rev = "Виявлений розрив × Δ12 i", x_con = "Договірний розрив × Δ12 i", x_lr = "Довгостроковий розрив × Δ12 i")[term]]
  save_tab(R, "S23_R4_validation_nim.csv", "R4_validation")
  a <- R[model == "(3) обидва"]
  fact("R4_horse_race", sprintf("Δ12 ЧПМ на прогноз: виявлений γ = %.2f (с.п. %.2f), договірний γ = %.2f (с.п. %.2f); %d банків",
       a[term == "x_rev", b], a[term == "x_rev", se], a[term == "x_con", b], a[term == "x_con", se], a$banks[1]))
})

blk("R6 реалізована бета ЧПМ банку", {
  bb <- B[is.finite(d12nim) & is.finite(d12i), if (.N >= 36 && sd(d12i) > 0) { f <- lm(d12nim ~ d12i); V <- tryCatch(sandwich::NeweyWest(f, lag = 12, prewhite = FALSE), error = function(e) vcov(f))
         .(nim_beta = coef(f)[2], nim_beta_se = sqrt(V[2, 2]), n = .N, RG12_mean = 100 * mean(RG12, na.rm = TRUE), CG12_mean = 100 * mean(CG12, na.rm = TRUE)) }, by = .(nkb, bank = fullname)]
  save_tab(bb, "S23_R6_bank_realised_nim_beta.csv", "R6_nim_beta")
  if (nrow(bb) >= 15) {
    cr <- bb[, .(corr_revealed = cor(nim_beta, RG12_mean), corr_contract = cor(nim_beta, CG12_mean), banks = .N)]
    fact("R6_cross_bank", sprintf("кореляція реалізованої бети ЧПМ банку з виявленим розривом %.2f, з договірним %.2f (%d банків)", cr$corr_revealed, cr$corr_contract, cr$banks))
    p <- ggplot(bb, aes(RG12_mean, nim_beta)) + geom_hline(yintercept = 0, linewidth = .3) + geom_vline(xintercept = 0, linewidth = .3) +
      geom_smooth(method = "lm", se = TRUE, colour = "black", fill = "grey85", linewidth = .5, formula = y ~ x) + geom_point(shape = 21, fill = "grey60") +
      labs(title = "Виявлений розрив і реалізована чутливість маржі банків", subtitle = "Кожна точка — банк: β з регресії Δ12 ЧПМ на Δ12 облікової (≥ 36 міс.)",
           x = "Середній виявлений розрив, % активів на 1 в.п.", y = "Реалізована бета ЧПМ, в.п. на 1 в.п.", caption = CAP) + theme_thesis()
    save_fig(p, "S23_R6_realised_vs_revealed.png", 8.5, 5.2)
  }
})

# ------------------------------ R5. scenarios -----------------------------------
blk("R5 сценарії", {
  Ls <- B[month == latest]
  shocks <- c(-300, -200, -100, 100, 200, 300)
  sc <- rbindlist(lapply(shocks, function(s) {
    P <- if (s > 0) BPU else BPD; Sx <- sens_path(Ls, P); Sl <- sens_path(Ls, BP); Sct <- sens_path(Ls, CP)
    rbindlist(lapply(1:min(24, HMAX), function(m) data.table(shock_bp = s, month_h = m,
      nii_sym = sum(rowSums(Sl[, 2:(m + 1), drop = FALSE])) / 12 * s / 1e4,
      nii_asym = sum(rowSums(Sx[, 2:(m + 1), drop = FALSE])) / 12 * abs(s) / 1e4 * (if (s > 0) 1 else -1),
      nii_contract = sum(rowSums(Sct[, 2:(m + 1), drop = FALSE])) / 12 * s / 1e4))) }))
  nii_base <- sum(Ls$NII12, na.rm = TRUE)
  sc[, `:=`(nii_sym_pct = 100 * nii_sym / nii_base, nii_asym_pct = 100 * nii_asym / nii_base, nii_contract_pct = 100 * nii_contract / nii_base)]
  save_tab(sc, "S23_R5_scenarios_system.csv", "R5_scenarios")
  y1 <- sc[month_h == 12, .(`Шок, б.п.` = shock_bp, `Договірний, % ЧПД` = round(nii_contract_pct, 2), `Виявлений (симетричні β), % ЧПД` = round(nii_sym_pct, 2),
                            `Виявлений (асиметричні β), % ЧПД` = round(nii_asym_pct, 2))]
  save_tab(y1, "S23_R5_scenarios_12m.csv", "R5_12m")
  p <- ggplot(melt(sc[, .(shock_bp, month_h, `Договірний` = nii_contract_pct, `Виявлений (асиметричні β)` = nii_asym_pct)], id.vars = c("shock_bp", "month_h")),
              aes(month_h, value, colour = factor(shock_bp), linetype = variable)) + geom_hline(yintercept = 0, linewidth = .3) + geom_line(linewidth = .6) +
    labs(title = "Накопичена зміна ЧПД банківської системи за сценаріями облікової ставки", subtitle = "% річного ЧПД; баланс постійний",
         x = "Місяців після зміни", y = "% ЧПД", colour = "Шок, б.п.", linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "S23_R5_scenarios.png", 8.5, 5)
})

# ------------------------------ exports for the dynamic Excel ----------------
blk("Excel-входи", {
  Ls <- B[month == latest]
  items_long <- rbindlist(lapply(seq_len(nrow(ITEMS)), function(k) { it <- ITEMS[k]
    Ls[, .(nkb, bank = fullname, owner, item = it$item, side = it$side, label = it$label, uah = get(paste0("U_", it$item)), fx = get(paste0("F_", it$item)))] }))
  save_tab(items_long, "S23_bank_items_latest.csv", "X_items")
  save_tab(Ls[, .(nkb, bank = fullname, owner, month, TA, EQ, NII12, policy, D_hhdem_b, D_corpdem_b, cod4_uah = 100 * col_or_na(Ls, "cod4_uah"),
                  demand_share = col_or_na(Ls, "demand_share"), fx_share = col_or_na(Ls, "fx_share"))], "S23_bank_totals_latest.csv", "X_totals")
  bp <- rbindlist(lapply(unique(na.omit(ITEMS$key)), function(k) data.table(beta_key = k, h = 0:HMAX, b = bpath(k), b_up = bpath(k, "b_up"), b_dn = bpath(k, "b_dn"))))
  save_tab(bp, "S23_beta_paths.csv", "X_betas")
  th <- rbindlist(lapply(unique(na.omit(ITEMS$key)), function(k) data.table(nkb = Ls$nkb, beta_key = k, theta = th_bank(Ls$nkb, k))))
  save_tab(th, "S23_bank_theta.csv", "X_theta")
  save_tab(ITEMS[, .(item, side, label, key, lambda, dur)], "S23_items_parameters.csv", "X_items_par")
  save_tab(data.table(param = names(PAR), value = unlist(PAR)), "S23_parameters.csv", "X_par")
  hist <- B[month >= add_months(latest, -59L), .(nkb, month, RG12 = 100 * RG12, CG12 = 100 * CG12, nim, cod4 = 100 * col_or_na(B[month >= add_months(latest, -59L)], "cod4_uah"), policy)]
  save_tab(hist, "S23_bank_history_60m.csv", "X_history")
  # ---- inputs of the IRRBB stress-test workbook (13_irrbb_stress_test.py) ----
  # (a) coupon / rate of each item by currency: stock-weighted across banks at the latest month (fallbacks: curve points)
  zc_last <- S[month == latest]
  cv <- function(v, dflt) { x <- col_or_na(zc_last, v); if (length(x) && is.finite(x[1])) x[1] else dflt }
  pol <- cv("policy_avg", 0.15); cd_on <- cv("cd_on_rate_avg", pol); fx3m <- cv("zc_fx_3m", 0.03); fx2y <- cv("zc_fx_2y", 0.035)
  wmean <- function(rate_col, stock_col, dflt) { r <- col_or_na(Ls, rate_col); w <- col_or_na(Ls, stock_col)
    ok <- is.finite(r) & is.finite(w) & w > 0 & r > 0 & r < 0.6; if (sum(ok) < 3) return(dflt); sum(r[ok] * w[ok]) / sum(w[ok]) }
  IR <- data.table(item = ITEMS$item, uah = NA_real_, fx = NA_real_)
  setr <- function(i, u, f) IR[item == i, `:=`(uah = u, fx = f)]
  setr("cd", wmean("y_cd_uah", "U_cd", cd_on), 0)
  setr("ib", pol, fx3m)
  setr("loan_corp", wmean("y_loans_corp_uah", "U_loan_corp", pol + 0.04), wmean("y_loans_corp_fx", "F_loan_corp", 0.07))
  setr("loan_hh", wmean("y_loans_hh_uah", "U_loan_hh", pol + 0.04), wmean("y_loans_hh_fx", "F_loan_hh", 0.07))
  setr("ovdp", wmean("y_refin_uah", "U_ovdp", pol), fx2y)
  setr("sec", wmean("y_sec_other_uah", "U_sec", pol), fx2y)
  setr("res", 0, 0)
  setr("hh_dem", wmean("r_hh_demand_uah", "U_hh_dem", 0.01), wmean("r_hh_demand_fx", "F_hh_dem", 0))
  setr("hh_term", wmean("r_hh_term_uah", "U_hh_term", pol - 0.01), wmean("r_hh_term_fx", "F_hh_term", 0.01))
  setr("corp_dem", wmean("r_corp_demand_uah", "U_corp_dem", 0.03), wmean("r_corp_demand_fx", "F_corp_dem", 0))
  setr("corp_term", wmean("r_corp_term_uah", "U_corp_term", pol - 0.02), wmean("r_corp_term_fx", "F_corp_term", 0.01))
  setr("nbu", pol + 0.01, 0); setr("ibl", pol, fx3m); setr("oth", pol - 0.02, 0.05)
  save_tab(IR, "S23_item_rates_latest.csv", "X_item_rates")
  # (b) NBU zero-coupon curves (monthly averages), UAH and FX, for historical scenarios
  ten <- c("3m", "6m", "1y", "2y", "3y", "5y", "7y", "10y")
  cur <- rbindlist(lapply(c("uah", "fx"), function(cc) { vv <- paste0("zc_", cc, "_", ten, "_avg"); vv2 <- paste0("zc_", cc, "_", ten)
    m <- S[, c(list(month = month), lapply(seq_along(ten), function(j) fcoalesce(col_or_na(S, vv[j]), col_or_na(S, vv2[j]))))]
    setnames(m, c("month", ten)); m[, ccy := cc]; m }))
  cur <- cur[rowSums(!is.na(cur[, ..ten])) >= 4]
  save_tab(cur, "S23_curves_history.csv", "X_curves")
})

save_tab(STATUS, "S23_00_blocks_status.csv", "Status"); save_tab(FACTS, "S23_00_key_facts.csv", "Key_facts")
write_workbook("Section_2_3.xlsx")
print(STATUS); print(FACTS)
log_msg("SECTION 2.3 COMPLETE: ", length(TABLES), " tables, ", sprintf("%.1f хв", as.numeric(difftime(Sys.time(), t0_all, units = "mins"))))
