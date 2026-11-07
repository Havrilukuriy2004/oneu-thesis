# ============================================================================
# 07_section_2_2_transmission.R — ПІДРОЗДІЛ 2.2
# Передача облікової ставки в ціни й обсяги банківського балансу:
# панель банків, асиметрія, залежність від стану, режими Маркова, швидкість
# переоцінки.
# ----------------------------------------------------------------------------
# Reads only the marts (run after 02, 03, 05). Output: output_nbu/section_2_2/
#   tables/S22_*.csv, figures/S22_*.png|pdf|emf|svg, Section_2_2.xlsx
#
# Blocks
#  P0  Bank × month panel: outcomes (rates in п.п., volumes in log), shocks,
#      bank characteristics (lagged, standardised), macro controls.
#  P1  Panel local projections (Jordà 2005), bank FE, Driscoll–Kraay s.e.:
#      cumulative pass-through β_h, h = 0…24, for funding costs, asset yields,
#      NIM components; deposit-weighted and unweighted.
#  P2  Sign asymmetry (Δi⁺ vs Δi⁻) with Wald tests at every horizon.
#  P3  State dependence: high vs low policy-rate level; wartime vs pre-war.
#  P4  Markov-switching models (Hamilton 1989; own estimator, utils_markov_v6.R):
#      A) MS-ADL in differences  Δr = c_S + β0_S Δi + β1_S Δi₋₁ + ρ Δr₋₁ + ε_S
#      B) MS partial adjustment  r = a_S + b_S i + ρ_S r₋₁ + ε_S  (long-run b/(1−ρ))
#      for book cost of deposits, UIRD, MIR new deposits/loans, asset yield;
#      K = 2 vs 3 (BIC), linear vs MS (LR, AIC/BIC), RCM, expected durations,
#      regimes vs policy phases.
#  P5  Panel LP conditional on the MS regime (filtered probability, t−1).
#  P6  Bank heterogeneity: Δi × {size, demand share (deposit franchise), liquid
#      assets (CD + OVDP), capital, loans/deposits, FX share, market share,
#      state, foreign} — single and joint ("horse race") interactions.
#  P7  Quantities: deposit channel (Drechsler–Savov–Schnabl) and bank lending /
#      liquidity channel (Kashyap–Stein): Δlog deposits, loans, CD, OVDP;
#      heterogeneity by demand share, liquidity and capital.
#  P8  Error-correction pass-through, bank by bank (mean group, Pesaran–Smith):
#      long-run θ, adjustment speed φ, half-life; asymmetric adjustment
#      ("rockets and feathers"); cross-bank determinants of θ and φ.
#  P9  Summary of repricing speed (β at 1/3/6/12/24 m, half-lives) and the
#      bank-level asset–liability long-run gap passed to 2.3.
#  P10 Robustness: alternative shocks (CD O/N, UONIA, OVDP 3M), excluding
#      2022-02…2022-07, CD-regime sample (≥ 2023-04), balanced panel,
#      weighting, lags, no bank controls, no lagged dependent variable (Nickell).
#  P11 Daily event study: posted deposit rates (UIRD, daily) around each NBU
#      rate change 2018–2026 — speed of repricing in business days; hikes vs cuts;
#      USD UIRD as placebo.
#  P12 Quasi-experiments: NBU 3-month CD rules (2023-04 introduction, 2023-09,
#      2024-04, 2025-04, 2026-08 tenders). Event study / continuous DiD with bank
#      + month FE: banks more reliant on CDs before the reform vs others —
#      household term-deposit rates and volumes.
# Identification notes
#  * aggregate effects (P1–P5, P7): bank FE only (the shock is common to all banks);
#    Δi is lagged-macro-controlled; Driscoll–Kraay s.e. with lag ≥ h + 1;
#  * cross-bank differences (P6, P12): bank AND month FE, characteristics
#    standardised cross-sectionally each month, so interactions are identified
#    only from differences between banks in the same month (no aggregate-cycle
#    contamination);
#  * Nickell bias of FE with lagged Δy is O(1/T), T ≈ 100 months; checked in P10.
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_models_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_markov_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_local_inputs_v6.R"), encoding = "UTF-8")
init_output("S22", folder = "section_2_2")
CAP <- "Джерело: розрахунки автора за даними НБУ (OSB, фінзвітність банків, статистика процентних ставок, UIRD, опитування очікувань)."
H_MAX <- as.integer(Sys.getenv("S22_HMAX", "24")); N_LAG <- 3L          # S22_HMAX / S22_MS_STARTS: quick test runs only
MS_STARTS <- as.integer(Sys.getenv("S22_MS_STARTS", "12"))
col_or_na <- function(dt, v) if (v %in% names(dt)) dt[[v]] else rep(NA_real_, nrow(dt))
has <- function(dt, v) v %in% names(dt) && sum(is.finite(dt[[v]])) > 0
t0_all <- Sys.time()

# ============================== P0. PANEL ====================================
sys <- store_read("mart_system_month_v6"); sys[, month := as.Date(month)]; setorder(sys, month)
bank <- store_read("mart_bank_month_v6"); bank[, `:=`(month = as.Date(month), nkb = as.character(nkb))]
ast <- store_read("mart_bank_assets_v6"); if (nrow(ast)) ast[, `:=`(month = as.Date(month), nkb = as.character(nkb))]
sast <- store_read("mart_system_assets_v6"); if (nrow(sast)) sast[, month := as.Date(month)]

rng <- bank[, .(m0 = min(month), m1 = max(month)), by = nkb]
B <- merge(rng[, .(month = seq(m0, m1, by = "month")), by = nkb], bank, by = c("nkb", "month"), all.x = TRUE)
if (nrow(ast)) {
  ac <- intersect(c("ii_m", "ii_le_m", "ii_hh_m", "ie_m", "nii_m", "alloc_prov_m", "avg_iea", "avg_ibl", "avg_loans_le", "avg_loans_hh",
                    "loans_le", "loans_hh", "loans_all", "assets", "equity"), names(ast))
  B <- merge(B, ast[, c("nkb", "month", ac), with = FALSE], by = c("nkb", "month"), all.x = TRUE)
}
setorder(B, nkb, month)
B[, annf := fcoalesce(col_or_na(B, "ann"), ann_factor(month))]
FINREP_BREAK <- as.Date("2021-01-01")
sr <- function(flow, stock, a) fifelse(is.finite(flow) & is.finite(stock) & stock > 1, 100 * a * flow / stock, NA_real_)
B[, `:=`(y_le = sr(col_or_na(B, "ii_le_m"), col_or_na(B, "avg_loans_le"), annf), y_hh = sr(col_or_na(B, "ii_hh_m"), col_or_na(B, "avg_loans_hh"), annf),
         y_iea = sr(col_or_na(B, "ii_m"), col_or_na(B, "avg_iea"), annf), c_ibl = sr(col_or_na(B, "ie_m"), col_or_na(B, "avg_ibl"), annf),
         nim = sr(col_or_na(B, "nii_m"), col_or_na(B, "avg_iea"), annf))]
B[month < FINREP_BREAK, y_le := NA_real_]

# outcome catalogue: prefer UAH series when populated, else all currencies
pick <- function(base) { u <- paste0(base, "_uah"); t <- paste0(base, "_total")
  if (u %in% names(B) && mean(is.finite(B[[u]])) > .3) u else if (t %in% names(B) && mean(is.finite(B[[t]])) > .1) t else NA_character_ }
RATES <- rbindlist(list(
  list("cod4",        pick("cod4"),          "Вартість депозитів (книжкова)",       "Пасиви", 100),
  list("hh_demand",   pick("r_hh_demand"),   "ФО: кошти на вимогу",                 "Пасиви", 100),
  list("hh_term",     pick("r_hh_term"),     "ФО: строкові депозити",               "Пасиви", 100),
  list("corp_demand", pick("r_corp_demand"), "СГ: кошти на вимогу",                 "Пасиви", 100),
  list("corp_term",   pick("r_corp_term"),   "СГ: строкові депозити",               "Пасиви", 100),
  list("c_ibl",       "c_ibl",               "Вартість процентних зобов'язань",     "Пасиви", 1),
  list("y_cd",        pick("y_cd"),          "Дохідність депсертифікатів НБУ",      "Активи", 100),
  list("y_refin",     pick("y_refin"),       "Дохідність ОВДП (рефінансовані ЦП)",  "Активи", 100),
  list("y_le",        "y_le",                "Дохідність кредитів СГ",              "Активи", 1),
  list("y_hh",        "y_hh",                "Дохідність кредитів ФО",              "Активи", 1),
  list("y_iea",       "y_iea",               "Дохідність процентних активів",       "Активи", 1),
  list("nim",         "nim",                 "Чиста процентна маржа",               "Маржа",  1)))
setnames(RATES, c("key", "var", "label", "side", "scale"))
RATES <- RATES[!is.na(var) & var %in% names(B)]
for (k in seq_len(nrow(RATES))) {
  v <- RATES$var[k]; nm <- paste0("o_", RATES$key[k])
  B[, (nm) := RATES$scale[k] * get(v)]
  B[!is.finite(get(nm)) | get(nm) < -1 | get(nm) > 60, (nm) := NA_real_]
  B[, (nm) := { x <- get(nm); q <- quantile(x, c(.01, .99), na.rm = TRUE, names = FALSE); if (all(is.finite(q))) pmin(pmax(x, q[1]), q[2]) else x }, by = month]
}
RATES <- RATES[vapply(paste0("o_", key), function(v) sum(is.finite(B[[v]])) >= 400, logical(1))]
QUANT <- rbindlist(list(
  list("q_dep_term",   "dep_term4_total",   "Строкові депозити (ФО + СГ)"),
  list("q_dep_demand", "dep_demand4_total", "Кошти на вимогу (ФО + СГ)"),
  list("q_dep_hh",     "dep_hh_total",      "Депозити ФО"),
  list("q_dep_hh_term", if ("dep_hh_term_uah" %in% names(B) && mean(B$dep_hh_term_uah > 0, na.rm = TRUE) > .3) "dep_hh_term_uah" else "dep_hh_term_total", "Строкові депозити ФО"),
  list("q_dep_corp",   "dep_corp_total",    "Депозити СГ"),
  list("q_loans_le",   "loans_le",          "Кредити СГ (валові)"),
  list("q_loans_hh",   "loans_hh",          "Кредити ФО (валові)"),
  list("q_cd",         "cd_nbu_total",      "Депсертифікати НБУ"),
  list("q_ovdp",       "ovdp_total",        "ОВДП")))
setnames(QUANT, c("key", "var", "label")); QUANT <- QUANT[var %in% names(B)]
for (k in seq_len(nrow(QUANT))) { nm <- paste0("o_", QUANT$key[k]); B[, (nm) := fifelse(get(QUANT$var[k]) > 0.5, 100 * log(get(QUANT$var[k])), NA_real_)] }
QUANT <- QUANT[vapply(paste0("o_", key), function(v) sum(is.finite(B[[v]])) >= 400, logical(1))]

# shocks (п.п.) and macro controls
SH <- sys[, .(month, i = 100 * policy_avg, i_cd = 100 * fcoalesce(col_or_na(sys, "cd_on_rate_avg"), policy_avg),
              i_uonia = 100 * col_or_na(sys, "uonia_avg"), i_zc3m = 100 * col_or_na(sys, "zc_uah_3m_avg"),
              cpi = 100 * col_or_na(sys, "cpi_yoy"), fxd = 100 * col_or_na(sys, "fx_basket_dlog"), usd = col_or_na(sys, "usd_eom"))]
setorder(SH, month)
SH[, `:=`(di = i - shift(i), di_cd = i_cd - shift(i_cd), di_uonia = i_uonia - shift(i_uonia), di_zc3m = i_zc3m - shift(i_zc3m),
          dcpi = cpi - shift(cpi), dfx = fifelse(is.finite(fxd), fxd, 100 * (log(usd) - log(shift(usd)))))]
SH[, `:=`(m_dcpi = shift(dcpi), m_dfx = shift(dfx), i_l1 = shift(i))]
# expectations of economic agents (02: NBU surveys + market curve), pp; IODA and BLS are indices
EXV <- c(pie_mean = 100, pie_hh = 100, pie_analysts = 100, edep_mean = 100, edep_hh = 100, exp_path_3m = 100, exp_path_1y = 100,
         ioda = 1, bls_dem_nfc = 1)
EXV <- EXV[names(EXV) %in% names(sys)]
if (length(EXV)) {
  E <- sys[, c("month", names(EXV)), with = FALSE]; for (v in names(EXV)) E[, (v) := EXV[[v]] * get(v)]
  SH <- merge(SH, E, by = "month", all.x = TRUE); setorder(SH, month)
  for (v in names(EXV)) SH[, (v) := nafill(get(v), "locf")]          # survey months only; carry the last reading
}
SH[, `:=`(m_pie = shift(col_or_na(SH, "pie_mean")), m_edep = shift(col_or_na(SH, "edep_mean")), m_ioda = shift(col_or_na(SH, "ioda")))]
# Policy predictability: E_{t−1}[Δi_t] from information available at t−1 — market-implied path (NSS 3M − policy),
# survey inflation expectations, expected depreciation, last change, last CPI change. Fitted = anticipated part,
# residual = surprise (Kuttner 2001; Romer & Romer 2004 logic at monthly frequency).
PRED <- intersect(c("exp_path_3m", "pie_mean", "edep_mean", "di", "dcpi"), names(SH))
for (v in PRED) SH[, (paste0("p_", v)) := shift(get(v))]
pv <- paste0("p_", PRED)
SH[, `:=`(di_ant = NA_real_, di_sur = NA_real_)]
okp <- is.finite(SH$di) & Reduce(`&`, lapply(pv, function(v) is.finite(SH[[v]])))
POLREG <- NULL
if (sum(okp) >= 36) {
  fpr <- lm(reformulate(pv, "di"), data = SH[okp])
  Vp <- sandwich::NeweyWest(fpr, lag = 3, prewhite = FALSE)
  SH[okp, di_ant := fitted(fpr)]; SH[okp, di_sur := di - di_ant]
  POLREG <- data.table(term = names(coef(fpr)), b = coef(fpr), se = sqrt(diag(Vp)))[, p := 2 * pnorm(-abs(b / se))]
  POLREG[, `:=`(r2 = summary(fpr)$r.squared, n = nobs(fpr), sd_di = sd(SH$di[okp]), sd_sur = sd(SH$di_sur[okp]))]
}
I_MED <- median(SH$i, na.rm = TRUE)
SH[, `:=`(st_high = as.numeric(i_l1 >= I_MED), st_war = as.numeric(month >= as.Date("2022-03-01")))]
B <- merge(B, SH, by = "month", all.x = TRUE); setorder(B, nkb, month)
MACRO <- intersect(c("m_dcpi", "m_dfx"), names(B)[vapply(B, function(x) is.numeric(x) && mean(is.finite(x)) > .5, logical(1))])
if (!is.null(POLREG)) {
  save_tab(POLREG, "S22_P13a_policy_predictability.csv", "P13a_policy_predictability")
  save_tab(SH[, .(month, i, di, di_ant, di_sur)], "S22_P13a_policy_surprises_monthly.csv", "P13a_surprises")
  fact("P13a_predictability", sprintf("R² = %.2f, N = %d: частка дисперсії Δi, передбачувана з інформації t−1; с.в. несподіванки %.2f vs Δi %.2f в.п.",
       POLREG$r2[1], POLREG$n[1], POLREG$sd_sur[1], POLREG$sd_di[1]))
}

# bank characteristics: lagged one month, winsorised 1/99, standardised
B[, `:=`(c_size = log(pmax(col_or_na(B, "fin_assets"), 1)), c_demand = col_or_na(B, "demand_share"),
         c_liquid = (fcoalesce(col_or_na(B, "cd_nbu_total"), 0) + fcoalesce(col_or_na(B, "ovdp_total"), 0)) / col_or_na(B, "fin_assets"),
         c_capital = col_or_na(B, "fin_equity") / col_or_na(B, "fin_assets"), c_l2d = col_or_na(B, "loans_to_dep"),
         c_fx = col_or_na(B, "fx_share"), c_state = as.numeric(owner == "State"), c_foreign = as.numeric(owner == "Foreign"))]
if (!"dep_core4_total" %in% names(B)) B[, dep_core4_total := NA_real_]
B[, c_mshare := dep_core4_total / sum(dep_core4_total, na.rm = TRUE), by = month]
CHARS <- c(c_size = "Розмір (log активів)", c_demand = "Частка коштів на вимогу", c_liquid = "Частка ДС + ОВДП в активах",
           c_capital = "Капітал / активи", c_l2d = "Кредити / депозити", c_fx = "Частка валютних депозитів",
           c_mshare = "Частка ринку депозитів", c_state = "Державний банк", c_foreign = "Іноземний банк")
# Supplemental characteristics do not shrink the baseline sample. P6 estimates each
# observed characteristic separately, with bank and month FE and one-month lags.
B[, `:=`(c_headroom = col_or_na(B, "headroom_total_min_pp"),
         c_tier1_headroom = col_or_na(B, "headroom_tier1_min_pp"),
         c_lcr = col_or_na(B, "lcr_all_pct"), c_nsfr = col_or_na(B, "nsfr_pct"))]
CHARS <- c(CHARS, c_headroom = "Запас до загального мінімуму капіталу, в.п.",
           c_tier1_headroom = "Запас Tier 1 до загального мінімуму, в.п.",
           c_lcr = "LCR за всіма валютами", c_nsfr = "NSFR")
for (v in names(CHARS)) {
  B[, (v) := shift(get(v)), by = nkb]
  x <- B[[v]]
  if (!v %in% c("c_state", "c_foreign")) { q <- quantile(x, c(.01, .99), na.rm = TRUE, names = FALSE); x <- pmin(pmax(x, q[1]), q[2]) }
  B[, (paste0("z_", v)) := x]
  # cross-sectional standardisation within each month: z = deviation from the month's mean, in that month's s.d.
  if (!v %in% c("c_state", "c_foreign")) B[, (paste0("z_", v)) := { m <- mean(get(paste0("z_", v)), na.rm = TRUE); sdv <- sd(get(paste0("z_", v)), na.rm = TRUE)
    if (is.finite(sdv) && sdv > 0) (get(paste0("z_", v)) - m) / sdv else NA_real_ }, by = month]
}
CTRL_CH <- c("z_c_size", "z_c_demand", "z_c_liquid", "z_c_capital")
B[, w_dep := shift(dep_core4_total), by = nkb]
status("P0 panel", "ok", sprintf("%d банків, %d банко-місяців, %s…%s; ставки: %s; обсяги: %s", uniqueN(B$nkb), nrow(B), min(B$month), max(B$month),
                                  paste(RATES$var, collapse = ", "), paste(QUANT$var, collapse = ", ")))
fact("P0_sample", sprintf("%d банків, %d банко-місяців", uniqueN(B$nkb), nrow(B)))
fact("P0_policy_median", sprintf("%.2f%%", I_MED), "поріг стану «висока / низька облікова»")

# ============================== estimators ===================================
fe_dk_v <- function(d, y, x, w = NULL, L = NULL) {
  cols <- c(y, x, w); ok <- complete.cases(d[, ..cols]); if (sum(ok) < length(x) + 40) return(NULL)
  d <- d[ok]; Z <- as.matrix(d[, c(y, x), with = FALSE]); g <- d$nkb
  ww <- if (is.null(w)) rep(1, nrow(d)) else { v <- d[[w]]; v <- pmax(v, 0); v / mean(v) }
  mw <- rowsum(Z * ww, g) / as.vector(rowsum(ww, g)); Z <- Z - mw[g, , drop = FALSE]
  s <- sqrt(ww); yv <- Z[, 1] * s; X <- Z[, -1, drop = FALSE] * s
  keep <- apply(X, 2, function(c) sd(c) > 1e-10); if (!all(keep[seq_len(min(2, length(keep)))])) return(NULL)
  X <- X[, keep, drop = FALSE]
  XtX <- crossprod(X); if (rcond(XtX) < 1e-12) return(NULL)
  Bi <- solve(XtX); b <- Bi %*% crossprod(X, yv); u <- as.vector(yv - X %*% b)
  hh <- rowsum(X * u, as.character(d$month)); hh <- hh[order(rownames(hh)), , drop = FALSE]; Tn <- nrow(hh)
  L <- L %||% floor(4 * (Tn / 100)^(2 / 9)); S <- crossprod(hh)
  for (l in seq_len(min(L, Tn - 1))) { G <- crossprod(hh[-(1:l), , drop = FALSE], hh[1:(Tn - l), , drop = FALSE]); S <- S + (1 - l / (L + 1)) * (G + t(G)) }
  V <- Bi %*% S %*% Bi; dimnames(V) <- list(colnames(X), colnames(X))
  list(b = setNames(as.vector(b), colnames(X)), V = V, n = nrow(d), nb = data.table::uniqueN(d$nkb), T = Tn)
}

# two-way (bank + month) FE by alternating projections; Driscoll–Kraay covariance
fe2_dk_v <- function(d, y, x, L = NULL, tol = 1e-9, maxit = 500, vcov = c("dk", "bank")) {
  vcov <- match.arg(vcov)
  cols <- c(y, x); ok <- complete.cases(d[, ..cols]); if (sum(ok) < length(x) + 60) return(NULL)
  d <- d[ok]; Z <- as.matrix(d[, ..cols]); g1 <- d$nkb; g2 <- as.character(d$month)
  n1 <- table(g1); n2 <- table(g2)
  for (it in seq_len(maxit)) { Z0 <- Z
    Z <- Z - (rowsum(Z, g1) / as.vector(n1[sort(unique(g1))]))[g1, , drop = FALSE]
    Z <- Z - (rowsum(Z, g2) / as.vector(n2[sort(unique(g2))]))[g2, , drop = FALSE]
    if (max(abs(Z - Z0)) < tol) break }
  yv <- Z[, 1]; X <- Z[, -1, drop = FALSE]
  keep <- apply(X, 2, function(c) sd(c) > 1e-10); X <- X[, keep, drop = FALSE]; if (!ncol(X)) return(NULL)
  XtX <- crossprod(X); if (rcond(XtX) < 1e-12) return(NULL)
  Bi <- solve(XtX); b <- Bi %*% crossprod(X, yv); u <- as.vector(yv - X %*% b)
  Tn <- data.table::uniqueN(g2)
  if (vcov == "bank") {                     # cluster by bank: event-time dummies are month-specific, so time clusters degenerate
    hb <- rowsum(X * u, g1); G <- nrow(hb); S <- crossprod(hb) * G / (G - 1) * (nrow(X) - 1) / (nrow(X) - ncol(X))
  } else {
    hh <- rowsum(X * u, g2); hh <- hh[order(rownames(hh)), , drop = FALSE]
    L <- L %||% floor(4 * (Tn / 100)^(2 / 9)); S <- crossprod(hh)
    for (l in seq_len(min(L, Tn - 1))) { G <- crossprod(hh[-(1:l), , drop = FALSE], hh[1:(Tn - l), , drop = FALSE]); S <- S + (1 - l / (L + 1)) * (G + t(G)) }
  }
  V <- Bi %*% S %*% Bi; dimnames(V) <- list(colnames(X), colnames(X))
  list(b = setNames(as.vector(b), colnames(X)), V = V, n = nrow(d), nb = data.table::uniqueN(d$nkb), T = Tn)
}

# panel LP: cumulative response y_{t+h} − y_{t−1} to the shock at t
lp_panel <- function(D, ov, spec = "base", shock = "di", H = H_MAX, nlag = N_LAG, chars = CTRL_CH, w = NULL, het = NULL, hs = 0:H,
                     twoway = FALSE, ldy = TRUE, extra = NULL, regime_filter = NULL) {
  keep <- intersect(unique(c("nkb", "month", if (!is.null(regime_filter)) "capital_regime", ov, shock, "st_high", "st_war", "ms_p", "di_ant", "di_sur", MACRO, extra, chars, het, w)), names(D))
  D <- copy(D[, ..keep]); if (!"ms_p" %in% names(D)) D[, ms_p := NA_real_]
  setnames(D, c(ov, shock), c(".y", ".s")); setorder(D, nkb, month)
  D[, .dy := .y - shift(.y), by = nkb]
  ctrl <- c(MACRO, intersect(extra, names(D)), chars)
  for (l in seq_len(nlag)) { D[, (paste0(".ldy", l)) := shift(.dy, l), by = nkb]; D[, (paste0(".ls", l)) := shift(.s, l), by = nkb]
    ctrl <- c(ctrl, if (ldy) paste0(".ldy", l), paste0(".ls", l)) }
  terms <- switch(spec,
    base = { ".s" },
    asym = { D[, `:=`(.s_up = pmax(.s, 0), .s_dn = pmin(.s, 0))]; c(".s_up", ".s_dn") },
    level = { D[, `:=`(.s_hi = .s * st_high, .s_lo = .s * (1 - st_high))]; ctrl <- c(ctrl, "st_high"); c(".s_hi", ".s_lo") },
    war = { D[, `:=`(.s_hi = .s * st_war, .s_lo = .s * (1 - st_war))]; ctrl <- c(ctrl, "st_war"); c(".s_hi", ".s_lo") },
    ms = { D[, `:=`(.s_hi = .s * ms_p, .s_lo = .s * (1 - ms_p))]; ctrl <- c(ctrl, "ms_p"); c(".s_hi", ".s_lo") },
    exp = { D[, `:=`(.s_ant = di_ant, .s_sur = di_sur)]; c(".s_ant", ".s_sur") },
    het = { tt <- ".s"; for (z in het) { nm <- paste0(".sx_", z); D[, (nm) := .s * get(z)]; tt <- c(tt, nm) }; ctrl <- unique(c(ctrl, het)); tt })
  if (twoway) {                                  # month FE absorb everything that varies only over time
    terms <- setdiff(terms, ".s"); ctrl <- setdiff(ctrl, c(MACRO, paste0(".ls", seq_len(nlag)), "st_high", "st_war", "ms_p")) }
  rbindlist(lapply(hs, function(h) {
    D[, .lhs := shift(.y, h, type = "lead") - shift(.y, 1), by = nkb]
    if (!is.null(regime_filter)) D[, .regok := capital_regime == regime_filter & shift(capital_regime, 1) == regime_filter & shift(capital_regime, h, type = "lead") == regime_filter, by = nkb]
    DD <- if (is.null(regime_filter)) D else D[.regok %in% TRUE]
    f <- if (twoway) fe2_dk_v(DD, ".lhs", c(terms, setdiff(ctrl, terms)), L = max(h + 1L, 3L)) else fe_dk_v(DD, ".lhs", c(terms, setdiff(ctrl, terms)), w = w, L = max(h + 1L, 3L))
    if (is.null(f)) return(NULL)
    tt <- intersect(terms, names(f$b))
    out <- data.table(h = h, term = tt, b = f$b[tt], se = sqrt(diag(f$V)[tt]), n = f$n, nb = f$nb)
    if (length(tt) == 2 && spec != "het") { d <- f$b[tt[1]] - f$b[tt[2]]; sd <- sqrt(f$V[tt[1], tt[1]] + f$V[tt[2], tt[2]] - 2 * f$V[tt[1], tt[2]]); out[, `:=`(diff = d, p_diff = 2 * pnorm(-abs(d / sd)))] }
    out
  }), fill = TRUE)
}
TERM_LAB <- c(.s = "Δ облікової", .s_up = "Підвищення", .s_dn = "Зниження", .s_hi = "Стан 1", .s_lo = "Стан 0")
ci_plot <- function(d, title, colour_by = "term", ncol = 3, ylab = "в.п. на 1 в.п. облікової") {
  ggplot(d, aes(h, b, colour = .data[[colour_by]], fill = .data[[colour_by]])) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
    geom_ribbon(aes(ymin = b - 1.645 * se, ymax = b + 1.645 * se), alpha = .15, colour = NA) + geom_line(linewidth = .75) +
    facet_wrap(~label, ncol = ncol, scales = "free_y") + scale_colour_manual(values = PAL) + scale_fill_manual(values = PAL) +
    labs(title = title, subtitle = "Панельні локальні проєкції, FE банку, похибки Дріскола–Краая; 90% інтервали", x = "Горизонт, місяців", y = ylab, caption = CAP) + theme_thesis()
}
# half-life of pass-through: first horizon at which the cumulative response reaches half of its maximum
half_life <- function(h, b) { b <- b[is.finite(b)]; if (!length(b) || max(b) <= 0) return(NA_real_); k <- which(b >= .5 * max(b))[1]; if (is.na(k)) NA_real_ else as.numeric(h[k]) }

# ============================== P4. MARKOV SWITCHING ==========================
t0 <- Sys.time()
SY <- sys[, .(month, i = 100 * policy_avg, cod4 = 100 * cod4_total, uird3m = 100 * col_or_na(sys, "uird_uah_3m_avg"),
              mir_hh = 100 * fcoalesce(col_or_na(sys, "fm_dep_hh_uah_1y"), col_or_na(sys, "nb_dep_hh_term_uah")),
              mir_nfc = 100 * fcoalesce(col_or_na(sys, "fm_dep_nfc_uah_1y"), col_or_na(sys, "nb_dep_nfc_term_uah")),
              mir_loan = 100 * fcoalesce(col_or_na(sys, "fm_loan_nfc_uah_xo"), col_or_na(sys, "nb_loan_nfc_all_uah")),
              book_hh = 100 * col_or_na(sys, "fm_out_dep_hh_uah"))]
if (nrow(sast) && has(sast, "y_iea")) SY <- merge(SY, sast[, .(month, y_iea = 100 * y_iea)], by = "month", all.x = TRUE)
setorder(SY, month); SY <- SY[is.finite(i)]
FMS <- has(sys, "fm_dep_hh_uah_1y")
MS_SER <- c(cod4 = "Вартість депозитів (книжкова)", uird3m = "UIRD 3M (оголошені ставки ФО)",
            mir_hh = if (FMS) "Нові депозити ФО до 1 р., грн" else "Нові строкові депозити ФО (MIR)",
            mir_nfc = if (FMS) "Нові депозити СГ до 1 р., грн" else "Нові строкові депозити СГ (MIR)",
            mir_loan = if (FMS) "Нові кредити СГ, грн (без овердрафту)" else "Нові кредити СГ (MIR)",
            y_iea = "Дохідність процентних активів")   # outstanding-rate series are noisy (composition jumps) -> used in 09 (DOLS), not in MS
MS_SER <- MS_SER[names(MS_SER) %in% names(SY)]
MS_SER <- MS_SER[vapply(names(MS_SER), function(v) sum(is.finite(SY[[v]])) >= 48, logical(1))]
SY[, di := i - shift(i)]; SY[, di_l1 := shift(di)]
MSA <- list(); MSB <- list(); ms_probs <- list(); ms_coef <- list(); ms_fitstat <- list()
for (v in names(MS_SER)) {
  y <- SY[[v]]; dy <- y - shift(y)
  XA <- cbind(const = 1, di = SY$di, di_l1 = SY$di_l1, dy_l1 = shift(dy))
  fa2 <- tryCatch(ms_fit(dy, XA, K = 2, sw = c(TRUE, TRUE, TRUE, FALSE), order_by = "di", dates = SY$month, n_start = MS_STARTS), error = function(e) NULL)
  fa3 <- if (v %in% c("cod4", "uird3m")) tryCatch(ms_fit(dy, XA, K = 3, sw = c(TRUE, TRUE, TRUE, FALSE), order_by = "di", dates = SY$month, n_start = max(4L, MS_STARTS %/% 2L)), error = function(e) NULL) else NULL
  XB <- cbind(const = 1, i = SY$i, r_l1 = shift(y))
  fb2 <- tryCatch(ms_fit(y, XB, K = 2, sw = c(TRUE, TRUE, TRUE), order_by = "i", dates = SY$month, n_start = MS_STARTS), error = function(e) NULL)
  for (nm in c("A2", "A3", "B2")) {
    f <- switch(nm, A2 = fa2, A3 = fa3, B2 = fb2); if (is.null(f)) next
    ms_coef[[length(ms_coef) + 1]] <- f$coef[, `:=`(series = v, label = MS_SER[[v]], model = nm)]
    ms_fitstat[[length(ms_fitstat) + 1]] <- data.table(series = v, label = MS_SER[[v]], model = nm, K = f$K, n = f$n, loglik = f$ll, loglik_linear = f$ll_linear,
      LR = f$LR, degenerate = f$degenerate, AIC = f$aic, AIC_linear = f$aic_linear, BIC = f$bic, BIC_linear = f$bic_linear, RCM = ms_rcm(f),
      dur = paste(sprintf("%.1f", f$durations), collapse = " / "), ergodic = paste(sprintf("%.2f", f$ergodic), collapse = " / "))
    pr <- copy(f$probs); pc <- grep("^smoothed", names(pr), value = TRUE)
    ms_probs[[length(ms_probs) + 1]] <- data.table(month = SY$month, series = v, label = MS_SER[[v]], model = nm,
                                                   p_high = pr[[pc[length(pc)]]], p_high_filt = pr[[grep("^filtered", names(pr), value = TRUE)[length(pc)]]],
                                                   regime = f$regime)
  }
  MSA[[v]] <- fa2; MSB[[v]] <- fb2
}
MSC <- rbindlist(ms_coef, fill = TRUE); MSF <- rbindlist(ms_fitstat, fill = TRUE); MSP <- rbindlist(ms_probs, fill = TRUE)
# long-run pass-through and half-life by regime (model B)
lrB <- rbindlist(lapply(names(MSB), function(v) { f <- MSB[[v]]; if (is.null(f)) return(NULL)
  Bm <- f$par$B; data.table(series = v, label = MS_SER[[v]], regime = seq_len(ncol(Bm)), b_short = Bm["i", ], rho = Bm["r_l1", ],
                            theta_long = Bm["i", ] / (1 - Bm["r_l1", ]), half_life_m = fifelse(Bm["r_l1", ] > 0 & Bm["r_l1", ] < 1, log(.5) / log(Bm["r_l1", ]), NA_real_),
                            sigma = f$par$sig, p_stay = diag(f$par$P), duration_m = f$durations) }))
save_tab(MSC, "S22_P4a_markov_coefficients.csv", "P4_MS_coef")
save_tab(MSF, "S22_P4b_markov_fit_vs_linear.csv", "P4_MS_fit")
save_tab(lrB, "S22_P4c_markov_longrun_by_regime.csv", "P4_MS_longrun")
save_tab(MSP, "S22_P4d_markov_regime_probabilities.csv", "P4_MS_probs")
if (nrow(MSP)) {
  pp <- MSP[model == "A2"]
  pol <- SY[, .(month, i)]; sc <- max(pol$i, na.rm = TRUE)
  p <- ggplot(pp, aes(month, p_high)) + geom_area(fill = "grey75", alpha = .8) +
    geom_line(data = merge(pp[, .(month, label)], pol, by = "month"), aes(month, i / sc), colour = INK, linewidth = .6) +
    facet_wrap(~label, ncol = 2) + scale_y_continuous(labels = label_percent(1), sec.axis = sec_axis(~ . * sc, name = "Облікова ставка, %")) +
    labs(title = "Ймовірність режиму сильної передачі (MS-ADL, згладжена)", subtitle = "Сіра площа — Pr(режим 2); лінія — облікова ставка (права шкала)",
         x = NULL, y = "Ймовірність режиму 2", caption = CAP) + theme_thesis()
  save_fig(p, "S22_P4_markov_probabilities.png", 8.5, 8)
  pb <- MSP[model == "B2"]
  if (nrow(pb)) {
    p <- ggplot(pb, aes(month, p_high)) + geom_area(fill = "grey75", alpha = .8) +
      geom_line(data = merge(pb[, .(month, label)], pol, by = "month"), aes(month, i / sc), colour = INK, linewidth = .6) +
      facet_wrap(~label, ncol = 2) + scale_y_continuous(labels = label_percent(1), sec.axis = sec_axis(~ . * sc, name = "Облікова ставка, %")) +
      labs(title = "Режими довгострокової передачі (MS часткового коригування, згладжена ймовірність)",
           subtitle = "r = a_S + b_S·i + ρ_S·r₋₁; сіра площа — Pr(режим з вищою короткостроковою передачею b_S)", x = NULL, y = "Ймовірність режиму 2", caption = CAP) + theme_thesis()
    save_fig(p, "S22_P4b_markov_probabilities_levels.png", 8.5, 8)
  }
  bb <- MSC[model == "A2" & grepl("^di \\|", term)][, regime := sub(".*режим ", "Режим ", term)]
  if (nrow(bb)) {
    p <- ggplot(bb, aes(estimate, label, shape = regime)) + geom_vline(xintercept = 0, colour = INK2, linewidth = .3) +
      geom_errorbarh(aes(xmin = estimate - 1.645 * se, xmax = estimate + 1.645 * se), height = .25, position = position_dodge(.5)) +
      geom_point(size = 2.2, position = position_dodge(.5)) + scale_shape_manual(values = c(1, 16, 17)) +
      labs(title = "Миттєва передача облікової ставки за режимами Маркова", subtitle = "β0 у Δr = c + β0·Δi + β1·Δi₋₁ + ρ·Δr₋₁; 90% інтервали",
           x = "β0, в.п. на 1 в.п.", y = NULL, caption = CAP) + theme_thesis()
    save_fig(p, "S22_P4_markov_betas.png", 8.5, 4.5)
  }
  # regimes vs policy phases (03: T18)
  cyc <- tryCatch(fread(file.path(OUTPUT_DIR, "section_2_1_v2", "tables", "T18_policy_cycles.csv")), error = function(e) data.table())
  if (nrow(cyc)) {
    cyc[, `:=`(start = as.Date(start), end = as.Date(end))]
    pp[, phase := cyc$regime[findInterval(as.numeric(month), as.numeric(cyc$start))]]
    xt <- pp[!is.na(phase), .(months = .N, mean_p_high = mean(p_high, na.rm = TRUE)), by = .(label, phase)]
    save_tab(dcast(xt, label ~ phase, value.var = "mean_p_high"), "S22_P4e_markov_vs_policy_phases.csv", "P4_MS_vs_phases")
  }
  for (v in unique(MSF$series)) { z <- MSF[series == v & model == "A2"]; if (!nrow(z)) next
    zb <- MSC[series == v & model == "A2" & grepl("^di \\|", term)]
    fact(paste0("P4_MS_", v), sprintf("β0: %s; тривалість режимів %s міс.; LR = %.1f; BIC MS %.1f vs лінійна %.1f; RCM %.0f",
         paste(sprintf("%.2f (%.2f)", zb$estimate, zb$se), collapse = " / "), z$dur, z$LR, z$BIC, z$BIC_linear, z$RCM)) }
}
# state variable for the panel: filtered Pr(strong pass-through) of book deposit cost, t−1
MS_STATE <- if (!is.null(MSA[["cod4"]])) "cod4" else if (length(MSA)) names(MSA)[1] else NA
if (!is.na(MS_STATE)) {
  sp <- MSP[series == MS_STATE & model == "A2", .(month, ms_p = shift(p_high_filt))]
  B <- merge(B, sp, by = "month", all.x = TRUE); setorder(B, nkb, month)
  status("P4 Markov", "ok", sprintf("%d рядів; стан для панелі: %s (фільтрована ймовірність, t−1); %.1f хв", length(MS_SER), MS_STATE, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
} else { B[, ms_p := NA_real_]; status("P4 Markov", "skipped", "немає рядів") }

# ============================== P1–P3, P5. PANEL LP ===========================
t0 <- Sys.time()
OUTS <- rbind(RATES[, .(key, label, side, kind = "rate")], QUANT[, .(key, label, side = "Обсяги", kind = "log")])
run_spec <- function(spec, w = NULL, ...) rbindlist(lapply(seq_len(nrow(OUTS)), function(k) {
  r <- lp_panel(B, paste0("o_", OUTS$key[k]), spec = spec, w = w, ...); if (is.null(r) || !nrow(r)) return(NULL)
  r[, `:=`(key = OUTS$key[k], label = OUTS$label[k], side = OUTS$side[k], kind = OUTS$kind[k])] }), fill = TRUE)
LP <- rbindlist(list(run_spec("base")[, spec := "base"], run_spec("base", w = "w_dep")[, spec := "base_w"],
                     run_spec("asym")[, spec := "asym"], run_spec("level")[, spec := "level"], run_spec("war")[, spec := "war"],
                     if (!is.na(MS_STATE)) run_spec("ms")[, spec := "ms"],
                     if (sum(is.finite(B$di_sur)) > 1000) run_spec("exp")[, spec := "exp"]), fill = TRUE)
LP[, term_lab := fcase(term == ".s", "Δ облікової", term == ".s_up", "Підвищення ставки", term == ".s_dn", "Зниження ставки",
                       spec == "level" & term == ".s_hi", sprintf("Облікова ≥ %.1f%%", I_MED), spec == "level" & term == ".s_lo", sprintf("Облікова < %.1f%%", I_MED),
                       spec == "war" & term == ".s_hi", "Війна (з 2022-03)", spec == "war" & term == ".s_lo", "До війни",
                       spec == "ms" & term == ".s_hi", "Режим сильної передачі", spec == "ms" & term == ".s_lo", "Режим слабкої передачі",
                       term == ".s_ant", "Очікувана зміна облікової", term == ".s_sur", "Несподівана зміна облікової", default = NA_character_)]
LP[is.na(term_lab), term_lab := term]
save_tab(LP, "S22_P1_P5_panel_local_projections.csv", "P1_P5_LP")
status("P1–P5 LP", "ok", sprintf("%d рядів, %.1f хв", nrow(LP), as.numeric(difftime(Sys.time(), t0, units = "mins"))))

rate_lab <- function(d) d[kind == "rate"]
p <- ci_plot(rate_lab(LP[spec %in% c("base", "base_w")])[, term_lab := fifelse(spec == "base", "Без ваг", "Зважено на депозити")],
             "Кумулятивна передача облікової ставки: панель банків", colour_by = "term_lab")
save_fig(p, "S22_P1_lp_baseline_rates.png", 8.5, 9.5)
for (sp in list(c("asym", "S22_P2_lp_asymmetry.png", "Асиметрія: підвищення vs зниження облікової ставки"),
                c("level", "S22_P3_lp_state_level.png", "Залежність від рівня облікової ставки"),
                c("war", "S22_P3b_lp_state_war.png", "Передача до і під час повномасштабної війни"),
                c("ms", "S22_P5_lp_markov_state.png", "Передача в режимах Маркова (фільтрована ймовірність, t−1)"),
                c("exp", "S22_P13_lp_anticipated_vs_surprise.png", "Очікувані та несподівані зміни облікової ставки"))) {
  d <- rate_lab(LP[spec == sp[1]]); if (!nrow(d)) next
  save_fig(ci_plot(d, sp[3], colour_by = "term_lab"), sp[2], 8.5, 9.5)
}
# tests table: difference between the two states at 3/6/12/24 months
TST <- LP[spec %in% c("asym", "level", "war", "ms", "exp") & h %in% c(3, 6, 12, 24) & term %in% c(".s_up", ".s_hi", ".s_ant"),
          .(spec, key, label, h, diff, p_diff)]
save_tab(dcast(TST, label + spec ~ paste0("h", h), value.var = c("diff", "p_diff")), "S22_P2_P3_P5_state_tests.csv", "P2_P5_tests")

# ============================== P6. HETEROGENEITY ===========================
t0 <- Sys.time()
HET_OUT <- intersect(c("cod4", "hh_term", "corp_term", "hh_demand", "y_le", "y_iea", "nim", "q_dep_term", "q_dep_demand", "q_loans_le"), OUTS$key)
het_one <- rbindlist(lapply(HET_OUT, function(k) rbindlist(lapply(paste0("z_", names(CHARS)), function(z) {
  regimes <- if (z %in% paste0("z_", c("c_headroom", "c_tier1_headroom", "c_lcr", "c_nsfr"))) c("legacy", "three_tier") else "pooled"
  rbindlist(lapply(regimes, function(reg) {
    r <- lp_panel(B, paste0("o_", k), spec = "het", het = z, chars = setdiff(CTRL_CH, z), hs = intersect(c(0, 1, 3, 6, 9, 12, 18, 24), 0:H_MAX), twoway = TRUE, regime_filter = if (reg == "pooled") NULL else reg)
    if (is.null(r) || !nrow(r)) return(NULL)
    r[term != ".s"][, `:=`(key = k, char = sub("^z_", "", z), capital_sample = reg)]
  }), fill = TRUE)
}))), fill = TRUE)
het_joint <- rbindlist(lapply(HET_OUT, function(k) {
  zs <- paste0("z_", c("c_size", "c_demand", "c_liquid", "c_capital", "c_l2d", "c_state", "c_foreign"))
  r <- lp_panel(B, paste0("o_", k), spec = "het", het = zs, chars = character(), hs = intersect(c(3, 6, 12, 24), 0:H_MAX), twoway = TRUE)
  if (is.null(r) || !nrow(r)) return(NULL); r[, key := k] }), fill = TRUE)
lab_out <- setNames(OUTS$label, OUTS$key)
if (nrow(het_one)) {
  het_one[, `:=`(label = lab_out[key], char_lab = CHARS[char])]
  save_tab(het_one, "S22_P6a_heterogeneity_single.csv", "P6_het_single")
  pk <- het_one[key %in% intersect(c("cod4", "y_le", "q_dep_term", "q_loans_le"), key) & char %in% c("c_size", "c_demand", "c_liquid", "c_capital", "c_state")]
  if (nrow(pk)) {
    p <- ggplot(pk, aes(h, b, colour = char_lab, fill = char_lab)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
      geom_ribbon(aes(ymin = b - 1.645 * se, ymax = b + 1.645 * se), alpha = .1, colour = NA) + geom_line(linewidth = .7) +
      facet_wrap(~label, ncol = 2, scales = "free_y") + scale_colour_manual(values = PAL) + scale_fill_manual(values = PAL) +
      labs(title = "Неоднорідність передачі: взаємодія Δi × характеристика банку (1 ст. відх. у межах місяця)",
           subtitle = "FE банку і місяця: порівнюються банки в той самий місяць; додатне — сильніша передача у банків з вищою характеристикою", x = "Горизонт, місяців", y = "Додаткова передача", caption = CAP) + theme_thesis()
    save_fig(p, "S22_P6_heterogeneity.png", 8.5, 7.5)
  }
}
if (nrow(het_joint)) {
  het_joint[, `:=`(label = lab_out[key], char = CHARS[sub("^\\.sx_z_", "", term)])]
  save_tab(dcast(het_joint, label + char ~ paste0("h", h), value.var = "b"), "S22_P6b_heterogeneity_joint_b.csv", "P6_het_joint_b")
  save_tab(dcast(het_joint, label + char ~ paste0("h", h), value.var = "se"), "S22_P6c_heterogeneity_joint_se.csv", "P6_het_joint_se")
}
status("P6 heterogeneity", "ok", sprintf("%.1f хв", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

# ============================== P7. QUANTITIES ==============================
q <- LP[kind == "log" & spec %in% c("base", "asym")]
if (nrow(q)) {
  p <- ci_plot(q, "Обсяги балансу: депозитний і кредитний канали", colour_by = "term_lab", ylab = "%, на 1 в.п. облікової")
  save_fig(p, "S22_P7_lp_quantities.png", 8.5, 9.5)
}
qh <- het_one[key %in% c("q_dep_term", "q_dep_demand", "q_loans_le") & char %in% c("c_demand", "c_liquid", "c_capital", "c_mshare") & h %in% c(6, 12, 24)]
if (nrow(qh)) save_tab(dcast(qh, label + char_lab ~ paste0("h", h), value.var = c("b", "se")), "S22_P7_quantity_channels_heterogeneity.csv", "P7_channels")

# ============================== P8. MEAN-GROUP ECM ============================
ecm_one <- function(r, i, min_n = 36) {
  dr <- r - shift(r); di <- i - shift(i); rl <- shift(r); il <- shift(i)
  X <- cbind(1, rl, il, di, shift(di), shift(dr)); ok <- is.finite(dr) & apply(is.finite(X), 1, all)
  if (sum(ok) < min_n) return(NULL)
  f <- lm.fit(X[ok, ], dr[ok]); b <- f$coefficients; if (any(!is.finite(b[1:3])) || abs(b[2]) < 1e-6) return(NULL)
  phi <- b[2]; theta <- -b[3] / b[2]; c0 <- -b[1] / b[2]
  ec <- rl - c0 - theta * il
  X2 <- cbind(1, pmax(ec, 0), pmin(ec, 0), di, shift(di), shift(dr)); ok2 <- is.finite(dr) & apply(is.finite(X2), 1, all)
  b2 <- tryCatch(lm.fit(X2[ok2, ], dr[ok2])$coefficients, error = function(e) rep(NA_real_, 6))
  data.table(phi = phi, theta = theta, beta0 = b[4], hl = if (phi < 0 && phi > -1) log(.5) / log(1 + phi) else NA_real_,
             phi_above = b2[2], phi_below = b2[3], n = sum(ok))
}
ECM_OUT <- intersect(c("cod4", "hh_demand", "hh_term", "corp_demand", "corp_term", "c_ibl", "y_cd", "y_le", "y_iea"), RATES$key)
MG <- rbindlist(lapply(ECM_OUT, function(k) B[, ecm_one(get(paste0("o_", k)), i), by = nkb][, key := k]), fill = TRUE)
if (nrow(MG)) {
  bc <- B[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = nkb, .SDcols = paste0("z_", names(CHARS))]
  MG <- merge(MG, bc, by = "nkb", all.x = TRUE); MG[, label := lab_out[key]]
  MG <- MG[is.finite(theta) & abs(theta) < 5 & is.finite(phi) & phi > -1.5 & phi < 0.5]
  tm <- function(x) mean(x, trim = .1, na.rm = TRUE)
  MGS <- MG[, .(banks = .N, theta_mg = tm(theta), theta_med = median(theta, na.rm = TRUE), theta_se = sd(theta, na.rm = TRUE) / sqrt(.N),
                phi_mg = tm(phi), phi_se = sd(phi, na.rm = TRUE) / sqrt(.N), half_life_med = median(hl, na.rm = TRUE),
                beta0_mg = tm(beta0), phi_above = tm(phi_above), phi_below = tm(phi_below),
                p_asym = { d <- phi_above - phi_below; d <- d[is.finite(d)]; if (length(d) > 5) 2 * pnorm(-abs(mean(d) / (sd(d) / sqrt(length(d))))) else NA_real_ }),
            by = .(key, label)]
  save_tab(MG, "S22_P8a_ecm_bank_level.csv", "P8_ECM_banks"); save_tab(MGS, "S22_P8b_ecm_mean_group.csv", "P8_ECM_MG")
  # system-level ECM for comparison
  SYS_ECM <- rbindlist(lapply(intersect(names(MS_SER), names(SY)), function(v) { r <- ecm_one(SY[[v]], SY$i, 30); if (is.null(r)) NULL else r[, `:=`(series = v, label = MS_SER[[v]])] }), fill = TRUE)
  save_tab(SYS_ECM, "S22_P8c_ecm_system.csv", "P8_ECM_system")
  # determinants of long-run pass-through and speed across banks
  det <- rbindlist(lapply(unique(MG$key), function(k) { z <- MG[key == k]
    xs <- intersect(paste0("z_", c("c_size", "c_demand", "c_liquid", "c_capital", "c_state", "c_foreign")), names(z)); z <- z[complete.cases(z[, ..xs])]
    if (nrow(z) < 20) return(NULL)
    rbindlist(lapply(c("theta", "phi"), function(yv) { f <- lm(reformulate(xs, yv), data = z); V <- sandwich::vcovHC(f, type = "HC1")
      data.table(outcome = k, label = lab_out[[k]], dep = yv, term = names(coef(f)), b = coef(f), se = sqrt(diag(V)), n = nrow(z)) })) }), fill = TRUE)
  if (nrow(det)) { setnames(det, "outcome", "key"); det[, char := fifelse(term == "(Intercept)", "Константа", CHARS[sub("^z_", "", term)])]; save_tab(det, "S22_P8d_ecm_determinants.csv", "P8_ECM_determinants") }
  p <- ggplot(MG[key %in% intersect(c("cod4", "hh_term", "corp_term", "y_le"), key)], aes(theta)) +
    geom_histogram(bins = 25, fill = "grey70", colour = "black", linewidth = .2) + geom_vline(xintercept = 1, linetype = "22", colour = INK) +
    facet_wrap(~label, ncol = 2, scales = "free_y") +
    labs(title = "Довгострокова передача облікової ставки: розподіл за банками", subtitle = "θ з банківських моделей корекції відхилень; пунктир — повна передача (θ = 1)",
         x = "θ (довгострокова передача)", y = "Кількість банків", caption = CAP) + theme_thesis()
  save_fig(p, "S22_P8_ecm_theta_distribution.png", 8.5, 6)
  ra <- MGS[is.finite(phi_above) & is.finite(phi_below)]
  if (nrow(ra)) {
    rl <- melt(ra[, .(label, `Ставка вища за рівноважну` = phi_above, `Ставка нижча за рівноважну` = phi_below)], id.vars = "label")
    p <- ggplot(rl, aes(value, label, shape = variable)) + geom_vline(xintercept = 0, colour = INK2, linewidth = .3) + geom_point(size = 2.4) +
      scale_shape_manual(values = c(16, 1)) +
      labs(title = "Асиметричне коригування до рівноваги («ракети і пір'я»)", subtitle = "Середнє за банками φ⁺ і φ⁻; більш від'ємне — швидше коригування",
           x = "Швидкість коригування φ за місяць", y = NULL, caption = CAP) + theme_thesis()
    save_fig(p, "S22_P8b_rockets_feathers.png", 8.5, 4.6)
  }
  for (k in MGS$key) { z <- MGS[key == k]; fact(paste0("P8_MG_", k), sprintf("θ = %.2f (s.e. %.2f), φ = %.3f, медіанний напівперіод %.1f міс., φ⁺/φ⁻ = %.3f / %.3f (p = %.3f), %d банків",
       z$theta_mg, z$theta_se, z$phi_mg, z$half_life_med, z$phi_above, z$phi_below, z$p_asym, z$banks)) }
}

# ============================== P9. SUMMARY & GAP FOR 2.3 ====================
g1 <- function(x) if (length(x)) x[1] else NA_real_
SUMM <- LP[spec == "base", {
  o <- order(h); bb <- b[o]; hh <- h[o]
  .(b0 = g1(bb[hh == 0]), b1 = g1(bb[hh == 1]), b3 = g1(bb[hh == 3]), b6 = g1(bb[hh == 6]), b12 = g1(bb[hh == 12]), b24 = g1(bb[hh == 24]),
    se12 = g1(se[h == 12]), half_life = half_life(hh, bb), b_max = max(bb, na.rm = TRUE)) }, by = .(key, label, side, kind)]
ad <- LP[spec == "asym" & h == 12 & term == ".s_up", .(key, asym_up_minus_down_12 = diff, p_asym_12 = p_diff)]
st <- LP[spec == "level" & h == 12 & term == ".s_hi", .(key, high_minus_low_12 = diff, p_level_12 = p_diff)]
ms12 <- LP[spec == "ms" & h == 12 & term == ".s_hi", .(key, ms_strong_minus_weak_12 = diff, p_ms_12 = p_diff)]
ex12 <- LP[spec == "exp" & h == 12 & term == ".s_ant", .(key, ant_minus_sur_12 = diff, p_exp_12 = p_diff)]
ex0 <- LP[spec == "exp" & h == 0, .(b_ant0 = b[term == ".s_ant"][1], b_sur0 = b[term == ".s_sur"][1]), by = key]
SUMM <- Reduce(function(a, b) merge(a, b, by = "key", all.x = TRUE), list(SUMM, unique(ad, by = "key"), unique(st, by = "key"), unique(ms12, by = "key"),
                                                                        unique(ex12, by = "key"), unique(ex0, by = "key")))
if (exists("MGS")) SUMM <- merge(SUMM, MGS[, .(key, theta_mg, phi_mg, half_life_ecm = half_life_med, phi_above, phi_below, p_asym_ecm = p_asym)], by = "key", all.x = TRUE)
save_tab(SUMM, "S22_P9_summary_repricing.csv", "P9_summary")
sr_r <- SUMM[kind == "rate" & is.finite(b12)]
if (nrow(sr_r)) {
  sl <- melt(sr_r[, .(label, `3 міс.` = b3, `12 міс.` = b12, `24 міс.` = b24)], id.vars = "label")
  sl[, label := factor(label, levels = rev(sr_r[order(-b12), label]))]
  p <- ggplot(sl, aes(value, label, shape = variable)) + geom_vline(xintercept = c(0, 1), colour = INK2, linewidth = .3, linetype = c("solid", "22")) +
    geom_point(size = 2.2) + scale_shape_manual(values = c(1, 16, 15)) +
    labs(title = "Швидкість переоцінки: кумулятивна передача за горизонтами", subtitle = "Панельні LP, середній банк; пунктир — повна передача",
         x = "β (в.п. на 1 в.п. облікової)", y = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "S22_P9_repricing_speed.png", 8.5, 5.2)
}
for (k in SUMM$key) { z <- SUMM[key == k]; fact(paste0("P9_", k), sprintf("β3 = %.2f, β12 = %.2f (s.e. %.2f), β24 = %.2f; напівперіод %s міс.; асиметрія12 %+.2f (p = %.3f); стан12 %+.2f (p = %.3f)",
     z$b3, z$b12, z$se12, z$b24, ifelse(is.na(z$half_life), "—", z$half_life), z$asym_up_minus_down_12, z$p_asym_12, z$high_minus_low_12, z$p_level_12)) }
# bank-level long-run gap (assets vs liabilities) — input for 2.3
if (exists("MG") && all(c("y_iea", "c_ibl") %in% MG$key)) {
  gp <- dcast(MG[key %in% c("y_iea", "c_ibl", "cod4", "y_le")], nkb ~ key, value.var = "theta")
  gp[, gap_theta := y_iea - c_ibl]; gp <- merge(gp, unique(B[, .(nkb, owner)], by = "nkb"), by = "nkb", all.x = TRUE)
  save_tab(gp, "S22_P9b_bank_longrun_gap_for_2_3.csv", "P9_gap_for_2_3")
  fact("P9_gap_theta", sprintf("медіана θ(активи) − θ(зобов'язання) = %.2f; частка банків з додатним розривом %.0f%%", median(gp$gap_theta, na.rm = TRUE), 100 * mean(gp$gap_theta > 0, na.rm = TRUE)))
}

# ============================== P10. ROBUSTNESS ===============================
t0 <- Sys.time()
ROB_OUT <- intersect(c("cod4", "hh_term", "corp_term", "y_le", "y_iea", "q_dep_term", "q_loans_le"), OUTS$key)
bal <- B[, .N, by = nkb][N >= max(N) * .95, nkb]
variants <- list(
  list("Базова специфікація", B, "di", NULL, N_LAG, CTRL_CH),
  list("Шок: ставка ДС овернайт", B, "di_cd", NULL, N_LAG, CTRL_CH),
  list("Шок: UONIA", B, "di_uonia", NULL, N_LAG, CTRL_CH),
  list("Шок: ОВДП 3М (NSS)", B, "di_zc3m", NULL, N_LAG, CTRL_CH),
  list("Без 2022-02…2022-07", B[!(month >= as.Date("2022-02-01") & month <= as.Date("2022-07-01"))], "di", NULL, N_LAG, CTRL_CH),
  list("Режим ДС (з 2023-04)", B[month >= as.Date("2023-04-01")], "di", NULL, N_LAG, CTRL_CH),
  list("Збалансована панель", B[nkb %in% bal], "di", NULL, N_LAG, CTRL_CH),
  list("Зважено на депозити", B, "di", "w_dep", N_LAG, CTRL_CH),
  list("6 лагів", B, "di", NULL, 6L, CTRL_CH),
  list("Без характеристик банку", B, "di", NULL, N_LAG, character()),
  list("Без лагів залежної змінної (Nickell)", B, "di", NULL, N_LAG, CTRL_CH, FALSE),
  list("Шок: несподівана складова", B, "di_sur", NULL, N_LAG, CTRL_CH),
  list("Контроль очікувань (π^e, Δs^e)", B, "di", NULL, N_LAG, CTRL_CH, TRUE, c("m_pie", "m_edep")))
ROB <- rbindlist(lapply(variants, function(vv) {
  if (!vv[[3]] %in% names(vv[[2]]) || sum(is.finite(vv[[2]][[vv[[3]]]])) < 24 || sd(vv[[2]][[vv[[3]]]], na.rm = TRUE) == 0) return(NULL)
  rbindlist(lapply(ROB_OUT, function(k) { r <- lp_panel(vv[[2]], paste0("o_", k), shock = vv[[3]], w = vv[[4]], nlag = vv[[5]], chars = vv[[6]], hs = intersect(c(6, 12), 0:H_MAX),
                                                     ldy = if (length(vv) >= 7) vv[[7]] else TRUE, extra = if (length(vv) >= 8) vv[[8]] else NULL)
    if (is.null(r) || !nrow(r)) NULL else r[, `:=`(variant = vv[[1]], key = k)] }), fill = TRUE) }), fill = TRUE)
if (nrow(ROB)) {
  ROB[, label := lab_out[key]]; save_tab(dcast(ROB[h == 12], label ~ variant, value.var = "b"), "S22_P10_robustness_b12.csv", "P10_robust_b12")
  save_tab(ROB, "S22_P10_robustness_long.csv", "P10_robust_long")
  rr <- ROB[h == 12 & key %in% intersect(c("cod4", "y_le", "q_dep_term"), key)]
  rr[, variant := factor(variant, levels = rev(vapply(variants, `[[`, "", 1)))]
  p <- ggplot(rr, aes(b, variant)) + geom_vline(xintercept = 0, colour = INK2, linewidth = .3) +
    geom_errorbarh(aes(xmin = b - 1.645 * se, xmax = b + 1.645 * se), height = .25) + geom_point(size = 2) +
    facet_wrap(~label, ncol = 3, scales = "free_x") +
    labs(title = "Стійкість оцінок: кумулятивна передача за 12 місяців", subtitle = "Панельні LP; 90% інтервали Дріскола–Краая",
         x = "β12", y = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "S22_P10_robustness.png", 8.5, 5.5)
}
status("P10 robustness", if (nrow(ROB)) "ok" else "skipped", sprintf("%.1f хв", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

# ============================== P11. DAILY EVENT STUDY (UIRD) ================
# Posted household deposit rates of the largest banks (UIRD, daily) around each
# change of the NBU key rate. Base = 2 business days before the effective date
# (i.e. before the announcement); window stops at the next rate change.
t0 <- Sys.time()
key <- store_read("raw_key"); UD <- tryCatch(read_uird_local(), error = function(e) NULL)
if (nrow(key) && !is.null(UD)) {
  kp <- unique(key[id_api == "KEY_PolicyRate", .(date = as.Date(parse_nbu_date(dt)), r = num(value))][order(date)], by = "date")
  ev <- kp[, .(date, r, dr = r - shift(r))][is.finite(dr) & abs(dr) > 1e-9]
  ev[, next_date := shift(date, type = "lead")]
  ud <- UD$daily[(ccy == "uah" & tenor %in% c("3m", "6m", "12m")) | (ccy == "usd" & tenor == "12m")]
  ud[, ser := paste0(toupper(ccy), " ", sub("m", " міс.", tenor))]; setorder(ud, ser, date)
  K_PRE <- 10L; K_POST <- 60L
  ESD <- rbindlist(lapply(seq_len(nrow(ev)), function(e) {
    d0 <- ev$date[e]; nxt <- ev$next_date[e]
    ud[, {
      pos0 <- which(date >= d0)[1]
      if (is.na(pos0) || pos0 <= 3) NULL else {
        base <- value[pos0 - 2L]; ks <- -K_PRE:K_POST; idx <- pos0 + ks; okk <- idx >= 1 & idx <= .N
        out <- data.table(k = ks[okk], d = date[idx[okk]], dv = value[idx[okk]] - base)
        if (!is.na(nxt)) out <- out[d < nxt]
        out }
    }, by = ser][, `:=`(event = e, date0 = d0, dr = ev$dr[e])]
  }), fill = TRUE)
  est_k <- function(z) {
    if (nrow(z) < 8 || sd(z$dr) == 0) return(NULL)
    f <- lm(dv ~ dr, data = z); V <- sandwich::vcovHC(f, type = "HC1")
    up <- pmax(z$dr, 0); dn <- pmin(z$dr, 0)
    f2 <- if (sum(up > 0) >= 3 && sum(dn < 0) >= 3) lm(z$dv ~ up + dn) else NULL; V2 <- if (!is.null(f2)) sandwich::vcovHC(f2, type = "HC1") else NULL
    data.table(b = coef(f)[2], se = sqrt(V[2, 2]), b_up = if (!is.null(f2)) coef(f2)[2] else NA_real_, se_up = if (!is.null(f2)) sqrt(V2[2, 2]) else NA_real_,
               b_dn = if (!is.null(f2)) coef(f2)[3] else NA_real_, se_dn = if (!is.null(f2)) sqrt(V2[3, 3]) else NA_real_, events = nrow(z))
  }
  ESE <- ESD[, est_k(.SD), by = .(ser, k)]
  save_tab(ev, "S22_P11a_nbu_rate_changes.csv", "P11_events")
  save_tab(ESE, "S22_P11b_daily_event_study_uird.csv", "P11_daily_ES")
  if (nrow(ESE)) {
    p <- ggplot(ESE, aes(k, b, colour = ser, fill = ser)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + geom_vline(xintercept = 0, colour = INK2, linewidth = .3, linetype = "22") +
      geom_ribbon(aes(ymin = b - 1.645 * se, ymax = b + 1.645 * se), alpha = .12, colour = NA) + geom_line(linewidth = .7) +
      scale_colour_manual(values = PAL) + scale_fill_manual(values = PAL) +
      labs(title = "Денна передача: оголошені ставки за депозитами після рішень НБУ", subtitle = sprintf("Зміна UIRD від дня до оголошення на 1 в.п. зміни облікової; %d рішень 2018–2026; 90%% інтервали HC1", nrow(ev)),
           x = "Робочі дні від набуття чинності рішенням", y = "в.п. на 1 в.п. облікової", caption = "Джерело: НБУ (UIRD, облікова ставка); розрахунки автора.") + theme_thesis()
    save_fig(p, "S22_P11_daily_event_study.png", 8.5, 4.8)
    # composition changes once events drop out (next decision inside the window): show the first 30 business days
    al <- melt(ESE[ser %in% c("UAH 3 міс.", "UAH 12 міс.") & k <= 30, .(ser, k, `Підвищення` = b_up, `Зниження` = b_dn)], id.vars = c("ser", "k"))
    if (nrow(al[is.finite(value)])) {
      p <- ggplot(al[is.finite(value)], aes(k, value, colour = variable)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + geom_vline(xintercept = 0, colour = INK2, linewidth = .3, linetype = "22") +
        geom_line(linewidth = .7) + facet_wrap(~ser) + scale_colour_manual(values = PAL) +
        labs(title = "Денна передача: підвищення vs зниження облікової ставки", x = "Робочі дні від набуття чинності", y = "в.п. на 1 в.п. облікової",
             caption = "Джерело: НБУ (UIRD, облікова ставка); розрахунки автора.") + theme_thesis()
      save_fig(p, "S22_P11b_daily_asymmetry.png", 8.5, 4.2)
    }
    for (sname in unique(ESE$ser)) { z <- ESE[ser == sname & k >= 0]; if (!nrow(z)) next
      hl <- { m <- max(z$b, na.rm = TRUE); if (m > 0) z$k[which(z$b >= .5 * m)[1]] else NA }
      fact(paste0("P11_", gsub("[^A-Za-z0-9]", "", sname)), sprintf("β(5 дн.) = %.2f, β(20) = %.2f, β(60) = %.2f; половина максимальної реакції за %s роб. дн.; подій %d",
           z[k == 5, b][1], z[k == 20, b][1], z[k == 60, b][1], hl, max(z$events))) }
  }
  status("P11 daily event study", "ok", sprintf("%d змін облікової, %.1f хв", nrow(ev), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
} else status("P11 daily event study", "skipped", "потрібні raw_key і UIRD.xls")

# ============================== P12. CD-RULE QUASI-EXPERIMENTS =================
# y_bt = α_b + δ_t + Σ_{k≠−1} β_k · 1[t − R = k] · E_b + ε,  E_b = pre-reform (6 m) share of NBU CDs in assets,
# standardised across banks. β_k: how much more CD-reliant banks changed y around reform R.
REF <- data.table(date = as.Date(c("2023-04-01", "2023-09-01", "2024-04-01", "2025-04-01", "2026-08-01")),
                  label = c("2023-04: запровадження 3М ДС", "2023-09: частка строкових у ліміті 70% → 35%", "2024-04: ліміт за 12-міс. приростом",
                            "2025-04: множник 3 → 3,5, спред +1 в.п.", "2026-08: процентні тендери"))
REF <- REF[date <= max(B$month)]
B[, cd_share := fcoalesce(col_or_na(B, "cd_nbu_total"), 0) / col_or_na(B, "fin_assets")]
ES_OUT <- intersect(c("hh_term", "q_dep_hh_term", "q_dep_term", "cod4"), OUTS$key)
kname <- function(k) paste0("ek_", ifelse(k < 0, paste0("m", -k), k))
es_did <- function(R, ov, pre = 6L, post = 9L) {
  post <- min(post, (year(max(B$month)) - year(R)) * 12L + month(max(B$month)) - month(R)); if (post < 2) return(NULL)
  D <- B[month >= add_months(R, -pre) & month <= add_months(R, post), .(nkb, month, y = get(ov), cdsh = cd_share)]
  E <- D[month < R, .(E = mean(cdsh, na.rm = TRUE)), by = nkb][is.finite(E)]
  if (nrow(E) < 15 || sd(E$E) == 0) return(NULL)
  E[, E := (E - mean(E)) / sd(E)]
  D <- merge(D, E, by = "nkb")[is.finite(y)]
  full <- D[, .N, by = nkb][N >= .8 * (pre + post + 1), nkb]; D <- D[nkb %in% full]
  D[, k := (year(month) - year(R)) * 12L + month(month) - month(R)]
  ks <- setdiff(-pre:post, -1L); for (kk in ks) D[, (kname(kk)) := as.numeric(k == kk) * E]
  f <- fe2_dk_v(D, "y", kname(ks), vcov = "bank"); if (is.null(f)) return(NULL)
  cf <- data.table(k = ks, b = f$b[kname(ks)], se = sqrt(diag(f$V)[kname(ks)]))
  pre_k <- intersect(kname(ks[ks < -1]), names(f$b)); post_k <- intersect(kname(ks[ks >= 0]), names(f$b))
  W <- tryCatch({ bp <- f$b[pre_k]; as.numeric(t(bp) %*% solve(f$V[pre_k, pre_k]) %*% bp) }, error = function(e) NA_real_)
  a <- rep(1 / length(post_k), length(post_k)); pa <- sum(a * f$b[post_k]); pse <- sqrt(as.numeric(t(a) %*% f$V[post_k, post_k] %*% a))
  list(coef = cf, summ = data.table(post_avg = pa, se = pse, p = 2 * pnorm(-abs(pa / pse)), pretrend_chi2 = W, pretrend_p = pchisq(W, length(pre_k), lower.tail = FALSE),
                                     banks = f$nb, obs = f$n))
}
ESR <- list(); ESS <- list()
for (r in seq_len(nrow(REF))) for (ok_ in ES_OUT) {
  z <- tryCatch(es_did(REF$date[r], paste0("o_", ok_)), error = function(e) NULL); if (is.null(z)) next
  ESR[[length(ESR) + 1]] <- z$coef[, `:=`(reform = REF$label[r], key = ok_)]
  ESS[[length(ESS) + 1]] <- z$summ[, `:=`(reform = REF$label[r], key = ok_)]
}
ESR <- rbindlist(ESR, fill = TRUE); ESS <- rbindlist(ESS, fill = TRUE)
if (nrow(ESR)) {
  ESR[, label := lab_out[key]]; ESS[, label := lab_out[key]]
  save_tab(ESR, "S22_P12a_cd_reforms_event_study.csv", "P12_ES_coef"); save_tab(ESS, "S22_P12b_cd_reforms_summary.csv", "P12_ES_summary")
  for (kk in intersect(c("hh_term", "q_dep_hh_term"), unique(ESR$key))) {
    z <- ESR[key == kk]; z <- rbind(z, unique(z[, .(reform, key, label)])[, `:=`(k = -1L, b = 0, se = 0)], fill = TRUE)
    p <- ggplot(z, aes(k, b)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + geom_vline(xintercept = -.5, colour = INK2, linewidth = .3, linetype = "22") +
      geom_ribbon(aes(ymin = b - 1.645 * se, ymax = b + 1.645 * se), fill = "grey80", alpha = .7) + geom_line(linewidth = .7, colour = INK) + geom_point(size = 1.2, colour = INK) +
      facet_wrap(~reform, ncol = 2, scales = "free_y") +
      labs(title = sprintf("Реформи 3М депсертифікатів: %s", lab_out[[kk]]),
           subtitle = "Подієве дослідження, FE банку і місяця, похибки кластеризовано за банком: банки з вищою (на 1 ст. відх.) часткою ДС в активах до реформи; база k = −1",
           x = "Місяці від реформи", y = if (kk == "hh_term") "в.п." else "%", caption = CAP) + theme_thesis()
    save_fig(p, sprintf("S22_P12_cd_reform_%s.png", kk), 8.5, 7)
  }
  for (i in seq_len(nrow(ESS))) { z <- ESS[i]; fact(sprintf("P12_%s_%s", substr(z$reform, 1, 7), z$key),
       sprintf("після реформи %+.2f (s.e. %.2f, p = %.3f); тест паралельних трендів p = %.2f; %d банків", z$post_avg, z$se, z$p, z$pretrend_p, z$banks)) }
  status("P12 CD reforms", "ok", sprintf("%d реформ × %d показників", uniqueN(ESS$reform), uniqueN(ESS$key)))
} else status("P12 CD reforms", "skipped", "недостатньо даних")

# ============================== EXPORT ========================================
save_tab(STATUS, "S22_00_blocks_status.csv", "Status"); save_tab(FACTS, "S22_00_key_facts.csv", "Key_facts")
write_workbook("Section_2_2.xlsx")
print(STATUS); print(FACTS)
log_msg("SECTION 2.2 COMPLETE: ", length(list.files(FIG, pattern = "^S22.*png$")), " figures, ", length(TABLES), " tables, ",
        sprintf("%.1f хв", as.numeric(difftime(Sys.time(), t0_all, units = "mins"))))
