# ============================================================================
# 14_ou_deposit_dynamics_v6.R — СТОХАСТИЧНА МОДЕЛЬ ДЕПОЗИТІВ: ПРОЦЕСИ ОРНШТЕЙНА–УЛЕНБЕКА З ОЧІКУВАННЯМИ І СТРИБКАМИ
# ----------------------------------------------------------------------------
# Місце в роботі: підрозділ 2.3 (новий пункт «Стохастична модель депозитів і стрес-тест IRRBB»);
#   O3 (обсяги) доповнює динаміку депозитів п. 2.1, O2 (ставки) — неперервний аналог ECM п. 2.2 (φ = 1 − e^(−κ/12)).
# Run AFTER 02, 07/09 (expectations in the mart) and 10 (S23_bank_items_latest.csv for bank weights).
# Output: output_nbu/ou_dynamics/{tables,figures}/OU_* + Section_OU.xlsx + inputs of 13_irrbb_stress_test.py.
#
# Ornstein–Uhlenbeck with moving mean and jumps (Vasicek 1977; Merton 1976; Barndorff-Nielsen & Shephard 2001):
#   dx_t = κ(μ(Z_t) − x_t)dt + σ dW_t + J dN_t,   N ~ Poisson(λ),  μ(Z) = a + b′Z  (Z: policy rate, expectations)
# Exact monthly discretisation (Δ = 1/12):  x_{t+1} = e^{−κΔ} x_t + (1 − e^{−κΔ}) μ(Z_t) + ε_{t+1}
#   ⇔ OLS  x_{t+1} = c + ρ x_t + g′Z_t + ε,  κ = −ln ρ / Δ,  b = g / (1 − ρ),  σ = sd(ε)·√(2κ / (1 − ρ²)),  half-life = ln2/κ.
# Jumps: residuals beyond k·MAD (robust) are jumps; λ = #jumps / years; diffusion re-estimated with jump dummies.
#
# Blocks
#  O1  short rate: policy (end of month) and UONIA as OU; mean depends on inflation expectations (Taylor-type) and on the
#      market-implied path (OVDP curve); jumps (02–06.2022)
#  O2  deposit and asset rates as OU around μ = a + β·i + γ·π^e + δ·E[Δs] (expectations of the matching agents:
#      households for HH products, firms for corporate, mean for assets); full sample vs without 2022; long-run β, γ, δ with
#      delta-method SE; asymmetric mean reversion κ⁺ ≠ κ⁻ (rockets & feathers); bank panel OU (bank FE, clustered SE)
#  O3  deposit volumes: monthly growth as OU-jump with drivers — ex-ante real deposit rate (r − π^e), expected
#      depreciation, banks' expectations of deposit demand (BLS); demand-deposit share as OU (term–demand spread, π^e)
#  O4  joint Monte Carlo (N paths × 36 months) of {π^e, E[Δs], i, rates, volumes} with correlated diffusion shocks,
#      Poisson jumps and parameter uncertainty → fan charts; distribution of Δi; cost of deposits; deposit drawdowns
#  O5  behavioural NMD from the OU model: stable share = 1 % quantile of the 12-month minimum of demand balances;
#      core = stable × (1 − β_LR); bank-level stable shares (bank OU-jump) → input of the IRRBB stress test
#  O6  NII-at-risk: per path the 12-month average change of every balance-sheet item rate → system and bank
#      distributions of ΔNII; OU-consistent rate scenarios for EVE (Δi quantiles × Vasicek decay (1 − e^{−κτ})/(κτ))
#  Treatment of limitations (v2, see OU_O7_limitations.csv):
#   L1 one jump in 2018–2026 → policy-rate history 2010–2026 (data_nbu/local_inputs/policy_rate_history.csv): jumps = monthly
#      moves ≥ 3 pp; frequency with Gamma–Poisson (Jeffreys) posterior drawn per path; sizes resampled × U(0.75, 1.25)
#   L2 small-bank stable shares → empirical-Bayes shrinkage of σ, ρ, λ to the size-quartile peer group; posterior-predictive
#      simulation; bootstrap 90 % intervals; banks with < 36 months take the peer parameters
#   L3 group expectations replaced by means → OU for each group (households, firms) anchored to the mean, simulated jointly
#   L4 policy rate is a step function → variant with decisions only in NBU meeting months on a 25 b.p. grid
#   L5 small-sample bias / sampling error of ρ → Kendall bias correction and residual block bootstrap CIs (κ, half-life)
#   L6 parameter instability (war) → rolling 48-month κ and sub-sample estimates
#   L7 fat tails → multivariate Student-t innovations (df from residual kurtosis)
#   L8 measurement error in survey expectations → 2SLS with lagged expectations as instruments
#   L9 constant balance sheet → dynamic NII with simulated deposit volumes (excess funds at the CD rate)
#   L10 one pass-through for all banks → bank-specific scaling of item rate changes by bank θ (2.2) relative to the median
#   L11 single-factor curve (Vasicek) → two-factor curve (PCA level/slope of the NBU curve) with OU factors for EVE scenarios
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_models_v6.R"), encoding = "UTF-8")
.need(c("sandwich", "MASS"))
init_output("OU", folder = "ou_dynamics")
CAP <- "Джерело: розрахунки автора за даними НБУ (OSB, процентні ставки, криві ОВДП, опитування очікувань, BLS)."
col_or_na <- function(dt, v) if (v %in% names(dt)) dt[[v]] else rep(NA_real_, nrow(dt))
blk <- function(name, expr) { t0 <- Sys.time()
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) { status(name, "error", conditionMessage(e)); message("  [", name, "] ", conditionMessage(e)); FALSE })
  if (ok) status(name, "ok", sprintf("%.1f с", as.numeric(difftime(Sys.time(), t0, units = "secs")))) }
t0_all <- Sys.time(); set.seed(20261105)
PAR <- list(dt = 1 / 12, N = 10000L, H = 36L, NX = 1000L, jump_k = 4, q_stable = 0.01, horizon_stable = 12L,
            war_from = as.Date("2022-02-01"), war_to = as.Date("2022-12-01"), rho_max = 0.995, bank_paths = 4000L,
            jump_pp = 0.03, jump_from = as.Date("2016-01-01"), k_shrink = 24, prior_years = 3, meet_months = c(1, 3, 4, 6, 7, 9, 10, 12), n_boot = 499L, block = 6L, roll = 48L, min_bank_n = 36L)
locf2 <- function(x) { y <- x; for (k in seq_along(y)[-1]) if (!is.finite(y[k]) && is.finite(y[k - 1])) y[k] <- y[k - 1]; y }

# ------------------------------ data --------------------------------------------
S <- store_read("mart_system_month_v6"); S[, month := as.Date(month)]; setorder(S, month)
B <- store_read("mart_bank_month_v6"); B[, `:=`(month = as.Date(month), nkb = as.character(nkb))]
wsys <- function(rc, wc, zero_ok = FALSE) {
  if (!all(c(rc, wc) %in% names(B))) return(data.table(month = unique(B$month), v = NA_real_))
  B[, .(v = { r <- get(rc); w <- get(wc); ok <- is.finite(r) & is.finite(w) & w > 0 & r < 0.6 & (r > 0 | (zero_ok & r >= 0))
              if (sum(ok) >= 3) sum(r[ok] * w[ok]) / sum(w[ok]) else NA_real_ }), by = month] }
SER <- list(  # name, rate column, weight column, zero allowed, expectations group, label
  hh_dem    = list("r_hh_demand_uah",   "dep_hh_demand_uah",   TRUE,  "hh",    "Кошти ФО на вимогу"),
  hh_term   = list("r_hh_term_uah",     "dep_hh_term_uah",     FALSE, "hh",    "Строкові депозити ФО"),
  corp_dem  = list("r_corp_demand_uah", "dep_corp_demand_uah", TRUE,  "firms", "Кошти СГ на вимогу"),
  corp_term = list("r_corp_term_uah",   "dep_corp_term_uah",   FALSE, "firms", "Строкові депозити СГ"),
  loan_corp = list("y_loans_corp_uah",  "loans_corp_all_uah",  FALSE, "mean",  "Кредити СГ (дохідність)"),
  loan_hh   = list("y_loans_hh_uah",    "loans_hh_all_uah",    FALSE, "mean",  "Кредити ФО (дохідність)"),
  cd        = list("y_cd_uah",          "cd_nbu_uah",          FALSE, "mean",  "Депсертифікати НБУ (дохідність)"),
  ovdp      = list("y_refin_uah",       "ovdp_uah",            FALSE, "mean",  "ОВДП (дохідність)"))
DEP <- c("hh_dem", "hh_term", "corp_dem", "corp_term")
M <- S[, .(month, i = policy_eom, i_avg = policy_avg, uonia = uonia_avg, cpi_mom,
           pie_hh = col_or_na(S, "pie_hh"), pie_firms = col_or_na(S, "pie_firms"), pie_mean = col_or_na(S, "pie_mean"),
           edep_hh = col_or_na(S, "edep_hh"), edep_firms = col_or_na(S, "edep_firms"), edep_mean = col_or_na(S, "edep_mean"),
           xpath = col_or_na(S, "exp_path_3m"), bls_dep = col_or_na(S, "bls_exp_dep") / 100,
           D_hh_dem = dep_hh_demand_uah, D_hh_term = dep_hh_term_uah, D_corp_dem = dep_corp_demand_uah, D_corp_term = dep_corp_term_uah)]
for (k in names(SER)) { z <- wsys(SER[[k]][[1]], SER[[k]][[2]], SER[[k]][[3]]); setnames(z, "v", paste0("r_", k)); M <- merge(M, z, by = "month", all.x = TRUE) }
M <- M[month >= as.Date("2018-01-01")]; setorder(M, month)
for (v in c("pie_hh", "pie_firms", "pie_mean", "edep_hh", "edep_firms", "edep_mean", "xpath", "bls_dep")) M[, (v) := locf2(get(v))]
M[, `:=`(pie_firms = fcoalesce(pie_firms, pie_mean), edep_firms = fcoalesce(edep_firms, edep_mean), xpath = fcoalesce(xpath, 0), bls_dep = fcoalesce(bls_dep, 0))]
M[, war := month >= PAR$war_from & month <= PAR$war_to]
# L1: long policy-rate history (end of month) for the jump calibration
POLF <- file.path(LOCAL_DIR, "policy_rate_history.csv")
POLH <- if (file.exists(POLF)) { z <- fread(POLF, encoding = "UTF-8"); z[, from := as.Date(from)]; setorder(z, from)
  mm <- seq(as.Date(format(min(z$from), "%Y-%m-01")), max(M$month), by = "month")
  data.table(month = mm, i_h = z$rate[findInterval(as.numeric(add_months(mm, 1) - 1), as.numeric(z$from))] / 100) } else M[, .(month, i_h = i)]
POLH[, d := c(NA, diff(i_h))]
POLH[, jump := is.finite(d) & abs(d) >= PAR$jump_pp - 1e-9]
JEV_all <- POLH[jump == TRUE, .(month, d)]
JEV <- JEV_all[month >= PAR$jump_from]                # base: inflation-targeting regime (since 2016); 2010–2015 — sensitivity
JM <- add_months(JEV_all$month, -1)                   # regression month t whose step to t+1 is a jump
for (k in DEP) M[, (paste0("g_", k)) := c(NA, diff(log(get(paste0("D_", k)))))]
M[, sh_dem := D_hh_dem / (D_hh_dem + D_hh_term)]
save_tab(M, "OU_00_data_monthly.csv", "Data")
EXPV <- function(grp) list(pie = switch(grp, hh = "pie_hh", firms = "pie_firms", "pie_mean"), edep = switch(grp, hh = "edep_hh", firms = "edep_firms", "edep_mean"))

# ------------------------------ OU estimator (exact discretisation, jumps, NW SE) --------------------------------
ou_fit <- function(D, y, X = character(0), jumps = TRUE, k = PAR$jump_k, sample = NULL, jm = NULL) {
  d <- copy(D); d[, `:=`(y1 = shift(get(y), type = "lead"), y0 = get(y))]
  if (!is.null(sample)) d <- d[sample]
  cols <- c("y1", "y0", X); d <- d[complete.cases(d[, ..cols])]
  if (nrow(d) < 24) stop("замало спостережень для ", y)
  fml <- as.formula(paste("y1 ~ y0", if (length(X)) paste("+", paste(X, collapse = " + ")) else ""))
  f0 <- lm(fml, d); e <- resid(f0); s_rob <- 1.4826 * median(abs(e - median(e)))
  J <- if (!is.null(jm)) which(d$month %in% jm) else if (jumps) which(abs(e - median(e)) > k * s_rob) else integer(0)
  if (length(J)) { for (j in J) d[[paste0("J", j)]] <- as.numeric(seq_len(nrow(d)) == j)
    fml <- update(fml, as.formula(paste(". ~ . +", paste(paste0("J", J), collapse = " + ")))) }
  f <- lm(fml, d); V <- tryCatch(sandwich::NeweyWest(f, lag = 6, prewhite = FALSE), error = function(e) vcov(f))
  b <- coef(f); rho <- min(b[["y0"]], PAR$rho_max); kap <- if (rho > 0) -log(rho) / PAR$dt else NA_real_   # ρ ≤ 0: no OU time scale (oscillation)
  eps <- resid(f); if (length(J)) eps <- eps[-J]
  sig <- if (is.finite(kap)) sd(eps) * sqrt(2 * kap / max(1 - rho^2, 1e-6)) else sd(eps) * sqrt(12)
  lr <- function(term) { if (!term %in% names(b)) return(c(NA, NA)); g <- c(1 / (1 - b[["y0"]]), b[[term]] / (1 - b[["y0"]])^2)
    vv <- V[c(term, "y0"), c(term, "y0")]; c(b[[term]] / (1 - b[["y0"]]), sqrt(as.numeric(t(g) %*% vv %*% g))) }
  LR <- rbindlist(lapply(c("(Intercept)", X), function(t) { z <- lr(t); data.table(term = t, lr = z[1], lr_se = z[2]) }))
  jr <- if (length(J)) e[J] else numeric(0)
  Tn <- nrow(d); rho_bc <- min(b[["y0"]] + (1 + 3 * b[["y0"]]) / Tn, PAR$rho_max)                  # Kendall (1954) small-sample bias correction
  kap_bc <- if (rho_bc > 0) -log(rho_bc) / PAR$dt else NA_real_
  ek <- mean((eps - mean(eps))^4) / var(eps)^2 - 3; df_t <- if (is.finite(ek) && ek > 0.05) min(30, max(4, 4 + 6 / ek)) else 30
  list(dat = d, Jidx = J, rho_bc = rho_bc, kappa_bc = kap_bc, hl_bc_m = log(2) / kap_bc * 12, df_t = df_t, kurt = ek, fit = f, V = V, b = b[c("(Intercept)", "y0", X)], Vb = V[c("(Intercept)", "y0", X), c("(Intercept)", "y0", X)], rho = rho, kappa = kap,
       hl_m = log(2) / kap * 12, sigma = sig, sd_e = sd(eps), LR = LR, n = nrow(d), r2 = summary(f)$r.squared, jumps = jr,
       jump_months = d$month[J], lam = length(J) / (nrow(d) / 12), months = d$month, resid = setNames(resid(f), as.character(d$month)), X = X, y = y)
}
ou_boot <- function(o, B = PAR$n_boot, bl = PAR$block) {             # recursive residual block bootstrap, X held fixed
  d <- o$dat; X <- o$X; n <- nrow(d); b <- o$b; e <- resid(o$fit); if (length(o$Jidx)) e <- e[-o$Jidx]; e <- e - mean(e)
  Xm <- if (length(X)) as.matrix(d[, ..X]) else matrix(0, n, 0)
  ks <- replicate(B, { idx <- unlist(lapply(sample.int(max(1, length(e) - bl + 1), ceiling(n / bl), TRUE), function(s0) s0:(s0 + bl - 1)))[1:n]
    es <- e[pmin(idx, length(e))]; ys <- numeric(n + 1); ys[1] <- d$y0[1]
    for (t in 1:n) ys[t + 1] <- b[1] + b[2] * ys[t] + (if (length(X)) sum(b[-(1:2)] * Xm[t, ]) else 0) + es[t]
    ff <- if (length(X)) lm.fit(cbind(1, ys[1:n], Xm), ys[2:(n + 1)]) else lm.fit(cbind(1, ys[1:n]), ys[2:(n + 1)])
    r <- ff$coefficients[2]; if (is.finite(r) && r > 0 && r < 1) -log(r) * 12 else NA_real_ })
  ks <- ks[is.finite(ks)]; c(kappa_boot_mean = mean(ks), kappa_lo = unname(quantile(ks, .05)), kappa_hi = unname(quantile(ks, .95)), hl_lo = log(2) / unname(quantile(ks, .95)) * 12, hl_hi = log(2) / unname(quantile(ks, .05)) * 12, valid = length(ks) / B) }
ou_row <- function(o, label, spec) {
  lr <- setNames(o$LR$lr, o$LR$term); lse <- setNames(o$LR$lr_se, o$LR$term)
  data.table(series = label, spec = spec, n = o$n, rho = o$rho, kappa = o$kappa, half_life_m = o$hl_m, sigma = o$sigma, r2 = o$r2,
             beta_i = lr["i"] %||% NA, beta_i_se = lse["i"] %||% NA,
             gamma_pie = lr[grep("^pie", names(lr))][1], gamma_pie_se = lse[grep("^pie", names(lse))][1],
             delta_edep = lr[grep("^edep", names(lr))][1], delta_edep_se = lse[grep("^edep", names(lse))][1],
             phi_xpath = lr["xpath"] %||% NA, jumps = length(o$jumps), lambda_yr = o$lam, mean_abs_jump = if (length(o$jumps)) mean(abs(o$jumps)) else NA_real_,
             jump_months = paste(format(add_months(o$jump_months, 1), "%m.%Y"), collapse = ", "), kappa_bc = o$kappa_bc, half_life_bc_m = o$hl_bc_m, df_t = o$df_t)
}
OU <- list()

# ------------------------------ O1 short rate ----------------------------------------------------------------
blk("O1 коротка ставка", {
  OU$i0 <- ou_fit(M, "i", character(0), jm = JM); OU$i1 <- ou_fit(M, "i", "pie_mean", jm = JM); OU$i2 <- ou_fit(M, "i", c("pie_mean", "xpath"), jm = JM)
  OU$u1 <- ou_fit(M, "uonia", "pie_mean", jm = JM)
  T1 <- rbind(ou_row(OU$i0, "Облікова ставка (кінець місяця)", "сталий μ"), ou_row(OU$i1, "Облікова ставка (кінець місяця)", "μ = a + b·π^e"),
              ou_row(OU$i2, "Облікова ставка (кінець місяця)", "μ = a + b·π^e + шлях ринку (крива ОВДП)"), ou_row(OU$u1, "UONIA", "μ = a + b·π^e"), fill = TRUE)
  save_tab(T1, "OU_O1_short_rate.csv", "O1_short_rate")
  # L1: jump calibration on 2010–2026
  jsum <- function(J, Ty, lab) data.table(sample = lab, years = Ty, jumps = nrow(J), jumps_up = sum(J$d > 0), jumps_down = sum(J$d < 0), lambda_mle = nrow(J) / Ty,
                   lambda_post_mean = (0.5 + nrow(J)) / Ty, lambda_post_q05 = qgamma(.05, 0.5 + nrow(J), Ty), lambda_post_q95 = qgamma(.95, 0.5 + nrow(J), Ty),
                   mean_abs_pp = 100 * mean(abs(J$d)), max_up_pp = 100 * max(c(0, J$d)), max_down_pp = 100 * min(c(0, J$d)))
  Ty <- nrow(POLH[is.finite(d) & month >= PAR$jump_from]) / 12; nJ <- nrow(JEV)
  JT <- data.table(month = JEV_all$month, change_pp = 100 * JEV_all$d, regime = fifelse(JEV_all$month >= PAR$jump_from, "інфляційне таргетування (база)", "2010–2015 (чутливість)"))
  JS <- rbind(jsum(JEV, Ty, "з 2016 р. (база)"), jsum(JEV_all, nrow(POLH[is.finite(d)]) / 12, "2010–2026 (чутливість)"))
  save_tab(JT, "OU_O1b_policy_jump_events.csv", "O1b_jump_events"); save_tab(JS, "OU_O1b_policy_jump_summary.csv", "O1b_jump_summary")
  JUMP <- list(sizes = JEV$d, a = 0.5 + nJ, b = Ty)
  JS <- JS[1]
  p <- ggplot(POLH, aes(month, 100 * i_h)) + geom_step(linewidth = .6) + geom_point(data = POLH[jump == TRUE], colour = "#c4452d", size = 2) +
    geom_vline(xintercept = PAR$jump_from, linetype = "22", linewidth = .3) +
    labs(title = "Облікова ставка НБУ 2010–2026 і стрибки (зміна за місяць ≥ 3 в.п.)", subtitle = sprintf("Стрибків: %d за %.1f року; апостеріорна частота %.2f на рік (90 %%: %.2f–%.2f)", nJ, Ty, JS$lambda_post_mean, JS$lambda_post_q05, JS$lambda_post_q95),
         x = NULL, y = "%", caption = CAP) + theme_thesis()
  save_fig(p, "OU_F12_policy_history_jumps.png", 8.5, 4.6)
  fact("O1_policy", sprintf("облікова: κ = %.2f (з поправкою на зміщення %.2f; напіврозпад %.1f міс.), b(π^e) = %.2f (%.2f), σ = %.1f в.п.; стрибки з 2016 р.: %d, частота %.2f на рік",
       OU$i1$kappa, OU$i1$kappa_bc, OU$i1$hl_m, T1$gamma_pie[2], T1$gamma_pie_se[2], 100 * OU$i1$sigma, nJ, JS$lambda_post_mean))
  mu <- M[, .(month, i, mu = (OU$i1$b[1] + OU$i1$b[3] * pie_mean) / (1 - OU$i1$b[2]))]
  p <- ggplot(melt(mu[, .(month, `Облікова ставка` = 100 * i, `Рухоме середнє OU μ(π^e)` = 100 * mu)], id.vars = "month"), aes(month, value, colour = variable, linetype = variable)) +
    geom_line(linewidth = .7) + geom_vline(xintercept = add_months(OU$i1$jump_months, 1), linetype = "22", colour = "grey55", linewidth = .3) +
    labs(title = "Облікова ставка як процес Орнштейна–Уленбека з рухомим середнім", subtitle = "μ_t = a + b·π^e_t; пунктир — місяці стрибків", x = NULL, y = "%", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "OU_F1_policy_mean.png", 8.5, 4.6)
  # L6: rolling 48-month κ
  roll <- function(y, X, jm = NULL) rbindlist(lapply(seq(PAR$roll, nrow(M) - 1, by = 3), function(e) { D <- M[(e - PAR$roll + 1):e]
    o <- tryCatch(ou_fit(D, y, X, jm = jm), error = function(err) NULL); if (is.null(o)) return(NULL); data.table(month = D$month[nrow(D)], kappa = o$kappa, hl = o$hl_m) }))
  RL <- rbind(roll("i", "pie_mean", JM)[, series := "Облікова ставка"], roll("r_hh_term", c("i", "pie_hh", "edep_hh"))[, series := "Строкові депозити ФО"],
              roll("r_corp_term", c("i", "pie_firms", "edep_firms"))[, series := "Строкові депозити СГ"], fill = TRUE)
  save_tab(RL, "OU_O1d_rolling_kappa.csv", "O1d_rolling")
  p <- ggplot(RL[is.finite(hl) & hl < 60], aes(month, hl, colour = series)) + geom_line(linewidth = .7) +
    labs(title = "Стабільність параметрів: напіврозпад у ковзному вікні 48 міс.", subtitle = "Оцінка OU з очікуваннями на кожному вікні; дата — кінець вікна", x = NULL, y = "міс.", colour = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "OU_F11_rolling_half_life.png", 8.5, 4.6)
})

# ------------------------------ O2 rates with expectations ---------------------------------------------------
blk("O2 ставки з очікуваннями", {
  out <- list(); ASY <- list()
  for (k in names(SER)) { y <- paste0("r_", k); if (sum(is.finite(M[[y]])) < 30) next
    e <- EXPV(SER[[k]][[4]]); lab <- SER[[k]][[5]]
    oA <- ou_fit(M, y, "i"); oB <- ou_fit(M, y, c("i", e$pie, e$edep)); oC <- ou_fit(M, y, c("i", e$pie, e$edep), sample = !M$war)
    OU[[paste0("r_", k)]] <- oB
    out[[length(out) + 1]] <- rbind(ou_row(oA, lab, "μ = a + β·i"), ou_row(oB, lab, "μ = a + β·i + γ·π^e + δ·E[Δs]"), ou_row(oC, lab, "те саме, без 02–12.2022"), fill = TRUE)
    # asymmetric reversion: Δr = a + κ⁺Δ·gap⁺ + κ⁻Δ·gap⁻, target from model B
    d <- copy(M); bb <- oB$b; d[, tgt := (bb[1] + bb[3] * i + bb[4] * get(e$pie) + bb[5] * get(e$edep)) / (1 - bb[2])]
    d[, `:=`(dy = shift(get(y), type = "lead") - get(y), gp = pmax(tgt - get(y), 0), gn = pmin(tgt - get(y), 0))]
    d <- d[is.finite(dy) & is.finite(gp)]
    fa <- lm(dy ~ gp + gn, d); Va <- tryCatch(sandwich::NeweyWest(fa, lag = 6, prewhite = FALSE), error = function(e) vcov(fa))
    kp <- -log(1 - min(coef(fa)[["gp"]], .99)) * 12; kn <- -log(1 - min(coef(fa)[["gn"]], .99)) * 12
    w <- (coef(fa)[["gp"]] - coef(fa)[["gn"]]) / sqrt(Va["gp", "gp"] + Va["gn", "gn"] - 2 * Va["gp", "gn"])
    ASY[[length(ASY) + 1]] <- data.table(series = lab, adj_up = coef(fa)[["gp"]], adj_up_se = sqrt(Va["gp", "gp"]), adj_dn = coef(fa)[["gn"]], adj_dn_se = sqrt(Va["gn", "gn"]),
                                         kappa_up = kp, kappa_dn = kn, hl_up_m = log(2) / kp * 12, hl_dn_m = log(2) / kn * 12, p_equal = 2 * pnorm(-abs(w)), n = nrow(d))
    if (k == "hh_term") {
      p <- ggplot(melt(d[, .(month, `Ставка` = 100 * get(y), `Рухома ціль OU μ(i, π^e, E[Δs])` = 100 * tgt, `Облікова` = 100 * i)], id.vars = "month"), aes(month, value, colour = variable, linetype = variable)) +
        geom_line(linewidth = .7) + labs(title = "Строкові депозити ФО: ставка повертається до цілі, що залежить від облікової та очікувань",
                                         subtitle = sprintf("κ = %.2f на рік, напіврозпад %.1f міс.", oB$kappa, oB$hl_m), x = NULL, y = "%", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
      save_fig(p, "OU_F2_hh_term_target.png", 8.5, 4.6)
    }
  }
  T2 <- rbindlist(out, fill = TRUE); save_tab(T2, "OU_O2_rates_ou.csv", "O2_rates")
  T3 <- rbindlist(ASY); save_tab(T3, "OU_O2b_asymmetric_reversion.csv", "O2b_asym")
  hl <- T2[spec == "μ = a + β·i + γ·π^e + δ·E[Δs]"]
  p <- ggplot(hl, aes(reorder(series, half_life_m), half_life_m)) + geom_col(fill = "#1f5fa8", width = .6) + coord_flip() +
    geom_text(aes(label = sprintf("beta == %.2f", beta_i)), parse = TRUE, hjust = -0.1, size = 3) +
    labs(title = "Швидкість повернення ставок до цілі (процес OU з очікуваннями)", subtitle = "Напіврозпад відхилення від цілі, міс.; підпис — довгострокова передача облікової β",
         x = NULL, y = "міс.", caption = CAP) + theme_thesis() + expand_limits(y = max(hl$half_life_m, na.rm = TRUE) * 1.3)
  save_fig(p, "OU_F3_half_lives.png", 8.5, 4.6)
  z <- hl[series == SER$hh_dem[[5]]]
  z <- hl[series == SER$hh_term[[5]]]
  if (nrow(z)) fact("O2_hh_term", sprintf("строкові депозити ФО: κ = %.2f (напіврозпад %.1f міс.), β = %.2f (%.2f), γ(π^e) = %.2f (%.2f), δ(E[Δs]) = %.2f (%.2f)", z$kappa, z$half_life_m, z$beta_i, z$beta_i_se, z$gamma_pie, z$gamma_pie_se, z$delta_edep, z$delta_edep_se))
  # bank panel OU (bank FE, clustered SE) for deposit products
  PAN <- list()
  for (k in DEP) { rc <- SER[[k]][[1]]; if (!rc %in% names(B)) next
    e <- EXPV(SER[[k]][[4]])
    d <- merge(B[, .(nkb, month, y = get(rc))], M[, c("month", "i", e$pie, e$edep), with = FALSE], by = "month")
    setnames(d, c(e$pie, e$edep), c("pie", "edep")); setorder(d, nkb, month)
    d[, y1 := shift(y, type = "lead"), by = nkb]; d <- d[is.finite(y) & is.finite(y1) & is.finite(pie) & is.finite(edep) & y < 0.6 & y1 < 0.6]
    d <- d[, if (.N >= 24) .SD, by = nkb]
    cols <- c("y1", "y", "i", "pie", "edep"); Z <- as.matrix(d[, ..cols]); g <- d$nkb
    Z <- Z - (rowsum(Z, g) / as.vector(table(g)[sort(unique(g))]))[g, ]
    X <- Z[, -1]; yv <- Z[, 1]; Bi <- solve(crossprod(X)); bh <- Bi %*% crossprod(X, yv); u <- as.vector(yv - X %*% bh)
    hb <- rowsum(X * u, g); G <- nrow(hb); Vc <- Bi %*% (crossprod(hb) * G / (G - 1)) %*% Bi
    rho <- bh[1]; lr <- function(j) { gg <- c(1 / (1 - rho), bh[j] / (1 - rho)^2); idx <- c(j, 1); c(bh[j] / (1 - rho), sqrt(as.numeric(t(gg) %*% Vc[idx, idx] %*% gg))) }
    PAN[[length(PAN) + 1]] <- data.table(series = SER[[k]][[5]], banks = G, n = nrow(d), rho = rho, kappa = -log(rho) * 12, half_life_m = log(2) / (-log(rho)),
                                         beta_i = lr(2)[1], beta_i_se = lr(2)[2], gamma_pie = lr(3)[1], gamma_pie_se = lr(3)[2], delta_edep = lr(4)[1], delta_edep_se = lr(4)[2])
  }
  save_tab(rbindlist(PAN), "OU_O2c_bank_panel_ou.csv", "O2c_panel")
  # L8: 2SLS — survey expectations measured with error; instruments: own lag and the other agents' lagged expectations
  IVR <- list()
  for (k in c("hh_term", "corp_term", "corp_dem", "loan_corp")) { y <- paste0("r_", k); e <- EXPV(SER[[k]][[4]])
    d <- copy(M); d[, `:=`(y1 = shift(get(y), type = "lead"), y0 = get(y), pe = get(e$pie), ed = get(e$edep),
                         z1 = shift(get(e$pie)), z2 = shift(get(e$edep)), z3 = shift(pie_mean, 2), z4 = shift(edep_mean, 2))]
    d <- d[complete.cases(d[, .(y1, y0, i, pe, ed, z1, z2, z3, z4)])]
    Xe <- cbind(1, d$y0, d$i, d$pe, d$ed); Zm <- cbind(1, d$y0, d$i, d$z1, d$z2, d$z3, d$z4)
    Xh <- Zm %*% solve(crossprod(Zm), crossprod(Zm, Xe)); bh <- solve(crossprod(Xh), crossprod(Xh, d$y1)); u <- as.vector(d$y1 - Xe %*% bh)
    V <- solve(crossprod(Xh)) %*% crossprod(Xh * u) %*% solve(crossprod(Xh))                                   # HC0
    fs <- summary(lm(d$pe ~ Zm[, -1]))$fstatistic[1]
    rho <- bh[2]; lrv <- function(j) { g <- c(1 / (1 - rho), bh[j] / (1 - rho)^2); idx <- c(j, 2); c(bh[j] / (1 - rho), sqrt(as.numeric(t(g) %*% V[idx, idx] %*% g))) }
    IVR[[k]] <- data.table(series = SER[[k]][[5]], n = nrow(d), rho = rho, beta_i = lrv(3)[1], beta_i_se = lrv(3)[2], gamma_pie = lrv(4)[1], gamma_pie_se = lrv(4)[2],
                           delta_edep = lrv(5)[1], delta_edep_se = lrv(5)[2], first_stage_F_pie = fs) }
  save_tab(rbindlist(IVR), "OU_O2d_iv_expectations.csv", "O2d_iv")
  # L3: group expectations as OU anchored to the cross-agent mean (simulated jointly in O4)
  for (g in c("pie_hh", "pie_firms")) OU[[g]] <- ou_fit(M, g, "pie_mean", jumps = FALSE)
  for (g in c("edep_hh", "edep_firms")) OU[[g]] <- ou_fit(M, g, "edep_mean")
  save_tab(rbindlist(lapply(c("pie_hh", "pie_firms", "edep_hh", "edep_firms"), function(g) ou_row(OU[[g]], g, "μ = a + b·середнє агентів"))), "OU_O2e_group_expectations.csv", "O2e_groups")
  # L5: bootstrap CIs of κ for the key equations
  BT <- rbindlist(lapply(c("r_hh_term", "r_corp_term", "r_corp_dem", "r_loan_corp", "r_cd"), function(nm) { o <- OU[[nm]]; if (is.null(o)) return(NULL)
    z <- ou_boot(o); data.table(eq = nm, series = SER[[sub("^r_", "", nm)]][[5]], kappa = o$kappa, kappa_bc = o$kappa_bc, half_life_m = o$hl_m, half_life_bc_m = o$hl_bc_m, t(z)) }))
  BT <- rbind(data.table(eq = "i1", series = "Облікова ставка", kappa = OU$i1$kappa, kappa_bc = OU$i1$kappa_bc, half_life_m = OU$i1$hl_m, half_life_bc_m = OU$i1$hl_bc_m, t(ou_boot(OU$i1))), BT, fill = TRUE)
  BT[, `:=`(kappa_bbc = fifelse(2 * kappa - kappa_boot_mean >= kappa_lo & 2 * kappa - kappa_boot_mean <= kappa_hi, 2 * kappa - kappa_boot_mean, NA_real_))]   # NA: skewed bootstrap
  BT[, half_life_bbc_m := log(2) / kappa_bbc * 12]
  setnames(BT, c("kappa_bc", "half_life_bc_m"), c("kappa_kendall", "half_life_kendall_m"))
  save_tab(BT, "OU_O1c_bootstrap_kappa.csv", "O1c_bootstrap")
  z <- rbindlist(PAN)[series == SER$hh_dem[[5]]]
  if (nrow(z)) fact("O2_hh_dem_panel", sprintf("кошти ФО на вимогу (панель банків): напіврозпад %.1f міс., β = %.2f (%.2f), γ(π^e) = %.2f (%.2f), δ(E[Δs]) = %.2f (%.2f)", z$half_life_m, z$beta_i, z$beta_i_se, z$gamma_pie, z$gamma_pie_se, z$delta_edep, z$delta_edep_se))
})

# ------------------------------ O3 deposit volumes -----------------------------------------------------------
blk("O3 обсяги депозитів", {
  out <- list()
  for (k in DEP) { y <- paste0("g_", k); r <- paste0("r_", k); e <- EXPV(SER[[k]][[4]])
    M[, `:=`(rr_real = get(r) - get(e$pie), ed = get(e$edep))]
    o <- ou_fit(M, y, c("rr_real", "ed", "bls_dep")); OU[[y]] <- o
    out[[length(out) + 1]] <- ou_row(o, SER[[k]][[5]], "Δlog D: μ = a + b₁(r − π^e) + b₂E[Δs] + b₃·BLS")
    lr <- setNames(o$LR$lr, o$LR$term)
    out[[length(out)]][, `:=`(b_real_rate = lr["rr_real"], b_real_rate_se = o$LR$lr_se[o$LR$term == "rr_real"], b_edep = lr["ed"], b_edep_se = o$LR$lr_se[o$LR$term == "ed"],
                               b_bls = lr["bls_dep"], b_bls_se = o$LR$lr_se[o$LR$term == "bls_dep"], mean_jump = if (length(o$jumps)) mean(o$jumps) else NA_real_)]
  }
  M[, `:=`(spr_td = r_hh_term - r_hh_dem)]
  osh <- ou_fit(M, "sh_dem", c("spr_td", "pie_hh", "edep_hh")); OU$sh <- osh
  T4 <- rbindlist(out, fill = TRUE); T4 <- rbind(T4, ou_row(osh, "Частка коштів ФО на вимогу", "μ = a + b₁(r_строк − r_вимога) + b₂π^e + b₃E[Δs]"), fill = TRUE)
  save_tab(T4, "OU_O3_volumes_ou_jump.csv", "O3_volumes")
  o <- OU$g_hh_dem
  d <- data.table(month = o$months, resid = o$resid)
  d[, jump := month %in% o$jump_months]
  p <- ggplot(M[is.finite(g_hh_dem)], aes(month, 100 * g_hh_dem)) + geom_col(fill = "grey70", width = 25) +
    geom_col(data = M[month %in% add_months(o$jump_months, 1)], fill = "#c4452d", width = 25) +
    labs(title = "Місячний приріст коштів ФО на вимогу: дифузія і стрибки", subtitle = sprintf("Червоним — стрибки (|залишок| > %d MAD); λ = %.2f на рік", PAR$jump_k, o$lam),
         x = NULL, y = "%, м/м", caption = CAP) + theme_thesis()
  save_fig(p, "OU_F4_jumps_hh_demand.png", 8.5, 4.4)
})

# ------------------------------ O4 joint Monte Carlo ---------------------------------------------------------
blk("O4 Монте-Карло", {
  OU$pie <- ou_fit(M, "pie_mean", character(0), jumps = FALSE); OU$edep <- ou_fit(M, "edep_mean", character(0))
  RN <- intersect(paste0("r_", names(SER)), names(OU)); GN <- intersect(paste0("g_", DEP), names(OU))
  GX <- c("pie_hh", "pie_firms", "edep_hh", "edep_firms")
  EQ <- c("pie", "edep", GX, "i1", RN, GN)
  pan <- TABLES[["O2c_panel"]]; last0 <- M[.N]
  for (k in DEP) { nm <- paste0("r_", k); o <- OU[[nm]]
    if (is.null(o) || o$rho < 0.99 || is.null(pan) || !SER[[k]][[5]] %in% pan$series) next
    z <- pan[series == SER[[k]][[5]]]; rho <- z$rho; g <- c(z$beta_i, z$gamma_pie, z$delta_edep) * (1 - rho)
    xl <- c(last0$i, last0[[o$X[2]]], last0[[o$X[3]]]); c0 <- last0[[nm]] * (1 - rho) - sum(g * xl)
    o$b <- setNames(c(c0, rho, g), c("(Intercept)", "y0", o$X)); o$rho <- rho
    o$Vb <- diag(c(1e-10, 1e-6, (c(z$beta_i_se, z$gamma_pie_se, z$delta_edep_se) * (1 - rho))^2)); dimnames(o$Vb) <- list(names(o$b), names(o$b))
    o$sim_note <- "панельна динаміка"; OU[[nm]] <- o }
  getm <- function(nm) if (nm == "i1") OU$i1 else OU[[nm]]
  R <- Reduce(function(a, b) merge(a, b, by = "month"), lapply(EQ, function(nm) { o <- getm(nm); data.table(month = as.Date(names(o$resid)), v = o$resid)[, setnames(.SD, "v", nm)] }))
  for (nm in EQ) { o <- getm(nm); R[month %in% (o$jump_months), (nm) := 0] }
  Sg <- cov(as.matrix(R[, ..EQ])); L <- t(chol(Sg + diag(1e-10, length(EQ))))
  save_tab(data.table(eq = EQ, round(cor(as.matrix(R[, ..EQ])), 3)), "OU_O4_residual_correlation.csv", "O4_corr")
  DF <- median(sapply(EQ, function(nm) getm(nm)$df_t))                     # L7: common Student-t degrees of freedom
  last <- M[.N]; N <- PAR$N; H <- PAR$H
  draw <- function(o) { b <- MASS::mvrnorm(N, o$b, (o$Vb + t(o$Vb)) / 2); b[, 2] <- pmin(b[, 2], PAR$rho_max); b }
  CO <- lapply(setNames(EQ, EQ), function(nm) draw(getm(nm)))
  jump_draw <- function(o, n) { if (!length(o$jumps)) return(rep(0, n)); (runif(n) < o$lam / 12) * sample(o$jumps, n, replace = TRUE) }
  LAMI <- rgamma(N, JUMP$a, JUMP$b)                                          # L1: jump frequency per path (posterior)
  pol_jump <- function(n, lam) (runif(n) < lam / 12) * sample(JUMP$sizes, n, TRUE) * runif(n, .75, 1.25)
  keep <- EQ; PATH <- lapply(setNames(keep, keep), function(z) matrix(NA_real_, N, H + 1))
  PATH$pie[, 1] <- last$pie_mean; PATH$edep[, 1] <- last$edep_mean; for (g in GX) PATH[[g]][, 1] <- last[[g]]
  PATH$i1[, 1] <- last$i; for (nm in RN) PATH[[nm]][, 1] <- last[[nm]]; for (nm in GN) PATH[[nm]][, 1] <- fcoalesce(M[[nm]][nrow(M)], 0)
  Dlev <- lapply(setNames(GN, GN), function(z) matrix(1, N, H + 1))
  ISTEP <- matrix(last$i, N, H + 1); INJ <- matrix(last$i, N, H + 1); sdi <- sqrt(Sg["i1", "i1"])
  mo0 <- as.integer(format(last$month, "%m"))
  for (h in 1:H) {
    w <- sqrt((DF - 2) / rchisq(N, DF))                                       # multivariate t scale per path-month
    Z <- (matrix(rnorm(N * length(EQ)), N) %*% t(L)) * w; colnames(Z) <- EQ
    cur <- lapply(keep, function(nm) PATH[[nm]][, h]); names(cur) <- keep
    nx <- list()
    nx$pie <- CO$pie[, 1] + CO$pie[, 2] * cur$pie + Z[, "pie"]
    nx$edep <- CO$edep[, 1] + CO$edep[, 2] * cur$edep + Z[, "edep"] + jump_draw(OU$edep, N)
    for (g in GX) { anc <- if (grepl("^pie", g)) cur$pie else cur$edep
      nx[[g]] <- CO[[g]][, 1] + CO[[g]][, 2] * cur[[g]] + CO[[g]][, 3] * anc + Z[, g] + (if (grepl("^edep", g)) jump_draw(OU[[g]], N) else 0) }
    nx$i1 <- pmax(CO$i1[, 1] + CO$i1[, 2] * cur$i1 + CO$i1[, 3] * cur$pie + Z[, "i1"] + pol_jump(N, LAMI), 0.005)
    INJ[, h + 1] <- pmax(CO$i1[, 1] + CO$i1[, 2] * INJ[, h] + CO$i1[, 3] * cur$pie + Z[, "i1"], 0.005)
    meet <- ((mo0 + h - 1) %% 12 + 1) %in% PAR$meet_months                   # L4: decisions only in meeting months, 25 b.p. grid
    ISTEP[, h + 1] <- if (meet) round(nx$i1 / 0.0025) * 0.0025 else ISTEP[, h]
    for (nm in RN) { o <- OU[[nm]]; b <- CO[[nm]]; ex <- o$X
      xv <- sapply(ex, function(v) if (v == "i") cur$i1 else if (v == "pie_mean") cur$pie else if (v == "edep_mean") cur$edep else cur[[v]])
      nx[[nm]] <- pmax(b[, 1] + b[, 2] * cur[[nm]] + rowSums(b[, 3:(2 + length(ex)), drop = FALSE] * xv) + Z[, nm], 0) }
    for (nm in GN) { o <- OU[[nm]]; b <- CO[[nm]]; k <- sub("^g_", "", nm); e <- EXPV(SER[[k]][[4]])
      rreal <- cur[[paste0("r_", k)]] - cur[[e$pie]]
      nx[[nm]] <- b[, 1] + b[, 2] * cur[[nm]] + b[, 3] * rreal + b[, 4] * cur[[e$edep]] + b[, 5] * last$bls_dep + Z[, nm] + jump_draw(o, N)
      Dlev[[nm]][, h + 1] <- Dlev[[nm]][, h] * exp(nx[[nm]]) }
    for (nm in keep) PATH[[nm]][, h + 1] <- nx[[nm]]
  }
  SIM <- list(PATH = PATH, D = Dlev, last = last, RN = RN, GN = GN, ISTEP = ISTEP, DF = DF)
  qs <- c(.01, .05, .25, .5, .75, .95, .99)
  fan <- function(mat, name, scale = 100) rbindlist(lapply(0:H, function(h) data.table(series = name, h = h, month = add_months(last$month, h),
                                                   q = paste0("q", 100 * qs), v = scale * quantile(mat[, h + 1], qs, na.rm = TRUE))))
  FAN <- rbindlist(c(list(fan(PATH$i1, "Облікова ставка, %"), fan(PATH$pie, "Інфляційні очікування, %"), fan(PATH$pie_hh, "Інфляційні очікування ФО, %"), fan(PATH$edep_hh, "Очікувана девальвація (ФО), %")),
                     lapply(RN, function(nm) fan(PATH[[nm]], paste0(SER[[sub("^r_", "", nm)]][[5]], ", %"))),
                     lapply(GN, function(nm) fan(Dlev[[nm]], paste0(SER[[sub("^g_", "", nm)]][[5]], ": обсяг, поточний = 100")))))
  FW <- dcast(FAN, series + h + month ~ q, value.var = "v"); save_tab(FW, "OU_O4_fan_quantiles.csv", "O4_fan")
  hist <- rbind(M[, .(series = "Облікова ставка, %", month, v = 100 * i)], M[, .(series = "Інфляційні очікування, %", month, v = 100 * pie_mean)],
                M[, .(series = "Інфляційні очікування ФО, %", month, v = 100 * pie_hh)], M[, .(series = "Очікувана девальвація (ФО), %", month, v = 100 * edep_hh)],
                rbindlist(lapply(RN, function(nm) M[, .(series = paste0(SER[[sub("^r_", "", nm)]][[5]], ", %"), month, v = 100 * get(nm))])))
  fanplot <- function(sers, file, title, ncol = 2) {
    f <- FW[series %in% sers]; hh <- hist[series %in% sers & month >= add_months(last$month, -48)]
    p <- ggplot(f, aes(month)) + geom_ribbon(aes(ymin = q1, ymax = q99), fill = "#1f5fa8", alpha = .12) + geom_ribbon(aes(ymin = q5, ymax = q95), fill = "#1f5fa8", alpha = .2) +
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#1f5fa8", alpha = .35) + geom_line(aes(y = q50), colour = "#1f5fa8", linewidth = .7) +
      geom_line(data = hh, aes(month, v), colour = "black", linewidth = .6) + facet_wrap(~series, scales = "free_y", ncol = ncol) +
      labs(title = title, subtitle = sprintf("%s траєкторій OU: t-шоки (df = %.0f), стрибки з апостеріорною частотою, невизначеність параметрів; смуги 50/90/98 %%", format(N, big.mark = " "), DF), x = NULL, y = NULL, caption = CAP) + theme_thesis()
    save_fig(p, file, 9.5, 6.5) }
  fanplot(c("Облікова ставка, %", "Інфляційні очікування, %", "Інфляційні очікування ФО, %", "Очікувана девальвація (ФО), %"), "OU_F5_fan_policy.png", "Віялові прогнози: облікова ставка й очікування", 2)
  fanplot(paste0(sapply(DEP, function(k) SER[[k]][[5]]), ", %"), "OU_F6_fan_deposit_rates.png", "Віялові прогнози ставок за депозитами (OU з очікуваннями груп агентів)", 2)
  vol <- FW[grepl("обсяг", series)]
  p <- ggplot(vol, aes(h)) + geom_ribbon(aes(ymin = q1, ymax = q99), fill = "#2a8a5c", alpha = .12) + geom_ribbon(aes(ymin = q5, ymax = q95), fill = "#2a8a5c", alpha = .22) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#2a8a5c", alpha = .38) + geom_line(aes(y = q50), colour = "#2a8a5c", linewidth = .7) + geom_hline(yintercept = 100, linewidth = .3) +
    facet_wrap(~series, scales = "free_y") + labs(title = "Віялові прогнози обсягів депозитів (OU зі стрибками)", subtitle = "Номінальний обсяг, поточний рівень = 100", x = "Місяців уперед", y = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "OU_F7_fan_volumes.png", 9.5, 6.5)
  qd <- function(MAT, lab) rbindlist(lapply(c(3, 6, 12), function(h) { x <- MAT[, h + 1] - MAT[, 1]; data.table(model = lab, h_m = h, mean = mean(x), sd = sd(x), q0.1 = quantile(x, .001), q1 = quantile(x, .01),
                  q5 = quantile(x, .05), q50 = quantile(x, .5), q95 = quantile(x, .95), q99 = quantile(x, .99), q99.9 = quantile(x, .999)) }))
  QD <- rbind(qd(INJ, "OU без стрибків"), qd(PATH$i1, "OU зі стрибками"), qd(ISTEP, "Кроковий процес рішень НБУ (зі стрибками)"))
  save_tab(QD, "OU_O4_policy_change_quantiles.csv", "O4_di")
  for (mm in unique(QD$model)) fact(paste("O4_di6", mm), sprintf("%s — Δ облікової за 6 міс.: 0,1 %% = %+.0f б.п., 99,9 %% = %+.0f б.п. (медіана %+.0f)", mm,
       1e4 * QD[h_m == 6 & model == mm, q0.1], 1e4 * QD[h_m == 6 & model == mm, q99.9], 1e4 * QD[h_m == 6 & model == mm, q50]))
  dd <- rbind(data.table(model = "OU без стрибків", x = 1e4 * (INJ[, 7] - INJ[, 1])), data.table(model = "OU зі стрибками", x = 1e4 * (PATH$i1[, 7] - PATH$i1[, 1])),
              data.table(model = "Кроковий процес рішень НБУ", x = 1e4 * (ISTEP[, 7] - ISTEP[, 1])))
  p <- ggplot(dd[abs(x) < 2500], aes(x, colour = model, linetype = model)) + geom_density(linewidth = .7, adjust = 1.5) + scale_y_sqrt() +
    labs(title = "Розподіл зміни облікової ставки за 6 міс. у трьох моделях", subtitle = "Шкала щільності — квадратний корінь (видно хвости)", x = "б.п.", y = "щільність", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "OU_F13_policy_change_density.png", 8.5, 4.6)
})

# ------------------------------ O4b two-factor curve (L11) -------------------------------------------------------
blk("O4b двофакторна крива", {
  ten <- c("3m", "6m", "1y", "2y", "3y", "5y", "7y", "10y"); tv <- paste0("zc_uah_", ten, "_avg")
  C <- S[month >= as.Date("2018-01-01"), c("month", intersect(tv, names(S))), with = FALSE]; C <- C[complete.cases(C)]
  CM <- as.matrix(C[, -1]); dC <- diff(CM)
  pc <- prcomp(dC, center = TRUE, scale. = FALSE); Lo <- pc$rotation[, 1:2]
  if (mean(Lo[, 1]) < 0) Lo[, 1] <- -Lo[, 1]; if (Lo[nrow(Lo), 2] - Lo[1, 2] < 0) Lo[, 2] <- -Lo[, 2]
  ve <- pc$sdev^2 / sum(pc$sdev^2)
  Fct <- scale(CM, center = colMeans(CM), scale = FALSE) %*% Lo
  FD <- data.table(month = C$month, f1 = Fct[, 1], f2 = Fct[, 2])
  of <- lapply(c("f1", "f2"), function(v) ou_fit(FD, v, character(0)))
  n <- PAR$N; h6 <- 6; Fs <- matrix(rep(Fct[nrow(Fct), ], each = n), n)
  Fsim <- Fs
  for (h in 1:h6) for (j in 1:2) { o <- of[[j]]; w <- sqrt((o$df_t - 2) / rchisq(n, o$df_t))
    Fsim[, j] <- o$b[1] + o$b[2] * Fsim[, j] + rnorm(n, 0, o$sd_e) * w + (runif(n) < o$lam / 12) * (if (length(o$jumps)) sample(o$jumps, n, TRUE) else 0) }
  dF <- Fsim - Fs
  q <- function(j, p) unname(quantile(dF[, j], p))
  SCV <- data.table(name = c("OU-крива: рівень 99,9 % вгору", "OU-крива: рівень 99,9 % вниз", "OU-крива: нахил 99,9 % (стрімкіша)", "OU-крива: нахил 99,9 % (пологіша)"),
                    f = c(1, 1, 2, 2), dq = c(q(1, .999), q(1, .001), q(2, .999), q(2, .001)))
  for (j in seq_along(ten)) SCV[, (paste0("t_", ten[j])) := 1e4 * dq * Lo[j, f]]
  save_tab(SCV, "OU_O4b_curve_scenarios.csv", "O4b_curve_scen")
  save_tab(data.table(tenor = ten, level = Lo[, 1], slope = Lo[, 2]), "OU_O4b_pca_loadings.csv", "O4b_loadings")
  save_tab(data.table(factor = c("рівень", "нахил"), var_share = ve[1:2], kappa = sapply(of, `[[`, "kappa"), half_life_m = sapply(of, `[[`, "hl_m"),
                      jumps = sapply(of, function(o) length(o$jumps)), df_t = sapply(of, `[[`, "df_t")), "OU_O4b_pca_factors.csv", "O4b_factors")
  ld <- melt(data.table(t = c(.25, .5, 1, 2, 3, 5, 7, 10), `Рівень (PC1)` = Lo[, 1], `Нахил (PC2)` = Lo[, 2]), id.vars = "t")
  p <- ggplot(ld, aes(t, value, colour = variable, linetype = variable)) + geom_hline(yintercept = 0, linewidth = .3) + geom_line(linewidth = .8) + geom_point() +
    labs(title = "Два фактори кривої нульового купона НБУ (головні компоненти змін)", subtitle = sprintf("Частка дисперсії: рівень %.0f %%, нахил %.0f %%", 100 * ve[1], 100 * ve[2]),
         x = "Строк, років", y = "Навантаження", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "OU_F14_curve_factors.png", 8.5, 4.6)
})

# ------------------------------ O5 behavioural NMD (stable and core shares) ----------------------------------
blk("O5 стабільна частка коштів на вимогу", {
  H12 <- PAR$horizon_stable; res <- list()
  for (k in c("hh_dem", "corp_dem")) { nm <- paste0("g_", k); D <- SIM$D[[nm]]; if (is.null(D)) next
    mn <- apply(D[, 1:(H12 + 1)], 1, min); stab <- min(1, quantile(mn, PAR$q_stable))
    blr <- OU[[paste0("r_", k)]]$LR; bi <- blr[term == "i", lr]; bse <- blr[term == "i", lr_se]; src <- "система (OU)"
    pan <- TABLES[["O2c_panel"]]
    if ((!is.finite(bse) || bse > 0.5 || OU[[paste0("r_", k)]]$rho >= 0.99) && !is.null(pan) && SER[[k]][[5]] %in% pan$series) {
      bi <- pan[series == SER[[k]][[5]], beta_i]; src <- "панель банків (OU з FE): системна оцінка неідентифікована" }
    res[[k]] <- data.table(product = k, label = SER[[k]][[5]], stable_q1 = stab, stable_q5 = min(1, quantile(mn, .05)), beta_lr = bi, beta_source = src, core = max(0, min(1, stab * (1 - max(0, min(1, bi))))),
                           mean_min = mean(mn)) }
  SYSCORE <- rbindlist(res); save_tab(SYSCORE, "OU_O5_stable_core_system.csv", "O5_system")
  # bank level (L2): OU-jump on bank demand-deposit growth with empirical-Bayes shrinkage to the size-quartile peer group,
  # posterior-predictive simulation (σ² ~ scaled-inv-χ², λ ~ Gamma), bootstrap 90 % interval of the 1 % quantile
  TA <- B[month == max(month), .(nkb, TA = fcoalesce(col_or_na(B[month == max(month)], "fin_assets"), 0))]
  TA[, peer := cut(rank(-TA), breaks = c(0, 10, 25, 45, Inf), labels = c("1–10", "11–25", "26–45", "46+"))]
  EST <- list()
  for (k in c("hh_dem", "corp_dem")) { dc <- c(hh_dem = "dep_hh_demand_uah", corp_dem = "dep_corp_demand_uah")[[k]]; if (!dc %in% names(B)) next
    d <- B[, .(nkb, month, D = get(dc))][is.finite(D) & D > 0]; setorder(d, nkb, month); d[, g := c(NA, diff(log(D))), by = nkb]
    for (b in unique(d$nkb)) { z <- d[nkb == b & is.finite(g)]; if (nrow(z) < 12 || tail(z$D, 1) < 1) next
      z[, g1 := shift(g, type = "lead")]; z <- z[is.finite(g1)]
      f <- lm(g1 ~ g, z); e <- resid(f); s_rob <- 1.4826 * median(abs(e - median(e))); J <- abs(e - median(e)) > PAR$jump_k * s_rob
      EST[[length(EST) + 1]] <- data.table(nkb = b, product = k, n = nrow(z), c0 = coef(f)[1], rho = min(max(coef(f)[2], -0.9), 0.95), s2 = var(e[!J]),
                                           nJ = sum(J), years = nrow(z) / 12, g_last = tail(z$g, 1), jv = list(e[J])) } }
  EST <- merge(rbindlist(EST), TA[, .(nkb, peer)], by = "nkb", all.x = TRUE); EST[is.na(peer), peer := "46+"]
  EST[, `:=`(s2_peer = median(s2), rho_peer = median(rho), c0_peer = median(c0), lam_peer = median(nJ / years)), by = .(product, peer)]
  EST[, w := n / (n + PAR$k_shrink)]
  EST[, `:=`(s2_post = w * s2 + (1 - w) * s2_peer, rho_post = w * rho + (1 - w) * rho_peer, c0_post = w * c0 + (1 - w) * c0_peer,
             a_post = lam_peer * PAR$prior_years + nJ, b_post = PAR$prior_years + years, peer_only = n < PAR$min_bank_n)]
  EST[peer_only == TRUE, `:=`(s2_post = s2_peer, rho_post = rho_peer, c0_post = c0_peer, a_post = lam_peer * PAR$prior_years + 0.5, b_post = PAR$prior_years)]
  allj <- EST[, .(jv = list({ v <- unlist(jv); if (length(v) >= 5) pmin(pmax(v, quantile(v, .05)), quantile(v, .95)) else v })), by = .(product, peer)]   # peer-group jumps, winsorised
  BK <- rbindlist(lapply(seq_len(nrow(EST)), function(r) { x <- EST[r]; n <- PAR$bank_paths; nu <- x$n + PAR$k_shrink
    s2 <- x$s2_post * nu / rchisq(n, nu); lam <- rgamma(n, max(x$a_post, 0.1), x$b_post)
    jv <- if (length(x$jv[[1]]) >= 1 && !x$peer_only) x$jv[[1]] else allj[product == x$product & peer == x$peer, jv][[1]]
    g <- rep(x$g_last, n); lev <- rep(1, n); mn <- rep(1, n)
    for (h in 1:H12) { jmp <- if (length(jv)) (runif(n) < lam / 12) * sample(jv, n, TRUE) else 0
      g <- x$c0_post + x$rho_post * g + rnorm(n, 0, sqrt(s2)) + jmp; lev <- lev * exp(g); mn <- pmin(mn, lev) }
    bs <- replicate(200, quantile(sample(mn, n, TRUE), PAR$q_stable))
    data.table(nkb = x$nkb, product = x$product, peer = as.character(x$peer), n = x$n, shrink_w = x$w, peer_only = x$peer_only, sigma_m = sqrt(x$s2_post),
               jumps_yr = x$a_post / x$b_post, stable_q1 = min(1, quantile(mn, PAR$q_stable)), stable_lo = min(1, quantile(bs, .05)), stable_hi = min(1, quantile(bs, .95)),
               stable_raw_sigma = sqrt(x$s2)) }))
  bl <- setNames(SYSCORE$beta_lr, SYSCORE$product)
  BK[, core := pmax(0, pmin(1, stable_q1 * (1 - pmax(0, pmin(1, bl[product])))))]
  BCORE <- dcast(BK, nkb ~ product, value.var = c("stable_q1", "core"))
  save_tab(BK, "OU_O5_stable_core_banks_long.csv", "O5_banks"); save_tab(BCORE, "OU_bank_core.csv", "X_bank_core")
  p <- ggplot(BK, aes(stable_q1, fill = product)) + geom_histogram(bins = 25, alpha = .7, position = "identity") +
    geom_vline(data = SYSCORE, aes(xintercept = stable_q1, colour = product), linewidth = .7, linetype = "22") +
    scale_fill_manual(values = c(hh_dem = "#1f5fa8", corp_dem = "#d98c1c"), labels = c(hh_dem = "кошти ФО", corp_dem = "кошти СГ")) +
    scale_colour_manual(values = c(hh_dem = "#1f5fa8", corp_dem = "#d98c1c"), guide = "none") +
    labs(title = "Стабільна частка коштів на вимогу за банками (OU зі стрибками)", subtitle = "1-й процентиль мінімуму обсягу за 12 міс. до поточного; пунктир — система",
         x = "Стабільна частка", y = "Банків", fill = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "OU_F8_stable_shares_banks.png", 8.5, 4.6)
  top <- merge(BK[product == "hh_dem"], B[month == max(month), .(nkb, Ddem = col_or_na(B[month == max(month)], "dep_hh_demand_uah"), bank = fullname)], by = "nkb")[order(-Ddem)][1:min(25, .N)]
  p <- ggplot(top, aes(reorder(substr(bank, 1, 28), Ddem), stable_q1)) + geom_pointrange(aes(ymin = stable_lo, ymax = stable_hi), size = .3) + coord_flip() +
    geom_hline(yintercept = SYSCORE[product == "hh_dem", stable_q1], linetype = "22", linewidth = .3) +
    labs(title = "Стабільна частка коштів ФО на вимогу: 25 найбільших банків", subtitle = "Емпірико-баєсове звуження до групи за розміром; 90 % бутстреп-інтервал; пунктир — система",
         x = NULL, y = "Стабільна частка", caption = CAP) + theme_thesis()
  save_fig(p, "OU_F15_stable_shares_ci.png", 8.5, 6.5)
  fact("O5_core", paste(sprintf("%s: стабільна %.0f %%, β_LR %.2f, ядро %.0f %%", SYSCORE$label, 100 * SYSCORE$stable_q1, SYSCORE$beta_lr, 100 * SYSCORE$core), collapse = "; "))
})

# ------------------------------ O6 NII-at-risk and OU rate scenarios for EVE -----------------------------
blk("O6 ЧПД під ризиком", {
  ITEM_MAP <- c(cd = "r_cd", ib = "i1", loan_corp = "r_loan_corp", loan_hh = "r_loan_hh", ovdp = "r_ovdp", sec = "r_ovdp", res = NA, hh_dem = "r_hh_dem", hh_term = "r_hh_term",
                corp_dem = "r_corp_dem", corp_term = "r_corp_term", nbu = "i1", ibl = "i1", oth = "r_corp_term")
  P <- SIM$PATH; NX <- min(PAR$NX, PAR$N)
  X <- sapply(names(ITEM_MAP), function(it) { s <- ITEM_MAP[[it]]; if (is.na(s) || is.null(P[[s]])) return(rep(0, PAR$N))
    rowMeans(P[[s]][, 2:13, drop = FALSE]) - P[[s]][, 1] })
  save_tab(as.data.table(X[seq_len(NX), ]), "OU_paths_item_rate_change_12m.csv", "X_paths")
  BI <- fread(file.path(OUTPUT_DIR, "section_2_3", "tables", "S23_bank_items_latest.csv"), encoding = "UTF-8")
  BT <- fread(file.path(OUTPUT_DIR, "section_2_3", "tables", "S23_bank_totals_latest.csv"), encoding = "UTF-8")
  BI[, nkb := as.character(nkb)]; BT[, nkb := as.character(nkb)]
  W <- dcast(BI, nkb ~ item, value.var = "uah", fun.aggregate = sum)
  for (it in setdiff(names(ITEM_MAP), names(W))) W[, (it) := 0]
  sg <- ifelse(names(ITEM_MAP) %in% c("hh_dem", "hh_term", "corp_dem", "corp_term", "nbu", "ibl", "oth"), -1, 1)
  Wm <- as.matrix(W[, names(ITEM_MAP), with = FALSE]); Wm[!is.finite(Wm)] <- 0; Wm <- sweep(Wm, 2, sg, `*`)
  # L10: bank-specific pass-through — item rate change scaled by bank θ (2.2) relative to the cross-bank median
  TH <- tryCatch(fread(file.path(OUTPUT_DIR, "section_2_3", "tables", "S23_bank_theta.csv"), encoding = "UTF-8"), error = function(e) NULL)
  KEYM <- c(cd = "y_cd", loan_corp = "y_le", loan_hh = "y_hh", ovdp = "y_refin", sec = "y_refin", hh_dem = "hh_demand", hh_term = "hh_term", corp_dem = "corp_demand", corp_term = "corp_term")
  SCL <- matrix(1, nrow(W), length(ITEM_MAP), dimnames = list(W$nkb, names(ITEM_MAP)))
  if (!is.null(TH)) { TH[, nkb := as.character(nkb)]
    for (it in names(KEYM)) { z <- TH[beta_key == KEYM[[it]]]; med <- median(z$theta, na.rm = TRUE); if (!is.finite(med) || abs(med) < 0.05) next
      r <- pmin(2, pmax(0, z$theta / med)); SCL[match(z$nkb, W$nkb)[!is.na(match(z$nkb, W$nkb))], it] <- r[!is.na(match(z$nkb, W$nkb))] } }
  DN <- X %*% t(Wm)                                        # N × banks, mn UAH over 12 months (system pass-through)
  DNb <- sapply(seq_len(nrow(W)), function(j) X %*% (Wm[j, ] * SCL[j, ]))   # bank-specific pass-through
  # L9: dynamic balance sheet — deposit volumes follow the simulated paths; the change in funding is placed/borrowed at the policy rate
  DYN <- matrix(0, PAR$N, 1)
  for (k in DEP) { V <- SIM$D[[paste0("g_", k)]]; rp <- P[[paste0("r_", k)]]; if (is.null(V) || is.null(rp)) next
    DYN <- DYN + rowMeans((V[, 2:13] - 1) * (P$i1[, 2:13] - rp[, 2:13])) * 1 }
  dynw <- sapply(DEP, function(k) W[[k]]); dynw[!is.finite(dynw)] <- 0
  DYNb <- sapply(seq_len(nrow(W)), function(j) { tot <- 0; for (k in DEP) { V <- SIM$D[[paste0("g_", k)]]; rp <- P[[paste0("r_", k)]]; if (is.null(V)) next
    tot <- tot + dynw[j, k] * rowMeans((V[, 2:13] - 1) * (P$i1[, 2:13] - rp[, 2:13])) }; tot })
  BT <- merge(BT, data.table(nkb = W$nkb, col = seq_len(nrow(W))), by = "nkb")
  BNR <- BT[, { x <- DN[, col]; xb <- DNb[, col]; xd <- xb + DYNb[, col]
    .(dnii_mean = mean(x), dnii_q1 = quantile(x, .01), dnii_q5 = quantile(x, .05), dnii_q50 = quantile(x, .5), es1 = mean(x[x <= quantile(x, .01)]),
      nii_at_risk1_pct_nii = -quantile(x, .01) / NII12, nii_at_risk1_pct_eq = -quantile(x, .01) / EQ,
      dnii_q1_bankbeta = quantile(xb, .01), dnii_q1_dynamic = quantile(xd, .01), dnii_q50_dynamic = quantile(xd, .5)) }, by = .(nkb, bank, EQ, NII12)]
  save_tab(BNR, "OU_bank_nii_at_risk.csv", "X_bank_nii")
  sysx <- rowSums(DN); nii_sys <- sum(BT$NII12, na.rm = TRUE); sysb <- rowSums(DNb); sysd <- sysb + rowSums(DYNb)
  stt <- function(x, lab) data.table(model = lab, stat = c("середнє", "1-й процентиль", "5-й процентиль", "медіана", "95-й процентиль", "ES 1 %"),
                                     dnii = c(mean(x), quantile(x, c(.01, .05, .5, .95)), mean(x[x <= quantile(x, .01)])))
  SN <- rbind(stt(sysx, "постійний баланс, системна передача"), stt(sysb, "постійний баланс, передача банків (θ)"), stt(sysd, "динамічний баланс, передача банків (θ)"))
  SN[, pct_nii := dnii / nii_sys]; save_tab(SN, "OU_O6_system_nii_distribution.csv", "O6_system_nii")
  dd <- rbind(data.table(model = "постійний баланс", x = 100 * sysb / nii_sys), data.table(model = "динамічний баланс", x = 100 * sysd / nii_sys))
  p <- ggplot(dd, aes(x, fill = model)) + geom_histogram(bins = 70, alpha = .55, position = "identity") +
    labs(title = "ЧПД системи за 12 міс.: постійний і динамічний баланс", subtitle = "Динамічний: обсяги депозитів за траєкторіями OU, зміна фондування — за обліковою ставкою",
         x = "% річного ЧПД", y = "Траєкторій", fill = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "OU_F16_nii_static_dynamic.png", 8.5, 4.6)
  p <- ggplot(data.table(x = 100 * sysx / nii_sys), aes(x)) + geom_histogram(bins = 60, fill = "#1f5fa8", alpha = .8) +
    geom_vline(xintercept = 100 * quantile(sysx, .01) / nii_sys, colour = "#c4452d", linetype = "22") +
    labs(title = "Розподіл зміни ЧПД банківської системи за 12 міс. (Монте-Карло OU)", subtitle = "Постійний баланс; червоний пунктир — 1-й процентиль (ЧПД під ризиком)",
         x = "% річного ЧПД", y = "Траєкторій", caption = CAP) + theme_thesis()
  save_fig(p, "OU_F9_nii_distribution.png", 8.5, 4.6)
  for (mm in unique(SN$model)) { z <- SN[model == mm]; fact(paste("O6_sys", mm), sprintf("%s: медіана %+.1f %%, 1-й процентиль %+.1f %%, ES 1 %% %+.1f %% річного ЧПД", mm, 100 * z$pct_nii[4], 100 * z$pct_nii[2], 100 * z$pct_nii[6])) }
  # OU-consistent EVE scenarios: Δi quantiles at 6 months × Vasicek decay
  kap <- OU$i1$kappa; QD <- TABLES[["O4_di"]]
  q6 <- function(mm, col) QD[h_m == 6 & model == mm][[col]]
  SCN <- data.table(name = c("OU 99,9 % вгору (стрибки, Васічек)", "OU 99,9 % вниз (стрибки, Васічек)"), type = "vasicek",
                    d_i_bp = 1e4 * c(q6("OU зі стрибками", "q99.9"), q6("OU зі стрибками", "q0.1")), kappa = kap)
  CV <- TABLES[["O4b_curve_scen"]]
  if (!is.null(CV)) SCN <- rbind(SCN, CV[, c(list(name = name, type = "curve", d_i_bp = NA_real_, kappa = NA_real_), .SD), .SDcols = patterns("^t_")], fill = TRUE)
  save_tab(SCN, "OU_stress_shocks.csv", "X_shocks")
  tau <- c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 15, 20)
  save_tab(data.table(tau = tau, decay = (1 - exp(-kap * tau)) / (kap * tau)), "OU_term_decay.csv", "X_decay")
  p <- ggplot(data.table(tau = seq(0.05, 20, .05))[, decay := (1 - exp(-kap * tau)) / (kap * tau)], aes(tau, decay)) + geom_line(colour = "#1f5fa8", linewidth = .8) +
    labs(title = "Як шок короткої ставки переходить на криву (модель Васічека з κ облікової)", subtitle = sprintf("ΔR(τ) = Δi·(1 − e^(−κτ))/(κτ), κ = %.2f", kap),
         x = "Строк τ, років", y = "Частка шоку", caption = CAP) + theme_thesis()
  save_fig(p, "OU_F10_vasicek_decay.png", 8.5, 4.4)
})

# ------------------------------ O7 limitations and treatment ---------------------------------------------------
blk("O7 обмеження", {
  js <- TABLES[["O1b_jump_summary"]][1]; qd <- TABLES[["O4_di"]]; bt <- TABLES[["O1c_bootstrap"]]; bk <- TABLES[["O5_banks"]]; sn <- TABLES[["O6_system_nii"]]
  q6 <- function(mm, col) 1e4 * qd[h_m == 6 & model == mm][[col]]
  LIM <- data.table(
    id = paste0("L", 1:11),
    limitation = c("Один стрибок облікової у 2018–2026 рр.", "Нестійкі стабільні частки малих банків", "Очікування груп агентів у симуляції замінені середніми",
                   "Облікова — кроковий процес рішень, а не дифузія", "Зміщення і вибіркова похибка ρ у коротких рядах", "Нестабільність параметрів (війна, зміна режиму)",
                   "Нормальні шоки недооцінюють хвости", "Помилка вимірювання очікувань в опитуваннях", "Постійний баланс у ЧПД", "Однакова передача для всіх банків",
                   "Однофакторна крива (Васічек) для EVE"),
    treatment = c("Історія облікової 2010–2026; стрибок — зміна ≥ 3 в.п. за місяць; частота — апостеріорний Гамма–Пуассон, розмір — емпіричний × U(0,75; 1,25)",
                  "Емпірико-баєсове звуження σ, ρ, λ до групи за розміром; апостеріорно-предиктивна симуляція; бутстреп-інтервали; < 36 міс. — параметри групи",
                  "OU для очікувань ФО і підприємств із прив'язкою до середнього; спільна симуляція з корельованими шоками",
                  "Варіант: рішення лише в місяці засідань НБУ, сітка 25 б.п.", "Поправка Кендалла для ρ; блочний бутстреп залишків (499 повторень) для κ і напіврозпаду",
                  "Ковзне вікно 48 міс.; оцінки без 02–12.2022", "Багатовимірний t-розподіл шоків (ступені свободи за ексцесом залишків)",
                  "2SLS: інструменти — лаг власних очікувань і лаг середніх очікувань", "Динамічний баланс: обсяги депозитів за траєкторіями OU, різниця фондування — за обліковою",
                  "Масштабування змін ставок статей на θ банку з п. 2.2 відносно медіани", "Дві головні компоненти кривої НБУ (рівень, нахил) як OU; сценарії 99,9 % для EVE"),
    result = c(if (!is.null(js)) sprintf("%d стрибків за %.1f року; частота %.2f на рік (90 %%: %.2f–%.2f)", js$jumps, js$years, js$lambda_post_mean, js$lambda_post_q05, js$lambda_post_q95) else "",
               if (!is.null(bk)) sprintf("медіана ширини 90 %%-інтервалу %.1f в.п.; на параметрах групи %d банко-продуктів", 100 * median(bk$stable_hi - bk$stable_lo), sum(bk$peer_only)) else "",
               "ставки ФО реагують на очікування ФО, ставки СГ — на очікування підприємств",
               if (!is.null(qd)) sprintf("99,9 %% за 6 міс.: дифузія %+.0f, зі стрибками %+.0f, кроковий %+.0f б.п.", q6("OU без стрибків", "q99.9"), q6("OU зі стрибками", "q99.9"), q6("Кроковий процес рішень НБУ (зі стрибками)", "q99.9")) else "",
               if (!is.null(bt)) sprintf("облікова: напіврозпад %.1f міс. (90 %%: %.1f–%.1f)", bt[eq == "i1", half_life_m], bt[eq == "i1", hl_lo], bt[eq == "i1", hl_hi]) else "",
               "див. OU_O1d_rolling_kappa.csv і рис. OU_F11", sprintf("df = %.0f", SIM$DF), "див. OU_O2d_iv_expectations.csv",
               if (!is.null(sn)) sprintf("1-й процентиль ЧПД системи: постійний %+.1f %%, динамічний %+.1f %%", 100 * sn[model == "постійний баланс, передача банків (θ)" & stat == "1-й процентиль", pct_nii], 100 * sn[model == "динамічний баланс, передача банків (θ)" & stat == "1-й процентиль", pct_nii]) else "",
               if (!is.null(sn)) sprintf("1-й процентиль ЧПД системи: системна передача %+.1f %%, передача банків %+.1f %%", 100 * sn[model == "постійний баланс, системна передача" & stat == "1-й процентиль", pct_nii], 100 * sn[model == "постійний баланс, передача банків (θ)" & stat == "1-й процентиль", pct_nii]) else "",
               "див. OU_O4b_* і сценарії «OU-крива» у стрес-тесті 13"),
    remaining = c("Стрибки 2014–2015 рр. — в іншому монетарному режимі (фіксований курс); оцінка частоти залежить від порогу 3 в.п.",
                  "Звуження зменшує шум, але не усуває зміни бізнес-моделей банків і злиттів", "Очікування самі не залежать від облікової (немає структурної моделі)",
                  "Розмір кроку рішення не моделюється окремо від цілі OU", "Ряди 2018–2026 рр. короткі, війна домінує у вибірці", "Переломи не датуються формально",
                  "t-розподіл симетричний", "Інструменти можуть бути слабкими (див. F першого етапу)", "Активи не реагують на зміну обсягів депозитів", "θ банків — оцінки з похибкою",
                  "Валютна крива не моделюється стохастично"))
  save_tab(LIM, "OU_O7_limitations.csv", "O7_limitations")
})

# parameters export for the Excel
blk("Excel-входи", {
  par <- rbindlist(lapply(c("i1", grep("^r_|^g_", names(OU), value = TRUE)), function(nm) { o <- OU[[nm]]; data.table(eq = nm, rho = o$rho, kappa = o$kappa, half_life_m = o$hl_m, sigma = o$sigma, lambda_yr = o$lam) }))
  save_tab(par, "OU_params.csv", "X_params")
  save_tab(TABLES[["O4_fan"]][series == "Облікова ставка, %"], "OU_fan_policy.csv", "X_fan_policy")
})

save_tab(STATUS, "OU_00_blocks_status.csv", "Status"); save_tab(FACTS, "OU_00_key_facts.csv", "Key_facts")
write_workbook("Section_OU.xlsx")
print(STATUS); print(FACTS)
log_msg("OU DEPOSIT DYNAMICS COMPLETE: ", length(TABLES), " tables, ", sprintf("%.1f хв", as.numeric(difftime(Sys.time(), t0_all, units = "mins"))))
