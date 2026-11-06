# ============================================================================
# 04_section_2_1_deposit_dynamics.R — DEPOSIT CONVEXITY & EXTENDED METRICS
# ----------------------------------------------------------------------------
# Run AFTER 02_build_marts_v6.R and 03_section_2_1_analysis_v2.R.
# Output: output_nbu/section_2_1_v2/{tables,figures}/D*  + Section_2_1_v2_dynamics.xlsx
#
# Blocks
#   D1  State-dependent long-difference betas (system): deposit convexity
#       Δh r_t = a + γ1 Δh i_t + γ2 Δh i_t · i_{t-h} + e ;  β(i0) = γ1 + γ2 i0
#   D2  Local projections (Jordà) with state dependence and sign asymmetry
#   D3  Bank-panel convexity: Δ12 r_it on Δ12 i × {level, demand share, size,
#       HH share, ownership}, bank FE, Driscoll–Kraay SE
#   D4  Deposit spreads (Drechsler–Savov–Schnabl) and deposit-franchise income
#       vs net interest income
#   D5  Effective duration of the deposit base (Greenwald–Schulhofer-Wohl–
#       Younger logic, calibrated): D(i) = 1/(i+δ) − β(i)/(r(i)+δ)
#   D6  Stock-flow repricing speed λ (book vs new business), by period/rolling
#   D7  Shapley decomposition of the mix effect: maturity vs sector
#   D8  Ownership gap decomposition (composition vs price), Oaxaca-type
#
# Discipline: these are descriptive / reduced-form. Few independent policy
# cycles (≈4–5) and the war-time 25% rate (06.2022) dominate the variation;
# causal and dynamic pass-through inference is the subject of Section 2.2.
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
.need(c("ggplot2", "scales", "sandwich", "openxlsx"))
suppressPackageStartupMessages({ library(ggplot2); library(scales) })

OUT <- file.path(OUTPUT_DIR, "section_2_1_v2"); FIG <- file.path(OUT, "figures"); TAB <- file.path(OUT, "tables")
for (d in c(FIG, TAB)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(FIG, pattern = "^D", full.names = TRUE)); unlink(list.files(TAB, pattern = "^D", full.names = TRUE))

sys  <- store_read("mart_system_month_v6");  sys[, month := as.Date(month)]
bank <- store_read("mart_bank_month_v6");    bank[, month := as.Date(month)]
prod <- store_read("mart_system_product_v6"); prod[, month := as.Date(month)]
sys <- sys[month >= START_MONTH]; bank <- bank[month >= START_MONTH]; prod <- prod[month >= START_MONTH]
setorder(sys, month)
has <- function(dt, v) v %in% names(dt) && any(is.finite(dt[[v]]))

# ------------------------------ style (same as 03) ---------------------------
PAL <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"
theme_thesis <- function() theme_minimal(base_size = 11) + theme(
  plot.title = element_text(face = "bold", size = 12.5, colour = INK),
  plot.subtitle = element_text(size = 9.5, colour = INK2), plot.caption = element_text(size = 8, colour = INK2, hjust = 0),
  axis.text = element_text(colour = INK2), axis.title = element_text(size = 9.5, colour = INK2),
  panel.grid.major = element_line(colour = GRID, linewidth = .3), panel.grid.minor = element_blank(),
  legend.position = "bottom", legend.title = element_blank(), strip.text = element_text(face = "bold", colour = INK))
CAP <- "Джерело: розрахунки автора за даними НБУ (OSB, MIR, облікова ставка). Описові оцінки; причинний аналіз — підрозділ 2.2."
source(file.path(PROJECT_DIR, "utils_origin_v6.R"), encoding = "UTF-8")
save_fig <- function(p, f, w = 8.5, h = 4.8) {
  ggsave(file.path(FIG, f), p, width = w, height = h, dpi = 300, bg = "white")
  tryCatch(origin_export(p, f, w, h), error = function(e) log_msg("Origin export skipped for ", f, ": ", conditionMessage(e)))
}
source(file.path(PROJECT_DIR, "utils_figures_academic_v6.R"), encoding = "UTF-8", local = environment())   # academic style: overrides PAL, theme_thesis(), save_fig()
TABLES <- list()
save_tab <- function(x, f, sheet) { x <- as.data.table(x); fwrite(x, file.path(TAB, f)); TABLES[[sheet]] <<- x; invisible(x) }
FACTS <- data.table(item = character(), value = character(), note = character())
fact <- function(k, v, n = "") FACTS <<- rbind(FACTS, data.table(item = k, value = as.character(v), note = n))
p2 <- function(x, d = 2) formatC(100 * x, format = "f", digits = d)

# ------------------------------ estimators -----------------------------------
# OLS with Newey–West HAC covariance.
nw_ols <- function(y, X, lags) {
  ok <- is.finite(y) & apply(is.finite(X), 1, all)
  y <- y[ok]; X <- cbind(`(Intercept)` = 1, X[ok, , drop = FALSE])
  fit <- lm.fit(X, y); u <- fit$residuals; b <- fit$coefficients
  bread <- solve(crossprod(X)); S <- crossprod(X * u)
  for (l in seq_len(lags)) {
    w <- 1 - l / (lags + 1); G <- crossprod(X[-(1:l), , drop = FALSE] * u[-(1:l)], X[1:(length(u) - l), , drop = FALSE] * u[1:(length(u) - l)])
    S <- S + w * (G + t(G))
  }
  V <- bread %*% S %*% bread * length(y) / (length(y) - ncol(X))
  list(b = b, V = V, n = length(y), r2 = 1 - sum(u^2) / sum((y - mean(y))^2))
}
# Within estimator (one-way bank FE or more), Driscoll–Kraay or cluster SE.
fe_dk <- function(d, y, x, fe = "nkb", time = "month", vcov = c("dk", "cluster"), lags = NULL) {
  vcov <- match.arg(vcov)
  cols <- c(y, x, fe, time); d <- d[complete.cases(d[, ..cols])]
  cyx <- c(y, x); Z <- as.matrix(d[, ..cyx])
  for (it in 1:200) { Z0 <- Z; for (f in fe) { g <- as.character(d[[f]]); Z <- Z - rowsum(Z, g)[g, , drop = FALSE] / as.vector(table(g)[g]) }; if (max(abs(Z - Z0)) < 1e-12) break }
  yv <- Z[, 1]; X <- Z[, -1, drop = FALSE]; B <- solve(crossprod(X)); b <- B %*% crossprod(X, yv); u <- as.vector(yv - X %*% b)
  if (vcov == "cluster") {
    cl <- d[[fe[1]]]; G <- length(unique(cl)); S <- crossprod(rowsum(X * u, cl)); adj <- G / (G - 1)
  } else {
    tt <- d[[time]]; h <- rowsum(X * u, tt); h <- h[order(rownames(h)), , drop = FALSE]; Tn <- nrow(h)
    L <- lags %||% floor(4 * (Tn / 100)^(2 / 9)); S <- crossprod(h)
    for (l in seq_len(L)) { G <- crossprod(h[-(1:l), , drop = FALSE], h[1:(Tn - l), , drop = FALSE]); S <- S + (1 - l / (L + 1)) * (G + t(G)) }
    adj <- 1
  }
  V <- B %*% S %*% B * adj; se <- sqrt(diag(V))
  list(tab = data.table(term = colnames(X), estimate = as.vector(b), std.error = se, t = as.vector(b) / se,
                        p.value = 2 * pnorm(-abs(as.vector(b) / se)), n_obs = nrow(X), n_banks = uniqueN(d[[fe[1]]])),
       b = setNames(as.vector(b), colnames(X)), V = V)
}

# ------------------------------ series ---------------------------------------
pc <- dcast(prod[ccy == "total" & core4 == TRUE], month ~ product, value.var = "rate")
S <- merge(sys[, .(month, i = policy_avg, cod4 = cod4_total, cod4_uah, cod4_fx)], pc, by = "month", all.x = TRUE)
mir_map <- c(nb_hh_term = "nb_dep_hh_term_total", nb_nfc_term = "nb_dep_nfc_term_total",
             nb_hh_demand = "nb_dep_hh_demand_total", nb_nfc_demand = "nb_dep_nfc_demand_total",
             nb_hh_term_uah = "nb_dep_hh_term_uah", nb_nfc_term_uah = "nb_dep_nfc_term_uah",
             uird3m = "uird_uah_3m_avg", uird12m = "uird_uah_12m_avg")   # UIRD: daily posted rates, no MIR gap
for (k in names(mir_map)) S[, (k) := if (has(sys, mir_map[[k]])) sys[[mir_map[[k]]]][match(month, sys$month)] else NA_real_]
setorder(S, month)
SER <- c(cod4 = "Вартість депозитів (книжкова)", hh_demand = "ФО: на вимогу (книжкова)", corp_demand = "СГ: на вимогу (книжкова)",
         hh_term = "ФО: строкові (книжкова)", corp_term = "СГ: строкові (книжкова)",
         nb_hh_term = "ФО: строкові, нові (MIR)", nb_nfc_term = "СГ: строкові, нові (MIR)",
         nb_hh_demand = "ФО: на вимогу, нові (MIR)", nb_nfc_demand = "СГ: на вимогу, нові (MIR)",
         cod4_uah = "Вартість депозитів, UAH", cod4_fx = "Вартість депозитів, FX",
         uird3m = "UIRD 3M (оголошені ставки ФО)", uird12m = "UIRD 12M (оголошені ставки ФО)")
SER <- SER[vapply(names(SER), function(v) has(S, v) && sum(is.finite(S[[v]])) >= 48, logical(1))]
GRID_I <- c(.06, .10, .15, .20, .25)

# ======================= D1. STATE-DEPENDENT LONG-DIFFERENCE BETAS ===========
d1 <- rbindlist(lapply(c(6L, 12L, 24L), function(h) rbindlist(lapply(names(SER), function(v) {
  y <- S[[v]] - shift(S[[v]], h); di <- S$i - shift(S$i, h); i0 <- shift(S$i, h)
  X <- cbind(di = di, di_x_i0 = di * i0)
  if (sum(is.finite(y) & is.finite(di)) < 30) return(NULL)
  f <- nw_ols(y, X, lags = h)
  g1 <- f$b[["di"]]; g2 <- f$b[["di_x_i0"]]
  V <- f$V[c("di", "di_x_i0"), c("di", "di_x_i0")]
  rbindlist(lapply(GRID_I, function(i0v) {
    be <- g1 + g2 * i0v; se <- sqrt(V[1, 1] + i0v^2 * V[2, 2] + 2 * i0v * V[1, 2])
    data.table(series = v, label = SER[[v]], horizon_m = h, i0 = i0v, beta = be, se = se, lo90 = be - 1.645 * se, hi90 = be + 1.645 * se,
               gamma2 = g2, se_gamma2 = sqrt(V[2, 2]), p_gamma2 = 2 * pnorm(-abs(g2 / sqrt(V[2, 2]))), n = f$n)
  }))
}))))
save_tab(d1, "D1_state_dependent_betas.csv", "D1_betas")
d1w <- dcast(d1[horizon_m == 12], label + gamma2 + p_gamma2 + n ~ paste0("beta_at_", 100 * i0, "pct"), value.var = "beta")
save_tab(d1w, "D1b_convexity_summary_12m.csv", "D1_summary_12m")
pick <- intersect(c("cod4", "hh_term", "corp_term", "nb_hh_term", "nb_nfc_term", "corp_demand", "uird3m", "uird12m"), names(SER))
p <- ggplot(d1[horizon_m == 12 & series %in% pick], aes(i0, beta)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
  geom_ribbon(aes(ymin = lo90, ymax = hi90), fill = PAL[1], alpha = .15) + geom_line(colour = PAL[1], linewidth = .8) + geom_point(colour = PAL[1], size = 1.4) +
  facet_wrap(~label, ncol = 3) + scale_x_continuous(labels = label_percent(1)) +
  labs(title = "Опуклість депозитів: 12-місячна бета залежно від стартового рівня облікової ставки",
       subtitle = "Δ12 r = γ1·Δ12 i + γ2·Δ12 i·i(t−12); β(i0) = γ1 + γ2·i0; 90% довірчий інтервал Newey–West",
       x = "Облікова ставка на початку вікна", y = "Бета (частка передачі)", caption = CAP) + theme_thesis()
save_fig(p, "D1_convexity_betas.png", 9.5, 7.6)
for (v in intersect(c("nb_nfc_term", "nb_hh_term", "hh_term", "corp_term", "cod4", "uird3m", "uird12m"), names(SER))) {
  z <- d1[horizon_m == 12 & series == v]
  fact(paste0("convexity_", v), sprintf("β(6%%)=%.2f, β(15%%)=%.2f, β(25%%)=%.2f; γ2=%.2f (p=%.3f)",
       z[i0 == .06, beta], z[i0 == .15, beta], z[i0 == .25, beta], z$gamma2[1], z$p_gamma2[1]))
}

# ======================= D2. LOCAL PROJECTIONS ================================
# y_{t+h} − y_{t−1} = a + β_h Δi_t + θ_h Δi_t (i_{t−1} − ī) + Σ_{l=1..3}(ρ Δy_{t−l} + φ Δi_{t−l}) + e
# Asymmetric variant: Δi split into increases and decreases.
lp_one <- function(v, H = 24, state = TRUE, asym = FALSE) {
  y <- S[[v]]; i <- S$i; di <- i - shift(i); ibar <- mean(i, na.rm = TRUE)
  ctrl <- do.call(cbind, lapply(1:3, function(l) cbind(shift(y - shift(y), l), shift(di, l))))
  rbindlist(lapply(0:H, function(h) {
    lhs <- shift(y, h, type = "lead") - shift(y, 1)
    X <- if (asym) cbind(up = pmax(di, 0), down = pmin(di, 0), ctrl) else
         if (state) cbind(di = di, di_state = di * (shift(i) - ibar), ctrl) else cbind(di = di, ctrl)
    colnames(X)[-(1:(if (asym || state) 2 else 1))] <- paste0("c", seq_len(ncol(ctrl)))
    f <- tryCatch(nw_ols(lhs, X, lags = h + 1), error = function(e) NULL); if (is.null(f)) return(NULL)
    if (asym) data.table(h = h, term = c("up", "down"), b = f$b[c("up", "down")], se = sqrt(diag(f$V)[c("up", "down")]), n = f$n) else
    if (state) {
      out <- rbindlist(lapply(c(low = .08, high = .20), function(s) {
        w <- c(1, s - ibar); V <- f$V[c("di", "di_state"), c("di", "di_state")]
        data.table(b = sum(w * f$b[c("di", "di_state")]), se = sqrt(as.numeric(t(w) %*% V %*% w)))
      }))
      out[, `:=`(h = h, term = c("стан: облікова 8%", "стан: облікова 20%"), n = f$n)][]
    } else NULL
  }))[, series := v]
}
# Book-rate series + UIRD: MIR has a 15-month gap (2023-02 … 2024-04) exactly in the
# easing phase, which leaves the state/asymmetry terms unidentified for MIR. UIRD
# (posted marginal household rates, daily) has no gap and stands in for MIR here.
lp_vars <- intersect(c("cod4", "hh_term", "corp_term", "corp_demand", "uird3m"), names(SER))
d2s <- rbindlist(lapply(lp_vars, lp_one, state = TRUE)); d2a <- rbindlist(lapply(lp_vars, lp_one, state = FALSE, asym = TRUE))
d2a[, term := fifelse(term == "up", "підвищення ставки", "зниження ставки")]
d2 <- rbind(d2s, d2a, fill = TRUE); d2[, label := SER[series]]
save_tab(d2, "D2_local_projections.csv", "D2_LP")
for (nm in c("state", "asym")) {
  z <- if (nm == "state") d2s else d2a; z[, label := SER[series]]
  p <- ggplot(z, aes(h, b, colour = term, fill = term)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
    geom_ribbon(aes(ymin = b - 1.645 * se, ymax = b + 1.645 * se), alpha = .12, colour = NA) + geom_line(linewidth = .8) +
    facet_wrap(~label, ncol = 3, scales = "free_y") + scale_colour_manual(values = PAL[c(1, 2)]) + scale_fill_manual(values = PAL[c(1, 2)]) +
    labs(title = if (nm == "state") "Кумулятивна передача облікової ставки: низький vs високий рівень ставок" else
                 "Кумулятивна передача: підвищення vs зниження облікової ставки",
         subtitle = "Локальні проєкції (Jordà), місячні дані, 3 лаги контролів; 90% інтервали Newey–West; попередній аналіз до 2.2",
         x = "Горизонт, місяців", y = "в.п. ставки на 1 в.п. облікової", caption = CAP) + theme_thesis()
  save_fig(p, paste0("D2_lp_", nm, ".png"), 9.5, 6)
}
for (v in lp_vars) {
  z <- d2s[series == v & h == 12]; a <- d2a[series == v & h == 12]
  if (nrow(z) == 2) fact(paste0("lp12_state_", v), sprintf("низький стан %.2f, високий %.2f", z$b[1], z$b[2]))
  if (nrow(a) == 2) fact(paste0("lp12_asym_", v), sprintf("підвищення %.2f, зниження %.2f", a[term == "підвищення ставки", b], a[term == "зниження ставки", b]))
}

# ======================= D3. BANK-PANEL CONVEXITY =============================
bp <- merge(bank[, .(nkb, month, cod4_total, r_hh_term_total, r_corp_term_total, r_corp_demand_total, r_hh_demand_total,
                     demand_share, hh_share, size_base, owner, dq_rate_out_of_bounds)],
            S[, .(month, i)], by = "month")
setorder(bp, nkb, month)
bp[, month_l12 := add_months(month, -12L)]
lag12 <- bp[, .(nkb, month_l12 = month, i_l = i, dem_l = demand_share, hh_l = hh_share, size_l = log(pmax(size_base, 1)),
                c_l = cod4_total, ht_l = r_hh_term_total, ct_l = r_corp_term_total, cd_l = r_corp_demand_total)]
bp <- merge(bp, lag12, by = c("nkb", "month_l12"))
bp[, `:=`(di = i - i_l, dy_cod = cod4_total - c_l, dy_ht = r_hh_term_total - ht_l, dy_ct = r_corp_term_total - ct_l,
          dy_cd = r_corp_demand_total - cd_l)]
for (v in c("dy_cod", "dy_ht", "dy_ct", "dy_cd")) bp[, (v) := winsor(get(v), c(.01, .99))]
bp[, `:=`(dem_c = dem_l - mean(dem_l, na.rm = TRUE), size_c = size_l - mean(size_l, na.rm = TRUE),
          hh_c = hh_l - mean(hh_l, na.rm = TRUE), i_c = i_l - mean(i_l, na.rm = TRUE),
          state = as.numeric(owner == "State"), foreign = as.numeric(owner == "Foreign"))]
bp[, `:=`(di_i = di * i_c, di_dem = di * dem_c, di_size = di * size_c, di_hh = di * hh_c, di_state = di * state, di_foreign = di * foreign)]
bp <- bp[!dq_rate_out_of_bounds | is.na(dq_rate_out_of_bounds)]
d3 <- rbindlist(lapply(list(list("dy_cod", "Вартість депозитів"), list("dy_ht", "ФО: строкові"), list("dy_ct", "СГ: строкові"), list("dy_cd", "СГ: на вимогу")),
  function(s) {
    r <- tryCatch(fe_dk(bp, s[[1]], c("di", "di_i", "di_dem", "di_size", "di_hh", "di_state", "di_foreign"), vcov = "dk", lags = 12),
                  error = function(e) { warning(s[[2]], ": ", conditionMessage(e)); NULL })
    if (is.null(r)) return(NULL); r$tab[, outcome := s[[2]]]
  }))
save_tab(d3, "D3_bank_panel_convexity.csv", "D3_panel")
if (nrow(d3)) {
  z <- d3[outcome == "Вартість депозитів"]
  fact("panel_beta_avg", sprintf("%.3f (на середньому банку при середній ставці)", z[term == "di", estimate]))
  fact("panel_convexity_gamma", sprintf("%.3f (p=%.3f)", z[term == "di_i", estimate], z[term == "di_i", p.value]))
  fact("panel_demand_interaction", sprintf("%.3f (p=%.3f): +10 в.п. частки на вимогу → бета %+.3f", z[term == "di_dem", estimate],
                                           z[term == "di_dem", p.value], .1 * z[term == "di_dem", estimate]))
}

# ======================= D4. DEPOSIT SPREADS & FRANCHISE INCOME ==============
ps <- merge(prod[ccy == "total" & core4 == TRUE, .(month, product, rate, avg_stock)], S[, .(month, i)], by = "month")
ps[, spread := i - rate]
ps[, ann := ann_factor(month)]
fr <- ps[is.finite(spread) & is.finite(avg_stock), .(franchise_bn = sum(spread * avg_stock) / 1000), by = month]   # mn UAH -> bn UAH, annual rate
if ("fin_nii_m" %in% names(bank)) {
  nii <- bank[, .(nii_m = sum(fin_nii_m, na.rm = TRUE)), by = month]
  nii[, nii_bn_ann := nii_m * ann_factor(month) / 1000]
  fr <- merge(fr, nii[, .(month, nii_bn_ann)], by = "month", all.x = TRUE)
  fr[, nii_bn_ann_12m := frollmean(nii_bn_ann, 12)]
  fr[, franchise_to_nii := franchise_bn / nii_bn_ann_12m]
}
save_tab(ps, "D4_deposit_spreads_by_product.csv", "D4_spreads")
save_tab(fr, "D4b_franchise_income_vs_nii.csv", "D4_franchise")
PL <- c(corp_demand = "СГ: на вимогу", corp_term = "СГ: строкові", hh_demand = "ФО: на вимогу", hh_term = "ФО: строкові")
p <- ggplot(ps[is.finite(spread)], aes(i, spread, colour = PL[product])) + geom_abline(slope = 1, intercept = 0, linetype = 3, colour = INK2) +
  geom_point(size = 1.1, alpha = .6) + geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linewidth = .8) +
  scale_colour_manual(values = PAL[1:4]) + scale_x_continuous(labels = label_percent(1)) + scale_y_continuous(labels = label_percent(1)) +
  labs(title = "Депозитний спред (облікова − ставка продукту) залежно від рівня облікової ставки",
       subtitle = "Нахил = 1 − бета; пунктир — спред при нульовій передачі; квадратичне згладжування", x = "Облікова ставка", y = "Спред", caption = CAP) + theme_thesis()
save_fig(p, "D4_spread_vs_level.png")
if ("nii_bn_ann_12m" %in% names(fr)) {
  fl <- melt(fr[, .(month, `Дохід депозитної франшизи (спред × залишок)` = franchise_bn, `Чистий процентний дохід, 12-міс. середнє` = nii_bn_ann_12m)], id.vars = "month")
  p <- ggplot(fl[is.finite(value)], aes(month, value, colour = variable)) + geom_line(linewidth = .8) + scale_colour_manual(values = PAL[c(1, 2)]) +
    labs(title = "Дохід депозитної франшизи та чистий процентний дохід банківської системи",
         subtitle = "Річні темпи, млрд грн; франшиза = Σ (облікова − ставка продукту) × середній залишок, 4 продукти СГ і ФО",
         x = NULL, y = "млрд грн на рік", caption = CAP) + theme_thesis()
  save_fig(p, "D4b_franchise_vs_nii.png")
  fact("franchise_last_bn", sprintf("%.0f млрд грн/рік; %.0f%% ЧПД", fr[.N, franchise_bn], 100 * fr[.N, franchise_to_nii]))
  fact("franchise_peak_bn", sprintf("%.0f млрд грн/рік (%s)", max(fr$franchise_bn, na.rm = TRUE), fr[which.max(franchise_bn), month]))
}
yr <- ps[, .(spread = mean(spread, na.rm = TRUE)), by = .(year = year(month), product)]
save_tab(dcast(yr, year ~ product, value.var = "spread"), "D4c_spreads_annual.csv", "D4_spreads_annual")

# ======================= D5. EFFECTIVE DURATION OF THE DEPOSIT BASE ==========
# Deposits as a liability paying r(i) with runoff δ: PV per UAH = (r+δ)/(i+δ).
# Effective duration D = −∂ln PV/∂i = 1/(i+δ) − β(i)/(r(i)+δ)   (years).
# β(i): 12-month state-dependent betas (D1); r(i): fitted level of the product
# rate at policy level i (quadratic in i). Term deposits: D ≈ h/24 years, h = 1/λ.
# Calibrated, not estimated: δ is an assumption (BCBS IRRBB caps core NMD
# maturity at 5 years for retail).
rfit <- function(v) { z <- S[is.finite(get(v)) & is.finite(i)]; m <- lm(z[[v]] ~ z$i + I(z$i^2)); function(x) sum(coef(m) * c(1, x, x^2)) }
d5 <- rbindlist(lapply(intersect(c("hh_demand", "corp_demand"), names(SER)), function(v) {
  rf <- rfit(v); bz <- d1[horizon_m == 12 & series == v]
  CJ(i0 = GRID_I, delta = c(.10, .20, .30))[, `:=`(product = v,
     beta = bz$beta[match(i0, bz$i0)], r = vapply(i0, rf, 0))][, duration := 1 / (i0 + delta) - pmax(beta, 0) / (pmax(r, 0) + delta)]
}))
save_tab(d5, "D5_effective_duration_nmd.csv", "D5_duration")
if (nrow(d5)) {
  p <- ggplot(d5, aes(i0, duration, colour = paste0("δ = ", 100 * delta, "%"))) + scale_colour_discrete(labels = math_label) + geom_line(linewidth = .8) + geom_point(size = 1.5) +
    facet_wrap(~PL[product]) + scale_colour_manual(values = PAL[c(1, 2, 3)]) + scale_x_continuous(labels = label_percent(1)) +
    labs(title = "Ефективна дюрація коштів на вимогу як функція рівня ставок (калібрування)",
         subtitle = "D = 1/(i+δ) − β(i)/(r+δ); δ — річний відтік; зростання β з рівнем ставок скорочує дюрацію (опуклість)",
         x = "Облікова ставка", y = "Років", caption = CAP) + theme_thesis()
  save_fig(p, "D5_duration_nmd.png")
  z <- d5[delta == .2 & product == "hh_demand"]
  fact("duration_hh_demand_delta20", sprintf("%.1f р. при 6%%, %.1f р. при 15%%, %.1f р. при 25%%", z[i0 == .06, duration], z[i0 == .15, duration], z[i0 == .25, duration]))
}

# ======================= D6. STOCK-FLOW REPRICING SPEED =======================
# Term products only: MIR "new demand deposits" cover a small interest-bearing
# subset of current accounts and are not the marginal price of the demand book.
pairs <- list(hh_term = "nb_hh_term", corp_term = "nb_nfc_term")
pairs <- pairs[vapply(names(pairs), function(k) has(S, k) && has(S, pairs[[k]]), logical(1))]
lam <- function(z) { x <- z$gap; y <- z$db; if (length(x) < 12) return(list(l = NA_real_, se = NA_real_, n = length(x)))
  l <- sum(x * y) / sum(x^2); r <- y - l * x; list(l = l, se = sqrt(sum(r^2) / (length(x) - 1) / sum(x^2)), n = length(x)) }
d6 <- rbindlist(lapply(names(pairs), function(k) {
  z <- S[, .(month, book = get(k), new = get(pairs[[k]]))]
  z[, `:=`(db = book - shift(book), gap = new - shift(book))]; z <- z[is.finite(db) & is.finite(gap)]
  rbindlist(lapply(list(all = z, prewar = z[month < WAR_START], war = z[month >= WAR_START]), function(s) as.data.table(lam(s))),
            idcol = "period")[, `:=`(product = k, horizon_m = 1 / l)]
}))
save_tab(d6, "D6_stock_flow_lambda.csv", "D6_lambda")
for (k in unique(d6$product)) { z <- d6[product == k]
  fact(paste0("lambda_", k), sprintf("λ=%.3f (≈%.1f міс.); до війни %.1f, у війну %.1f міс.", z[period == "all", l], z[period == "all", horizon_m],
                                     z[period == "prewar", horizon_m], z[period == "war", horizon_m])) }
roll <- rbindlist(lapply(names(pairs), function(k) {
  z <- S[, .(month, book = get(k), new = get(pairs[[k]]))][, `:=`(db = book - shift(book), gap = new - shift(book))]
  z[, lambda24 := frollapply(seq_len(.N), 24, function(ix) { s <- z[ix][is.finite(db) & is.finite(gap)]; if (nrow(s) < 18) NA_real_ else sum(s$gap * s$db) / sum(s$gap^2) })]
  z[, .(month, product = k, lambda24)]
}))
save_tab(roll, "D6b_lambda_rolling24.csv", "D6_rolling")
p <- ggplot(roll[is.finite(lambda24)], aes(month, lambda24, colour = PL[product])) + geom_line(linewidth = .8) +
  scale_colour_manual(values = PAL[1:4]) + labs(title = "Швидкість переоцінки портфеля: частка, що переоцінюється за місяць (λ)",
  subtitle = "Ковзне 24-місячне вікно; Δ книжкової = λ·(нова ставка MIR − книжкова t−1)", x = NULL, y = "λ, частка на місяць", caption = CAP) + theme_thesis()
save_fig(p, "D6_lambda_rolling.png")

# ======================= D7. SHAPLEY MIX: MATURITY vs SECTOR ==================
# Mix effect of the 4-cell identity split exactly into a maturity component
# (demand <-> term) and a sector component (HH <-> corporate), averaging the two
# orderings (Shapley). Between-group mix uses group-level rates.
cells <- prod[ccy == "total" & core4 == TRUE, .(month, product, sector, maturity, rate, w = avg_stock)][is.finite(rate) & is.finite(w)]
cells[, s := w / sum(w), by = month]
mix_between <- function(dt, g) {
  z <- dt[, .(s = sum(s), r = sum(s * rate) / sum(s)), by = c("month", g)]
  setorderv(z, c(g, "month")); z[, `:=`(s_l = shift(s), r_l = shift(r), m_l = shift(month)), by = g]
  z[m_l == add_months(month, -1L), .(v = sum((r + r_l) / 2 * (s - s_l))), by = month]
}
setorder(cells, product, month)
cells[, `:=`(s_l = shift(s), r_l = shift(rate), m_l = shift(month)), by = product]
tot_mix <- cells[m_l == add_months(month, -1L), .(mix = sum((rate + r_l) / 2 * (s - s_l))), by = month]
mb_mat <- mix_between(cells, "maturity"); mb_sec <- mix_between(cells, "sector")
d7 <- merge(merge(tot_mix, mb_mat[, .(month, mat_first = v)], by = "month"), mb_sec[, .(month, sec_first = v)], by = "month")
d7[, `:=`(maturity = (mat_first + (mix - sec_first)) / 2, sector = (sec_first + (mix - mat_first)) / 2)]
cyc <- tryCatch(fread(file.path(TAB, "T18_policy_cycles.csv")), error = function(e) data.table())
if (nrow(cyc)) {
  cyc[, `:=`(start = as.Date(start), end = as.Date(end))]
  d7[, cycle := cyc$cycle[findInterval(as.numeric(month), as.numeric(cyc$start))]]
  d7c <- merge(d7[, .(mix = sum(mix), maturity = sum(maturity), sector = sum(sector)), by = cycle], cyc[, .(cycle, regime, start, end, months)], by = "cycle")
  save_tab(d7c, "D7b_shapley_mix_by_cycle.csv", "D7_cycles")
}
save_tab(d7, "D7_shapley_mix_monthly.csv", "D7_monthly")
fact("shapley_mix_total", sprintf("mix %s в.п. = строк %s + сектор %s", p2(sum(d7$mix)), p2(sum(d7$maturity)), p2(sum(d7$sector))))

# ======================= D8. OWNERSHIP GAP: COMPOSITION vs PRICE =============
# c_A − c_B = Σ (s_jA − s_jB) r̄_j  [composition]  +  Σ s̄_j (r_jA − r_jB)  [price]
gp <- rbindlist(lapply(c("corp_demand", "corp_term", "hh_demand", "hh_term"), function(p) {
  bank[!is.na(owner) & is.finite(get(paste0("exp_", p, "_total"))) & get(paste0("avg_dep_", p, "_total")) >= 1,
       .(flow = sum(get(paste0("exp_", p, "_total"))), w = sum(get(paste0("avg_dep_", p, "_total"))), ann = ann[1]), by = .(month, owner)][, product := p]
}))
gp[, `:=`(rate = ann * flow / w, s = w / sum(w)), by = .(month, owner)]
gp[, s := w / sum(w), by = .(month, owner)]
oax <- function(A, B) {
  a <- gp[owner == A, .(month, product, sA = s, rA = rate)]; b <- gp[owner == B, .(month, product, sB = s, rB = rate)]
  z <- merge(a, b, by = c("month", "product"))
  z[, .(gap = sum(sA * rA) - sum(sB * rB), composition = sum((sA - sB) * (rA + rB) / 2), price = sum((sA + sB) / 2 * (rA - rB))), by = month][, pair := paste(A, "−", B)]
}
d8 <- rbind(oax("State", "Private"), oax("Foreign", "Private"), oax("State", "Foreign"))
save_tab(d8, "D8_ownership_gap_decomposition.csv", "D8_ownership")
OWN <- c("State − Private" = "Державні − приватні", "Foreign − Private" = "Іноземні − приватні", "State − Foreign" = "Державні − іноземні")
p <- ggplot(melt(d8[, .(month, pair = OWN[pair], `Склад пасивів` = composition, `Ціна на той самий продукт` = price)], id.vars = c("month", "pair")),
            aes(month, value, fill = variable)) + geom_col(width = 31, position = "stack") + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
  facet_wrap(~pair, ncol = 1) + scale_fill_manual(values = PAL[c(2, 1)]) + scale_y_continuous(labels = label_percent(.5)) +
  labs(title = "Чому державні та іноземні банки платять менше: склад пасивів чи ціна продукту",
       subtitle = "Точна декомпозиція різниці вартості депозитів між групами власності", x = NULL, y = "в.п.", caption = CAP) + theme_thesis()
save_fig(p, "D8_ownership_gap.png", 9, 7)
for (pr in unique(d8$pair)) { z <- d8[pair == pr & month >= WAR_START]
  fact(paste0("oaxaca_", gsub(" − ", "_", pr)), sprintf("розрив %s в.п.: склад %s, ціна %s (середнє з 2022-02)", p2(mean(z$gap)), p2(mean(z$composition)), p2(mean(z$price)))) }

# ======================= EXPORT ===============================================
save_tab(FACTS, "D0_key_facts_dynamics.csv", "Key_facts")
wb <- openxlsx::createWorkbook(); hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#1F4E78", fontColour = "white")
for (nm in names(TABLES)) { sh <- substr(nm, 1, 31); openxlsx::addWorksheet(wb, sh)
  openxlsx::writeData(wb, sh, as.data.frame(TABLES[[nm]]), headerStyle = hs, withFilter = TRUE); openxlsx::freezePane(wb, sh, firstRow = TRUE) }
openxlsx::saveWorkbook(wb, file.path(OUT, "Section_2_1_v2_dynamics.xlsx"), overwrite = TRUE)
print(FACTS)
log_msg("DEPOSIT DYNAMICS COMPLETE: ", length(list.files(FIG, pattern = "^D")), " figures, ", length(TABLES), " tables")
