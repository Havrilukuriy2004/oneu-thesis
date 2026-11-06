# ============================================================================
# 09_expectations_money_v6.R — ОЧІКУВАННЯ ЕКОНОМІЧНИХ АГЕНТІВ, ПОВНА СТАТИСТИКА
# ПРОЦЕНТНИХ СТАВОК НБУ І ГРОШОВА СТАТИСТИКА (доповнення до 2.1 і 2.2)
# ----------------------------------------------------------------------------
# Reads only marts (02) and the CSV of 03/07. Output: output_nbu/expectations_money/
#   tables/EX21_* (descriptive, for 2.1), tables/EX22_* (econometrics, for 2.2),
#   figures/EX*, Expectations_money.xlsx
#
# Why expectations belong in a model of bank funding cost (identification logic)
#  * Depositors compare the UAH deposit rate with (i) expected inflation — the real
#    return (Fisher), and (ii) expected depreciation — the return on FX deposits (UIP for
#    households). A bank that wants to keep UAH term funding must pay a premium that moves
#    with these expectations, not only with the policy rate. Omitting them biases the
#    long-run pass-through θ (the policy rate itself reacts to expected inflation).
#  * Banks and markets price on the EXPECTED policy path: an anticipated change should be
#    in deposit/loan rates before or at the decision, a surprise only after it. Splitting
#    Δi into anticipated and surprise parts (07, P13a) tests forward-looking pricing.
#  * Loan volumes fall after a hike both because banks cut supply and because firms demand
#    less. Bank Lending Survey (demand, standards) and business expectations (IODA) separate
#    the two (bank lending channel vs demand).
#  * Monetary aggregates show how transmission ends up in money holdings: the mix between
#    cash, UAH transferable/term deposits and FX deposits (dollarisation).
#
# Blocks
#  EX21_E1  expectations, real ex-ante rates, expected depreciation, UIP gap — by policy regime
#  EX21_E2  full NBU rate statistics (UAH, new vs outstanding, by sector/maturity) — by regime;
#           stock–flow repricing speed of official outstanding rates
#  EX21_E3  monetary aggregates: growth, dollarisation, term share, multiplier; consistency of
#           bank-panel deposits with monetary statistics
#  EX22_P13b expectations-augmented long-run pass-through (DOLS, Stock–Watson 1993) + ECM speed
#  EX22_P13c anticipation in daily posted deposit rates (UIRD) around NBU decisions
#  EX22_P13d currency substitution: dollarisation vs UIP excess return and expected depreciation
#  EX22_P13e credit supply vs demand: loans response with/without BLS demand & IODA; BLS standards
#  EX22_P13f Markov regimes (07, P4) vs expectations
#  EX22_P13g monetary transmission: LP of real money, dollarisation, term share, multiplier
# Time-series inference: Newey–West (lag = h + 1 for LP, 6 for levels/DOLS).
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_models_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_local_inputs_v6.R"), encoding = "UTF-8")
.need(c("sandwich"))
init_output("EX", folder = "expectations_money")
CAP <- "Джерело: розрахунки автора за даними НБУ (статистика процентних ставок, грошова статистика, опитування очікувань, BLS)."
t0_all <- Sys.time()
col_or_na <- function(dt, v) if (v %in% names(dt)) dt[[v]] else rep(NA_real_, nrow(dt))
blk <- function(name, expr) { t0 <- Sys.time()
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) { status(name, "error", conditionMessage(e)); message("  [", name, "] ", conditionMessage(e)); FALSE })
  if (ok) status(name, "ok", sprintf("%.1f с", as.numeric(difftime(Sys.time(), t0, units = "secs")))) }
nw_tab <- function(f, lag) { V <- sandwich::NeweyWest(f, lag = lag, prewhite = FALSE, adjust = TRUE)
  data.table(term = names(coef(f)), b = coef(f), se = sqrt(diag(V)))[, p := 2 * pnorm(-abs(b / se))][] }

# ------------------------------ data -----------------------------------------
S <- store_read("mart_system_month_v6"); S[, month := as.Date(month)]; setorder(S, month)
A <- store_read("mart_system_assets_v6"); if (nrow(A)) { A[, month := as.Date(month)]; S <- merge(S, A[, intersect(c("month", "loans_le", "loans_hh"), names(A)), with = FALSE], by = "month", all.x = TRUE, suffixes = c("", "_a")) }
pp <- function(v) 100 * col_or_na(S, v)                                       # shares -> % / п.п.
M <- S[, .(month, i = pp("policy_avg"), cpi = pp("cpi_yoy"), cpi_m = col_or_na(S, "cpi_mom"), usd = col_or_na(S, "usd_avg"),
           cod4 = pp("cod4_total"), uird3m = pp("uird_uah_3m_avg"), uird12m = pp("uird_uah_12m_avg"),
           dep_hh_1y = pp("fm_dep_hh_uah_1y"), dep_hh_dem = pp("fm_dep_hh_uah_dem"), dep_hh_2y = pp("fm_dep_hh_uah_2y"), dep_hh_uah = pp("fm_dep_hh_uah"),
           dep_hh_fx1y = pp("fm_dep_hh_fx_1y"), dep_nfc_1y = pp("fm_dep_nfc_uah_1y"), dep_nfc_dem = pp("fm_dep_nfc_uah_dem"),
           loan_nfc = pp("fm_loan_nfc_uah_xo"), loan_nfc_all = pp("fm_loan_nfc_uah"), loan_hh = pp("fm_loan_hh_uah_xo"), loan_hh_all = pp("fm_loan_hh_uah"),
           out_dep_hh = pp("fm_out_dep_hh_uah"), out_dep_hh_dem = pp("fm_out_dep_hh_uah_dem"), out_dep_nfc = pp("fm_out_dep_nfc"),
           out_loan_nfc = pp("fm_out_loan_nfc"), out_loan_hh = pp("fm_out_loan_hh_uah"), ib = pp("fm_ib_uah"), ovdp = pp("fm_ovdp_prim"),
           pie_hh = pp("pie_hh"), pie_firms = pp("pie_firms"), pie_banks = pp("pie_banks"), pie_an = pp("pie_analysts"), pie_mean = pp("pie_mean"),
           edep_hh = pp("edep_hh"), edep_an = pp("edep_analysts"), edep_mean = pp("edep_mean"),
           rr_policy = pp("rr_policy"), rr_dep_hh = pp("rr_dep_hh"), uip_hh = pp("uip_hh"), xp1y = pp("exp_path_1y"), xp3m = pp("exp_path_3m"),
           ioda = col_or_na(S, "ioda"), bls_dem_nfc = col_or_na(S, "bls_dem_nfc"), bls_std_nfc = col_or_na(S, "bls_std_nfc"),
           bls_dem_hh = col_or_na(S, "bls_dem_hh"), bls_std_hh = col_or_na(S, "bls_std_hh"), bls_exp_dep = col_or_na(S, "bls_exp_dep"),
           m0 = col_or_na(S, "mon_m0"), m1 = col_or_na(S, "mon_m1"), m2 = col_or_na(S, "mon_m2"), m3 = col_or_na(S, "mon_m3"),
           dollar = pp("mon_dollar"), hh_dollar = pp("mon_hh_dollar"), hh_term_uah = pp("mon_hh_term_share_uah"), mult = col_or_na(S, "mon_mult"),
           cash_share = pp("mon_cash_share"), mon_dep_hh = col_or_na(S, "mon_hh_dep_uah") + col_or_na(S, "mon_hh_dep_fx"),
           mon_dep_nfc = col_or_na(S, "mon_nfc_trans_uah") + col_or_na(S, "mon_nfc_oth_uah") + col_or_na(S, "mon_nfc_trans_fx") + col_or_na(S, "mon_nfc_oth_fx"),
           loans_le = col_or_na(S, "loans_le"))]
dcols <- grep("^dep_(hh|corp)_(demand|term)_(uah|fx)$", names(S), value = TRUE)
M[, panel_dep := if (length(dcols)) rowSums(S[, ..dcols]) else NA_real_]
for (v in c("pie_hh", "pie_firms", "pie_banks", "pie_an", "pie_mean", "edep_hh", "edep_an", "edep_mean", "ioda", "bls_dem_nfc", "bls_std_nfc"))
  M[, (paste0(v, "_f")) := nafill(get(v), "locf")]                           # carried reading (for regressions with lags)
M[, cpi_ix := { x <- fcoalesce(cpi_m, 0); cumprod(1 + x) }]
M[, `:=`(di = i - shift(i), dl_usd = 100 * (log(usd) - log(shift(usd))))]
sur <- tryCatch(fread(file.path(OUTPUT_DIR, "section_2_2", "tables", "S22_P13a_policy_surprises_monthly.csv")), error = function(e) NULL)
if (!is.null(sur) && nrow(sur)) { sur[, month := as.Date(month)]; M <- merge(M, sur[, .(month, di_ant, di_sur)], by = "month", all.x = TRUE) } else M[, `:=`(di_ant = NA_real_, di_sur = NA_real_)]
setorder(M, month)
cyc <- tryCatch(fread(file.path(OUTPUT_DIR, "section_2_1_v2", "tables", "T18_policy_cycles.csv")), error = function(e) data.table())
REG_UA <- c("Tightening" = "Посилення", "Easing" = "Пом'якшення", "High-rate plateau" = "Плато високої ставки", "Stable/low-rate" = "Стабільна / низька ставка")
if (nrow(cyc)) { cyc[, `:=`(start = as.Date(start), end = as.Date(end))]; M[, regime := cyc$regime[findInterval(as.numeric(month), as.numeric(cyc$start))]] } else M[, regime := NA_character_]
M[, regime_ua := fifelse(is.na(REG_UA[regime]), regime, REG_UA[regime])]
M[, period := fcase(month < as.Date("2022-03-01"), "До війни (2018–2022-02)", month < as.Date("2023-04-01"), "Війна, до режиму ДС (2022-03–2023-03)", default = "Режим 3М ДС (з 2023-04)")]
M <- M[month >= START_MONTH]
fact("EX_sample", sprintf("%s … %s, %d міс.", min(M$month), max(M$month), nrow(M)))
fact("EX_coverage", paste(sprintf("%s: %d", c("π^e ФО", "π^e аналітики", "Δs^e ФО", "ставки НБУ (нові ФО ≤1р)", "M3", "BLS", "IODA"),
     c(sum(is.finite(M$pie_hh)), sum(is.finite(M$pie_an)), sum(is.finite(M$edep_hh)), sum(is.finite(M$dep_hh_1y)), sum(is.finite(M$m3)), sum(is.finite(M$bls_dem_nfc)), sum(is.finite(M$ioda)))), collapse = "; "))

by_group <- function(vars, labs, grp = "regime_ua") {
  z <- M[!is.na(get(grp)) & nzchar(get(grp)), c(list(`Місяців` = .N), lapply(.SD, function(x) mean(x, na.rm = TRUE))), by = grp, .SDcols = vars]
  all <- M[, c(list(`Місяців` = .N), lapply(.SD, function(x) mean(x, na.rm = TRUE))), .SDcols = vars][, (grp) := "Уся вибірка"]
  z <- rbind(z, all, use.names = TRUE); setnames(z, c(grp, vars), c("Група", labs)); z[] }

# ============================== EX21 (2.1, descriptive) =========================
blk("E1 очікування за режимами", {
  v <- c("i", "cpi", "pie_hh", "pie_firms", "pie_banks", "pie_an", "rr_policy", "rr_dep_hh", "edep_hh", "edep_an", "uip_hh", "xp1y", "ioda")
  lab <- c("Облікова ставка", "ІСЦ, р/р", "π^e домогосподарств", "π^e підприємств", "π^e банків", "π^e фін. аналітиків", "Реальна облікова (ex ante)",
           "Реальна ставка депозитів ФО ≤1 р. (ex ante)", "Очікувана девальвація: ФО", "Очікувана девальвація: аналітики", "Надлишкова дохідність грн депозиту (UIP)",
           "Очікувана зміна облікової (крива 1 р.)", "ІОДА")
  save_tab(by_group(v, lab), "EX21_E1_expectations_by_regime.csv", "E1_by_regime")
  save_tab(by_group(v, lab, "period"), "EX21_E1b_expectations_by_period.csv", "E1b_by_period")
  M[, yr := format(month, "%Y")]; save_tab(by_group(v, lab, "yr"), "EX21_E1c_expectations_by_year.csv", "E1c_by_year")
  # anchoring: dispersion across groups and sensitivity of expectations to realised inflation
  M[, pie_disp := apply(cbind(pie_hh_f, pie_firms_f, pie_banks_f, pie_an_f), 1, function(x) if (sum(is.finite(x)) >= 3) sd(x, na.rm = TRUE) else NA_real_)]
  anc <- rbindlist(lapply(names(c(pie_hh = 1, pie_firms = 1, pie_banks = 1, pie_an = 1)), function(g) {
    z <- M[is.finite(get(g)) & is.finite(cpi)]; if (nrow(z) < 20) return(NULL)
    f <- lm(reformulate(c("cpi", "i"), g), data = z); r <- nw_tab(f, 3)[term != "(Intercept)"]
    r[, `:=`(group = g, n = nrow(z), r2 = summary(f)$r.squared, mean_gap = mean(z[[g]] - z$cpi))] }))
  anc[, group := c(pie_hh = "Домогосподарства", pie_firms = "Підприємства", pie_banks = "Банки", pie_an = "Фінансові аналітики")[group]]
  save_tab(anc, "EX21_E1d_expectations_anchoring.csv", "E1d_anchoring")
  fact("E1_disp", sprintf("середнє міжгрупове с.в. інфляційних очікувань %.2f в.п.; ФО − аналітики в середньому %+.2f в.п.",
       mean(M$pie_disp, na.rm = TRUE), mean(M$pie_hh_f - M$pie_an_f, na.rm = TRUE)))
  L <- melt(M[, .(month, `Домогосподарства` = pie_hh_f, `Підприємства` = pie_firms_f, `Банки` = pie_banks_f, `Фінансові аналітики` = pie_an_f, `Фактична інфляція (ІСЦ, р/р)` = cpi)],
            id.vars = "month", variable.name = "s")[is.finite(value)]
  p <- ggplot(L, aes(month, value, colour = s, linetype = s)) + geom_line(linewidth = .6) +
    labs(title = "Інфляційні очікування на 12 місяців і фактична інфляція", x = NULL, y = "%", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "EX21_E1_inflation_expectations.png", 8.5, 4.6)
  L <- melt(M[, .(month, `Облікова ставка` = i, `Реальна облікова (ex ante, аналітики)` = rr_policy, `Реальна ставка депозитів ФО ≤1 р. (ex ante, ФО)` = rr_dep_hh)],
            id.vars = "month", variable.name = "s")[is.finite(value)]
  p <- ggplot(L, aes(month, value, colour = s, linetype = s)) + geom_hline(yintercept = 0, linewidth = .3) + geom_line(linewidth = .6) +
    labs(title = "Номінальні та реальні (ex ante) ставки", x = NULL, y = "%", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "EX21_E2_real_rates.png", 8.5, 4.6)
  L <- melt(M[, .(month, `Очікувана девальвація (ФО)` = edep_hh_f, `Очікувана девальвація (аналітики)` = edep_an_f,
                  `Спред грн − валюта, депозити ФО ≤1 р.` = dep_hh_1y - dep_hh_fx1y, `Надлишкова дохідність грн депозиту` = uip_hh)], id.vars = "month", variable.name = "s")[is.finite(value)]
  p <- ggplot(L, aes(month, value, colour = s, linetype = s)) + geom_hline(yintercept = 0, linewidth = .3) + geom_line(linewidth = .6) +
    labs(title = "Курсові очікування і привабливість гривневих депозитів", x = NULL, y = "%, в.п.", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "EX21_E3_fx_expectations_uip.png", 8.5, 4.6)
})

blk("E2 ставки НБУ (нові vs непогашені)", {
  v <- c("i", "dep_hh_dem", "dep_hh_1y", "dep_hh_2y", "out_dep_hh", "dep_hh_fx1y", "dep_nfc_dem", "dep_nfc_1y", "out_dep_nfc",
         "loan_nfc", "out_loan_nfc", "loan_hh", "out_loan_hh", "ib", "ovdp")
  lab <- c("Облікова ставка", "Нові депозити ФО на вимогу, грн", "Нові депозити ФО до 1 р., грн", "Нові депозити ФО 1–2 р., грн", "Непогашені депозити ФО, грн",
           "Нові депозити ФО до 1 р., валюта", "Нові депозити СГ на вимогу, грн", "Нові депозити СГ до 1 р., грн", "Непогашені депозити СГ",
           "Нові кредити СГ, грн (без овердрафту)", "Непогашені кредити СГ", "Нові кредити ФО, грн (без овердрафту)", "Непогашені кредити ФО, грн",
           "Міжбанк, грн", "ОВДП, первинний ринок")
  save_tab(by_group(v, lab), "EX21_E2_nbu_rates_by_regime.csv", "E2_rates_by_regime")
  # stock–flow: book_t − book_{t−1} = λ (new_t − book_{t−1}) — share of the book repriced per month
  sf <- function(book, new, nm, lab) rbindlist(lapply(c("all", "prewar", "war"), function(pr) {
    z <- M[, .(month, b = get(book), n = get(new))][, `:=`(db = b - shift(b), gap = n - shift(b))]
    z <- z[is.finite(db) & is.finite(gap)]
    z <- switch(pr, all = z, prewar = z[month < as.Date("2022-03-01")], war = z[month >= as.Date("2022-03-01")])
    if (nrow(z) < 18) return(NULL)
    f <- lm(db ~ 0 + gap, data = z); r <- nw_tab(f, 6)
    data.table(series = nm, label = lab, period = pr, lambda = r$b, se = r$se, n = nrow(z), horizon_m = 1 / r$b) }))
  SF <- rbind(sf("out_dep_hh", "dep_hh_uah", "dep_hh", "Депозити ФО, грн"), sf("out_loan_nfc", "loan_nfc_all", "loan_nfc", "Кредити СГ"),
              sf("out_loan_hh", "loan_hh_all", "loan_hh", "Кредити ФО, грн"), sf("out_dep_nfc", "dep_nfc_1y", "dep_nfc", "Депозити СГ (нові до 1 р. → непогашені)"))
  save_tab(SF, "EX21_E2b_stock_flow_official_rates.csv", "E2b_stock_flow")
  L <- melt(M[, .(month, `Облікова ставка` = i, `Нові депозити ФО до 1 р.` = dep_hh_1y, `Непогашені депозити ФО` = out_dep_hh,
                  `Нові кредити СГ` = loan_nfc, `Непогашені кредити СГ` = out_loan_nfc)], id.vars = "month", variable.name = "s")[is.finite(value)]
  p <- ggplot(L, aes(month, value, colour = s, linetype = s)) + geom_line(linewidth = .6) +
    labs(title = "Ставки за новими та непогашеними операціями (гривня), статистика НБУ", x = NULL, y = "%", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "EX21_E4_new_vs_outstanding.png", 8.5, 4.8)
})

blk("E3 грошова статистика", {
  if (!any(is.finite(M$m3))) stop("немає грошових агрегатів (raw_monetary)")
  M[, `:=`(g_m0 = 100 * (log(m0) - log(shift(m0, 12))), g_m1 = 100 * (log(m1) - log(shift(m1, 12))), g_m3 = 100 * (log(m3) - log(shift(m3, 12))),
           g_m3_real = 100 * (log(m3 / cpi_ix) - log(shift(m3 / cpi_ix, 12))))]
  v <- c("g_m0", "g_m1", "g_m3", "g_m3_real", "dollar", "hh_dollar", "hh_term_uah", "cash_share", "mult")
  lab <- c("M0, % р/р", "M1, % р/р", "M3, % р/р", "M3 реальний, % р/р", "Доларизація депозитів у M3, %", "Доларизація депозитів ФО, %",
           "Частка строкових у грн депозитах ФО, %", "Готівка (M0) / M3, %", "Грошовий мультиплікатор M3/M0")
  save_tab(by_group(v, lab), "EX21_E3_money_by_regime.csv", "E3_money_by_regime")
  # consistency: deposits in the bank panel (OSB) vs monetary statistics (households + NFC, all currencies)
  z <- M[is.finite(panel_dep) & is.finite(mon_dep_hh) & is.finite(mon_dep_nfc) & panel_dep > 0]
  if (nrow(z) > 12) {
    z[, mon := mon_dep_hh + mon_dep_nfc]; z[, `:=`(ratio = panel_dep / mon, g_p = 100 * (log(panel_dep) - log(shift(panel_dep))), g_m = 100 * (log(mon) - log(shift(mon))))]
    save_tab(data.table(months = nrow(z), ratio_mean = mean(z$ratio), ratio_min = min(z$ratio), ratio_max = max(z$ratio),
                        corr_mom_growth = cor(z$g_p, z$g_m, use = "complete.obs")), "EX21_E3b_panel_vs_monetary_deposits.csv", "E3b_consistency")
    fact("E3_consistency", sprintf("депозити панелі / депозити в грошовій статистиці: %.2f (%.2f…%.2f); кореляція м/м темпів %.2f",
         mean(z$ratio), min(z$ratio), max(z$ratio), cor(z$g_p, z$g_m, use = "complete.obs")))
  }
  L <- melt(M[, .(month, `Доларизація депозитів у M3` = dollar, `Доларизація депозитів ФО` = hh_dollar, `Строкові у грн депозитах ФО` = hh_term_uah,
                  `Реальний M3, % р/р` = g_m3_real)], id.vars = "month", variable.name = "s")[is.finite(value)]
  p <- ggplot(L, aes(month, value, colour = s, linetype = s)) + geom_hline(yintercept = 0, linewidth = .3) + geom_line(linewidth = .6) +
    labs(title = "Грошова статистика: структура депозитів і реальна грошова маса", x = NULL, y = "%", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "EX21_E5_money.png", 8.5, 4.6)
})

# ============================== EX22 (2.2, econometrics) =========================
# time-series local projection: y_{t+h} − y_{t−1} = β x_t + Σ_{l=1..L}(Δy_{t−l}, x_{t−l}) + controls + ε, NW(h+1)
ts_lp <- function(D, y, x, ctrl = character(), H = 12, L = 3) {
  d <- copy(D[, unique(c("month", y, x, ctrl)), with = FALSE]); setnames(d, c(y, x), c(".y", ".x"))
  d[, .dy := .y - shift(.y)]; lc <- character()
  for (l in seq_len(L)) { d[, (paste0(".ldy", l)) := shift(.dy, l)]; d[, (paste0(".lx", l)) := shift(.x, l)]; lc <- c(lc, paste0(".ldy", l), paste0(".lx", l)) }
  rbindlist(lapply(0:H, function(h) {
    d[, .lhs := shift(.y, h, type = "lead") - shift(.y, 1)]
    z <- d[complete.cases(d[, c(".lhs", ".x", lc, ctrl), with = FALSE])]; if (nrow(z) < 30) return(NULL)
    f <- lm(reformulate(c(".x", lc, ctrl), ".lhs"), data = z); r <- nw_tab(f, h + 1)[term == ".x"]
    r[, `:=`(h = h, n = nrow(z))] }), fill = TRUE)
}

blk("P13b DOLS з очікуваннями", {
  SER <- list(cod4 = c("Вартість депозитів (книжкова, панель)", "hh"), out_dep_hh = c("Непогашені депозити ФО, грн", "hh"),
              dep_hh_1y = c("Нові депозити ФО до 1 р., грн", "hh"), uird3m = c("UIRD 3М (оголошені ставки ФО)", "hh"),
              dep_nfc_1y = c("Нові депозити СГ до 1 р., грн", "nfc"), loan_nfc = c("Нові кредити СГ, грн", "nfc"))
  SER <- SER[names(SER) %in% names(M)]
  dols <- function(y, xs, k = 2) {
    d <- M[, c("month", y, xs), with = FALSE]; setnames(d, y, ".y"); lead_lag <- character()
    for (x in xs) { d[, (paste0("d_", x)) := get(x) - shift(get(x))]
      for (l in -k:k) { nm <- paste0("dd_", x, "_", l + k)
        d[, (nm) := if (l < 0) shift(get(paste0("d_", x)), -l, type = "lead") else shift(get(paste0("d_", x)), l)]; lead_lag <- c(lead_lag, nm) } }
    z <- d[complete.cases(d[, c(".y", xs, lead_lag), with = FALSE])]; if (nrow(z) < 36) return(NULL)
    f <- lm(reformulate(c(xs, lead_lag), ".y"), data = z); r <- nw_tab(f, 6)[term %in% xs]
    # Engle–Granger ECM on the long-run residual: Δy = c + φ u_{t−1} + β Δi + ρ Δy_{t−1}
    z[, u := .y - as.vector(as.matrix(cbind(1, z[, ..xs])) %*% coef(f)[c("(Intercept)", xs)])]
    z[, `:=`(dy = .y - shift(.y), ul = shift(u), di_ = i - shift(i))]; z[, dyl := shift(dy)]
    e <- tryCatch(nw_tab(lm(dy ~ ul + di_ + dyl, data = z), 3)[term == "ul"], error = function(e) NULL)
    r[, `:=`(n = nrow(z), r2 = summary(f)$r.squared, phi = e$b %||% NA_real_, phi_se = e$se %||% NA_real_)][] }
  R <- rbindlist(lapply(names(SER), function(y) {
    ex <- if (SER[[y]][2] == "hh") c("pie_hh_f", "edep_hh_f") else c("pie_mean_f", "edep_mean_f")
    rbindlist(list(dols(y, "i")[, model := "(1) лише облікова"], dols(y, c("i", ex[1]))[, model := "(2) + інфляційні очікування"],
                   dols(y, c("i", ex))[, model := "(3) + інфляційні і курсові очікування"]), fill = TRUE)[, `:=`(key = y, label = SER[[y]][1])] }), fill = TRUE)
  R[, term_lab := fifelse(term == "i", "θ: облікова ставка", fifelse(grepl("^pie", term), "γ: інфляційні очікування", fifelse(grepl("^edep", term), "δ: очікувана девальвація", term)))]
  save_tab(R, "EX22_P13b_dols_expectations.csv", "P13b_DOLS")
  for (y in unique(R$key)) { a <- R[key == y & term == "i"]; if (nrow(a) >= 2) fact(paste0("P13b_", y), sprintf("θ: %s", paste(sprintf("%s %.2f (%.2f)", substr(a$model, 1, 3), a$b, a$se), collapse = "; "))) }
})

blk("P13c очікувані рішення і UIRD (денні)", {
  ev <- tryCatch(fread(file.path(OUTPUT_DIR, "section_2_2", "tables", "S22_P11a_nbu_rate_changes.csv")), error = function(e) NULL)
  UD <- tryCatch(read_uird_local(), error = function(e) NULL)
  if (is.null(ev) || is.null(UD)) stop("немає подій 07/P11 або UIRD")
  ev[, `:=`(date = as.Date(date), next_date = as.Date(next_date), m_prev = add_months(month_floor(as.Date(date)), -1L))]
  ev <- merge(ev, M[, .(m_prev = month, xp3m, pie_an_f, i_prev = i)], by = "m_prev", all.x = TRUE)
  # market expectation of the decision: fitted from the 3M curve slope at the end of the previous month
  fe <- lm(dr ~ xp3m, data = ev[is.finite(xp3m)]); ev[is.finite(xp3m), `:=`(dr_exp = predict(fe, newdata = .SD), dr_sur = dr - predict(fe, newdata = .SD))]
  ud <- UD$daily[ccy == "uah" & tenor %in% c("3m", "12m")]; setorder(ud, tenor, date)
  win <- function(tn, d0, nxt) { u <- ud[tenor == tn]; p0 <- which(u$date >= d0)[1]; if (is.na(p0) || p0 <= 25) return(c(NA, NA))
    base <- u$value[p0 - 2L]; pre <- base - u$value[p0 - 22L]; pe <- min(p0 + 20L, nrow(u)); if (!is.na(nxt)) pe <- min(pe, max(which(u$date < nxt)))
    c(pre, u$value[pe] - base) }
  W <- rbindlist(lapply(seq_len(nrow(ev)), function(e) rbindlist(lapply(c("3m", "12m"), function(tn) { w <- win(tn, ev$date[e], ev$next_date[e])
    data.table(event = e, date = ev$date[e], dr = ev$dr[e], dr_exp = ev$dr_exp[e], dr_sur = ev$dr_sur[e], tenor = tn, pre = w[1], post = w[2]) }))))
  W <- W[is.finite(pre) & is.finite(post) & is.finite(dr_exp)]
  if (nrow(W) < 12) stop("замало подій з повними вікнами")
  R <- rbindlist(lapply(c("3m", "12m"), function(tn) rbindlist(lapply(c("pre", "post"), function(w) {
    f <- lm(reformulate(c("dr_exp", "dr_sur"), w), data = W[tenor == tn]); V <- sandwich::vcovHC(f, type = "HC1")
    data.table(tenor = tn, window = w, term = names(coef(f)), b = coef(f), se = sqrt(diag(V)), n = nobs(f), r2 = summary(f)$r.squared) }))))
  R[, p := 2 * pnorm(-abs(b / se))]
  R[, window_lab := fifelse(window == "pre", "До рішення: [−22; −2] роб. дн.", "Після рішення: [−2; +20] роб. дн.")]
  save_tab(R, "EX22_P13c_anticipation_daily_uird.csv", "P13c_anticipation")
  save_tab(data.table(term = names(coef(fe)), b = coef(fe), se = sqrt(diag(sandwich::vcovHC(fe, type = "HC1"))), n = nobs(fe), r2 = summary(fe)$r.squared),
           "EX22_P13c_decision_predictability.csv", "P13c_predictability")
  save_tab(W, "EX22_P13c_event_windows.csv", "P13c_events")
  a <- R[tenor == "3m" & term == "dr_exp"]; s <- R[tenor == "3m" & term == "dr_sur"]
  fact("P13c_uird3m", sprintf("очікувана частина рішення: до %.2f, після %.2f; несподівана: до %.2f, після %.2f (в.п. UIRD 3М на 1 в.п.)",
       a$b[a$window == "pre"], a$b[a$window == "post"], s$b[s$window == "pre"], s$b[s$window == "post"]))
})

blk("P13d доларизація і UIP", {
  M[, `:=`(d_hh_dollar = hh_dollar - shift(hh_dollar), d_dollar = dollar - shift(dollar), d_edep = edep_hh_f - shift(edep_hh_f))]
  M[, `:=`(uip_l = shift(uip_hh), d_hh_dollar_l = shift(d_hh_dollar), d_dollar_l = shift(d_dollar))]
  R <- rbindlist(lapply(c("d_hh_dollar", "d_dollar"), function(y) {
    z <- M[complete.cases(M[, c(y, "uip_l", "d_edep", "di", "dl_usd", paste0(y, "_l")), with = FALSE])]; if (nrow(z) < 30) return(NULL)
    f <- lm(reformulate(c("uip_l", "d_edep", "di", "dl_usd", paste0(y, "_l")), y), data = z)
    nw_tab(f, 3)[, `:=`(dep = y, n = nrow(z), r2 = summary(f)$r.squared)] }), fill = TRUE)
  R[, term_lab := c("(Intercept)" = "Константа", uip_l = "Надлишкова дохідність грн депозиту (t−1)", d_edep = "Δ очікуваної девальвації (ФО)",
                    di = "Δ облікової ставки", dl_usd = "Δ log курсу (переоцінка)", d_hh_dollar_l = "Залежна змінна (t−1)", d_dollar_l = "Залежна змінна (t−1)")[term]]
  R[, dep_lab := fifelse(dep == "d_hh_dollar", "Δ доларизації депозитів ФО, в.п.", "Δ доларизації депозитів у M3, в.п.")]
  save_tab(R, "EX22_P13d_dollarisation_uip.csv", "P13d_dollarisation")
  LPd <- rbindlist(list(ts_lp(M, "hh_dollar", "di", c("dl_usd"))[, shock := "Δi"],
                        if (any(is.finite(M$di_sur))) ts_lp(M, "hh_dollar", "di_sur", c("dl_usd"))[, shock := "Несподівана Δi"]), fill = TRUE)
  save_tab(LPd, "EX22_P13d_lp_dollarisation.csv", "P13d_LP")
})

blk("P13e пропозиція vs попит на кредит", {
  if (!any(is.finite(M$loans_le))) stop("немає системного обсягу кредитів СГ")
  M[, `:=`(lq = 100 * log(loans_le / cpi_ix), dem_l = shift(bls_dem_nfc_f, 3), ioda_l = shift(ioda_f), std_l = shift(bls_std_nfc_f, 3))]
  base <- ts_lp(M, "lq", "di", c("dl_usd"))[, spec := "(1) базова"]
  dem <- ts_lp(M, "lq", "di", c("dl_usd", "dem_l"))[, spec := "(2) + попит на кредит (BLS)"]
  dio <- ts_lp(M, "lq", "di", c("dl_usd", "dem_l", "ioda_l"))[, spec := "(3) + попит (BLS) і ІОДА"]
  LPq <- rbindlist(list(base, dem, dio), fill = TRUE)
  save_tab(LPq, "EX22_P13e_lp_loans_demand_controls.csv", "P13e_LP_loans")
  # BLS credit standards: tightening after policy hikes, controlling for demand and expectations (quarterly)
  Q <- M[, .(std = mean(bls_std_nfc, na.rm = TRUE), dem = mean(bls_dem_nfc, na.rm = TRUE), di_q = sum(di, na.rm = TRUE),
             pie = mean(pie_mean_f, na.rm = TRUE), edep = mean(edep_mean_f, na.rm = TRUE)), by = .(q = paste(year(month), quarter(month)))]
  Q[, `:=`(di_l = shift(di_q), std_l = shift(std))]
  z <- Q[is.finite(std) & is.finite(di_l) & is.finite(dem) & is.finite(pie) & is.finite(edep) & is.finite(std_l)]
  if (nrow(z) >= 16) { f <- lm(std ~ di_q + di_l + dem + pie + edep + std_l, data = z)
    save_tab(nw_tab(f, 2)[, `:=`(n = nrow(z), r2 = summary(f)$r.squared)], "EX22_P13e_bls_standards.csv", "P13e_BLS_standards") }
  a <- LPq[h == 12]; fact("P13e_loans_b12", paste(sprintf("%s: %.2f (%.2f)", a$spec, a$b, a$se), collapse = "; "))
})

blk("P13f режими Маркова і очікування", {
  pr <- tryCatch(fread(file.path(OUTPUT_DIR, "section_2_2", "tables", "S22_P4d_markov_regime_probabilities.csv")), error = function(e) NULL)
  if (is.null(pr) || !nrow(pr)) stop("немає ймовірностей режимів (07/P4)")
  pr[, month := as.Date(month)]
  X <- M[, .(month, pie_l = shift(pie_hh_f), edep_l = shift(edep_hh_f), i_l = shift(i), xp_l = shift(xp1y))]
  R <- rbindlist(lapply(unique(pr[model == "A2"]$series), function(sv) {
    z <- merge(pr[model == "A2" & series == sv, .(month, p = 100 * p_high, label)], X, by = "month")
    z <- z[complete.cases(z[, .(p, pie_l, edep_l, i_l, xp_l)])]; if (nrow(z) < 30) return(NULL)
    f <- lm(p ~ pie_l + edep_l + i_l + xp_l, data = z); nw_tab(f, 6)[, `:=`(series = sv, label = z$label[1], n = nrow(z), r2 = summary(f)$r.squared)] }), fill = TRUE)
  R[, term_lab := c("(Intercept)" = "Константа", pie_l = "Інфляційні очікування ФО (t−1)", edep_l = "Очікувана девальвація ФО (t−1)",
                    i_l = "Облікова ставка (t−1)", xp_l = "Очікувана зміна облікової (крива, t−1)")[term]]
  save_tab(R, "EX22_P13f_markov_regimes_vs_expectations.csv", "P13f_MS_vs_exp")
})

blk("P13g грошова трансмісія", {
  if (!any(is.finite(M$m3))) stop("немає грошових агрегатів")
  M[, `:=`(lm1 = 100 * log(m1 / cpi_ix), lm3 = 100 * log(m3 / cpi_ix), lm0 = 100 * log(m0 / cpi_ix))]
  Y <- c(lm0 = "Реальний M0", lm1 = "Реальний M1", lm3 = "Реальний M3", dollar = "Доларизація депозитів у M3", hh_term_uah = "Частка строкових у грн депозитах ФО", mult = "Мультиплікатор M3/M0")
  R <- rbindlist(lapply(names(Y), function(y) rbindlist(list(ts_lp(M, y, "di", c("dl_usd"))[, shock := "Δi"],
                   if (any(is.finite(M$di_sur))) ts_lp(M, y, "di_sur", c("dl_usd"))[, shock := "Несподівана Δi"]), fill = TRUE)[, `:=`(key = y, label = Y[[y]])]), fill = TRUE)
  save_tab(R, "EX22_P13g_lp_money.csv", "P13g_LP_money")
  p <- ggplot(R[shock == "Δi"], aes(h, b)) + geom_hline(yintercept = 0, linewidth = .3) +
    geom_ribbon(aes(ymin = b - 1.645 * se, ymax = b + 1.645 * se), fill = "grey80", colour = NA) + geom_line(linewidth = .7) +
    facet_wrap(~label, ncol = 3, scales = "free_y") +
    labs(title = "Реакція грошових агрегатів і структури депозитів на облікову ставку", x = "Горизонт, місяців", y = "%, в.п. на 1 в.п.", caption = CAP) + theme_thesis()
  save_fig(p, "EX22_P13g_money_lp.png", 8.5, 5.8)
})

# ============================== EXPORT ========================================
save_tab(STATUS, "EX_00_blocks_status.csv", "Status"); save_tab(FACTS, "EX_00_key_facts.csv", "Key_facts")
write_workbook("Expectations_money.xlsx")
print(STATUS); print(FACTS)
log_msg("09 COMPLETE: ", length(TABLES), " tables, ", sprintf("%.1f хв", as.numeric(difftime(Sys.time(), t0_all, units = "mins"))))
