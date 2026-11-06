# ============================================================================
# 03_section_2_1_analysis_v2.R — SECTION 2.1 EMPIRICAL PACKAGE (v2)
# "Структурні зміни вартості фондування та дохідності процентних активів
#  банків України"
# ----------------------------------------------------------------------------
# Input : marts built by 02_build_marts_v6.R
# Output: output_nbu/section_2_1_v2/{tables,figures} + Section_2_1_v2.xlsx
#         + key_facts.csv (numbers quoted in the text of 2.1)
#
# Structure follows the subsection design 2.1.1–2.1.10. Every block is
# robust to missing inputs: if a dataset is not available yet (e.g. UONIA,
# OVDP auctions, currency split of expense, loan principal accounts), the
# block is skipped and the reason is written to tables/T00_blocks_status.csv.
#
# Interpretation discipline (do NOT relax in the text):
#   * realised book rates are not marginal rates;
#   * descriptive cycle ratios are not deposit betas (betas -> 2.2);
#   * FE regressions are associations, not causal effects;
#   * FX nominal cost excludes reserve-requirement shadow cost (-> 2.4);
#   * OVDP auction yield != book yield of a bank's OVDP portfolio.
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "account_map_v6.R"), encoding = "UTF-8")
.need(c("ggplot2", "scales", "strucchange", "openxlsx", "zoo"))
suppressPackageStartupMessages({ library(ggplot2); library(scales); library(strucchange) })

OUT <- file.path(OUTPUT_DIR, "section_2_1_v2")
FIG <- file.path(OUT, "figures"); TAB <- file.path(OUT, "tables")
for (d in c(OUT, FIG, TAB)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(FIG, full.names = TRUE)); unlink(list.files(TAB, full.names = TRUE))

# ------------------------------ LOAD -----------------------------------------
sys  <- store_read("mart_system_month_v6")
bank <- store_read("mart_bank_month_v6")
prod <- store_read("mart_system_product_v6")
if (!nrow(sys)) stop("Run 02_build_marts_v6.R first.")
for (d in list(sys, bank, prod)) d[, month := as.Date(month)]
sys  <- sys[month >= START_MONTH]; bank <- bank[month >= START_MONTH]; prod <- prod[month >= START_MONTH]
setorder(sys, month)
has <- function(dt, ...) all(c(...) %in% names(dt)) && all(vapply(c(...), function(v) any(is.finite(dt[[v]])), logical(1)))
LAST <- max(sys[is.finite(cod4_total), month])

# ------------------------------ STYLE ----------------------------------------
PAL <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"
theme_thesis <- function() theme_minimal(base_size = 11) + theme(
  plot.title = element_text(face = "bold", size = 12.5, colour = INK),
  plot.subtitle = element_text(size = 9.5, colour = INK2), plot.caption = element_text(size = 8, colour = INK2, hjust = 0),
  axis.text = element_text(colour = INK2), axis.title = element_text(size = 9.5, colour = INK2),
  panel.grid.major = element_line(colour = GRID, linewidth = .3), panel.grid.minor = element_blank(),
  legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 9),
  strip.text = element_text(face = "bold", size = 9.5, colour = INK))
pct_axis <- function(acc = 1) scale_y_continuous(labels = label_percent(accuracy = acc))
CAP <- "Джерело: розрахунки автора за даними НБУ (OSB, фінзвітність банків, MIR, ринкові дані)."
source(file.path(PROJECT_DIR, "utils_origin_v6.R"), encoding = "UTF-8")
save_fig <- function(p, file, w = 8.5, h = 4.8) {
  ggsave(file.path(FIG, file), p, width = w, height = h, dpi = 300, bg = "white")
  tryCatch(origin_export(p, file, w, h), error = function(e) log_msg("Origin export skipped for ", file, ": ", conditionMessage(e)))
}
source(file.path(PROJECT_DIR, "utils_figures_academic_v6.R"), encoding = "UTF-8", local = environment())   # academic style: overrides PAL, theme_thesis(), save_fig()
TABLES <- list()
save_tab <- function(x, file, sheet = NULL) {
  x <- as.data.table(x); fwrite(x, file.path(TAB, file))
  TABLES[[sheet %||% sub("\\.csv$", "", file)]] <<- x; invisible(x)
}
STATUS <- data.table(block = character(), status = character(), note = character())
status <- function(b, s, n = "") STATUS <<- rbind(STATUS, data.table(block = b, status = s, note = n))
FACTS <- data.table(item = character(), value = character(), note = character())
fact <- function(k, v, n = "") FACTS <<- rbind(FACTS, data.table(item = k, value = as.character(v), note = n))
p2 <- function(x, d = 2) formatC(100 * x, format = "f", digits = d)

# ------------------------------ MONETARY REGIMES -----------------------------
# Data-driven regimes from the policy-rate path (month-average, decimals):
#   Tightening: 3-month change >= +50 bp; Easing: <= -50 bp;
#   High-rate plateau: otherwise and policy >= sample median;
#   Stable/low-rate: otherwise. Runs shorter than 2 months are absorbed.
reg <- sys[, .(month, policy = policy_avg)]
reg[, d3 := policy - shift(policy, 3)]
q_hi <- quantile(reg$policy, .50, na.rm = TRUE)   # plateau = no move AND policy above sample median
reg[, regime := fcase(d3 >= 0.005, "Tightening", d3 <= -0.005, "Easing",
                      policy >= q_hi, "High-rate plateau", default = "Stable/low-rate")]
reg[is.na(d3), regime := fifelse(policy >= q_hi, "High-rate plateau", "Stable/low-rate")]
reg[, run := rleid(regime)]
short <- reg[, .N, by = run][N < 2, run]
for (r in short) { i <- which(reg$run == r); if (min(i) > 1) reg$regime[i] <- reg$regime[min(i) - 1] }
reg[, cycle := rleid(regime)]
cycles <- reg[, .(start = min(month), end = max(month), months = .N,
                  policy_start = policy[1], policy_end = policy[.N]), by = .(cycle, regime)]
REG_COL <- c("Tightening" = "#ececec", "Easing" = "#f6f6f6", "High-rate plateau" = "#dcdcdc", "Stable/low-rate" = "#ffffff")   # grey bands (academic)
REG_LAB <- c("Tightening" = "Фаза: посилення", "Easing" = "Фаза: пом'якшення", "High-rate plateau" = "Фаза: плато високих ставок")
# Regime shading is inserted as the FIRST layer so that data lines stay on top.
add_regimes <- function(p) {
  rect <- geom_rect(data = cycles[regime %in% names(REG_LAB)],
                    aes(xmin = start, xmax = end + 27, ymin = -Inf, ymax = Inf, fill = regime), inherit.aes = FALSE, alpha = .7)
  p$layers <- c(list(rect), p$layers)
  p + scale_fill_manual(values = REG_COL, labels = REG_LAB, breaks = names(REG_LAB), guide = guide_legend(order = 2)) +
    theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
}
sys <- merge(sys, reg[, .(month, regime, cycle)], by = "month", all.x = TRUE)
save_tab(cycles, "T18_policy_cycles.csv", "Cycles")

# ======================= 2.1.1 SAMPLE AND DATA QUALITY =======================
t01 <- bank[, .(banks = uniqueN(nkb), banks_with_cod = sum(is.finite(cod4_total)),
                deposits_core4_bn = sum(dep_core4_total, na.rm = TRUE) / 1000,
                flagged_rates = sum(dq_rate_out_of_bounds, na.rm = TRUE)), by = month]
if ("coverage_core4" %in% names(sys)) t01 <- merge(t01, sys[, .(month, coverage_core4)], by = "month")
save_tab(t01, "T01_sample_by_month.csv", "Sample")
fact("sample_period", paste(min(bank$month), "—", LAST))
fact("bank_months", nrow(bank)); fact("banks_total", uniqueN(bank$nkb))
fact("banks_first", t01[1, banks]); fact("banks_last", t01[month == LAST, banks])
if ("coverage_core4" %in% names(sys)) fact("coverage_core4_mean", p2(mean(sys$coverage_core4, na.rm = TRUE), 1), "% коштів клієнтів у фінзвітності")

# Correction effect: gross (v5) vs net (v6) turnover, 12x vs day-count.
fl <- tryCatch(fread(file.path(LOG_DIR, "build_v6_flow_turnovers_by_account.csv")), error = function(e) data.table())
if (nrow(fl)) {
  fl[, month := as.Date(month)]; fl[, r020 := sprintf("%04d", as.integer(r020))]
  g <- fl[r020 %in% c("7020", "7021", "7040", "7041"), .(gross = sum(gross_debit), net = sum(gross_debit - gross_credit)), by = month]
  g <- merge(g, sys[, .(month, avg = cod4_total_avgstock, cod4_total)], by = "month")
  g[, `:=`(v5_gross_12x = 12 * gross / avg, net_12x = 12 * net / avg, v6 = cod4_total)]
  save_tab(g, "T02_correction_gross_vs_net.csv", "Correction")
  gl <- melt(g[, .(month, `v5: валовий дебет × 12` = v5_gross_12x, `нетто × 12` = net_12x, `v6: нетто × днів/міс.` = v6)],
             id.vars = "month")
  p <- ggplot(gl[is.finite(value)], aes(month, value, colour = variable)) + geom_line(linewidth = .6) +
    scale_colour_manual(values = PAL[c(8, 4, 1)]) + pct_axis(.5) +
    labs(title = "Ефект методологічних виправлень: вартість депозитів (4 продукти)",
         subtitle = "Валовий дебетовий оборот містить сторнування; 12× ігнорує кількість днів у місяці",
         x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
  save_fig(p, "F00_correction_gross_vs_net.png")
  worst <- g[which.max(v5_gross_12x - v6)]
  fact("storno_worst_month", worst$month, sprintf("v5 %.2f%% vs v6 %.2f%%", 100 * worst$v5_gross_12x, 100 * worst$v6))
  status("2.1.1 correction", "ok")
}

# ======================= 2.1.2 FUNDING STRUCTURE ==============================
fs <- sys[, .(month, demand_share, hh_share, fx_share, cd_to_dep, ovdp_to_dep, hhi_dep, top5_dep, n_eff,
              dep_core4_total, dep_core4_uah, dep_core4_fx)]
# FX share: valuation vs volume decomposition (requires official FX rates).
if (has(sys, "usd_eom") && "fx_basket_dlog" %in% names(sys)) {
  fs <- merge(fs, sys[, .(month, fx_basket_dlog)], by = "month")
  setorder(fs, month)
  # FX stock at constant (previous-month) exchange rate -> pure volume change.
  fs[, fx_const := dep_core4_fx / exp(fx_basket_dlog)]
  fs[, s_l := shift(fx_share)]
  fs[, share_const := fx_const / (fx_const + dep_core4_uah)]
  fs[, `:=`(d_fx_share = fx_share - s_l, d_valuation = fx_share - share_const, d_volume = share_const - s_l)]
  fs[, `:=`(cum_valuation = cumsum(fcoalesce(d_valuation, 0)), cum_volume = cumsum(fcoalesce(d_volume, 0)))]
  status("2.1.2 FX valuation split", "ok")
  p <- ggplot(melt(fs[, .(month, `Переоцінка (курс)` = cum_valuation, `Реальний переток (обсяг)` = cum_volume)], id.vars = "month"),
              aes(month, value, colour = variable)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
    geom_line(linewidth = .7) + scale_colour_manual(values = PAL[c(2, 1)]) + pct_axis(1) +
    labs(title = "Накопичена зміна валютної частки депозитів: курс vs обсяг",
         subtitle = "Точна декомпозиція: валютні залишки за курсом попереднього місяця", x = NULL, y = "в.п. частки", caption = CAP) + theme_thesis()
  save_fig(p, "F02b_fx_share_valuation_vs_volume.png")
} else status("2.1.2 FX valuation split", "skipped", "немає raw_fx (запустіть 01_nbu_download_v6.R)")
save_tab(fs, "T03_funding_structure.csv", "Funding_structure")

fsl <- melt(fs[, .(month, `Кошти на вимогу` = demand_share, `Населення` = hh_share, `Іноземна валюта` = fx_share)], id.vars = "month")
p <- ggplot(fsl, aes(month, value, colour = variable)) + geom_line(linewidth = .7) +
  scale_colour_manual(values = PAL[1:3]) + pct_axis(1) +
  labs(title = "Трансформація структури депозитного фондування",
       subtitle = "Системні частки в коштах СГ і ФО (рахунки гр. 260, 261, 262, 263), розраховані із сум залишків",
       x = NULL, y = "Частка", caption = CAP) + theme_thesis()
save_fig(add_regimes(p), "F01_funding_structure.png")

p <- ggplot(fs, aes(month, n_eff)) + geom_line(colour = PAL[1], linewidth = .7) +
  labs(title = "Концентрація депозитного ринку: ефективна кількість банків (1/HHI)",
       subtitle = sprintf("HHI %.3f → %.3f; частка топ-5: %s%% → %s%%", fs[1, hhi_dep], fs[month == LAST, hhi_dep],
                          p2(fs[1, top5_dep], 1), p2(fs[month == LAST, top5_dep], 1)),
       x = NULL, y = "Банків-еквівалентів", caption = CAP) + theme_thesis()
save_fig(p, "F02_concentration.png")
f0 <- fs[month == min(month)]; fL <- fs[month == LAST]
for (v in c("demand_share", "hh_share", "fx_share", "cd_to_dep", "ovdp_to_dep", "top5_dep"))
  fact(paste0(v, "_start_end"), paste0(p2(f0[[v]], 1), "% → ", p2(fL[[v]], 1), "%"))
fact("n_eff_start_end", sprintf("%.1f → %.1f", f0$n_eff, fL$n_eff))

# Maturity structure (monetary statistics; HH + NFC, all currencies).
if (has(sys, "ms_mat_total")) {
  ms <- sys[, .(month, `На вимогу` = ms_mat_demand / ms_mat_total, `До 1 року` = ms_mat_lt1y / ms_mat_total,
                `1–2 роки` = ms_mat_1_2y / ms_mat_total, `Понад 2 роки` = ms_mat_gt2y / ms_mat_total)]
  save_tab(ms, "T03b_maturity_structure_monetary_stats.csv", "Maturity")
  p <- ggplot(melt(ms, id.vars = "month"), aes(month, value, fill = variable)) +
    geom_area(colour = "white", linewidth = .25) + scale_fill_manual(values = PAL[1:4]) + pct_axis(1) +
    labs(title = "Строкова структура депозитів ФО та нефінансових корпорацій",
         subtitle = "Грошово-кредитна статистика НБУ (ods183dd), усі валюти", x = NULL, y = "Частка", caption = CAP) + theme_thesis()
  save_fig(p, "F02c_maturity_structure.png")
  fact("ms_gt1y_share_start_end", paste0(p2(sum(unlist(ms[1, 4:5])), 1), "% → ", p2(sum(unlist(ms[month == LAST, 4:5])), 1), "%"))
}

# ======================= 2.1.3 REALISED FUNDING COST ==========================
PRODUCT_LAB <- c(corp_demand = "СГ: на вимогу", corp_term = "СГ: строкові", hh_demand = "ФО: на вимогу", hh_term = "ФО: строкові",
                 nbfi_demand = "НБФУ: на вимогу", nbfi_term = "НБФУ: строкові")
pc <- prod[ccy == "total" & core4 == TRUE, .(month, product, rate, avg_stock)]
cost_wide <- dcast(pc, month ~ product, value.var = "rate")
if (!"cpi_yoy" %in% names(sys)) sys[, cpi_yoy := NA_real_]
cost_wide <- merge(cost_wide, sys[, .(month, cod4_total, cod4_uah, cod4_fx, policy_avg, cpi_yoy)], by = "month")
cost_wide[, real_cod4 := (1 + cod4_total) / (1 + cpi_yoy) - 1]
cost_wide[, `:=`(hh_term_demand_gap = hh_term - hh_demand, corp_term_demand_gap = corp_term - corp_demand,
                 hh_corp_term_gap = hh_term - corp_term)]
save_tab(cost_wide, "T04_realised_cost_products.csv", "Cost_products")

pl <- rbind(pc[, .(month, series = PRODUCT_LAB[product], rate)], sys[, .(month, series = "Разом (4 продукти)", rate = cod4_total)])
pl[, series := factor(series, levels = c(PRODUCT_LAB[1:4], "Разом (4 продукти)"))]
p <- ggplot(pl[is.finite(rate)], aes(month, rate, colour = series, linewidth = series == "Разом (4 продукти)")) + geom_line() +
  scale_linewidth_manual(values = c(.55, 1.1), guide = "none") + scale_colour_manual(values = c(PAL[1:4], INK)) + pct_axis(1) +
  labs(title = "Фактична (книжкова) вартість депозитного фондування за продуктами",
       subtitle = "Нетто процентні витрати × (днів у році / днів у місяці) / середній залишок; узгоджена вибірка банків",
       x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
save_fig(add_regimes(p), "F03_realised_cost_products.png")

if (has(cost_wide, "cpi_yoy")) {
  rl <- melt(cost_wide[, .(month, `Номінальна вартість депозитів` = cod4_total, `Реальна (дефлятована ІСЦ р/р)` = real_cod4,
                           `Облікова ставка, реальна` = (1 + policy_avg) / (1 + cpi_yoy) - 1)], id.vars = "month")
  p <- ggplot(rl[is.finite(value)], aes(month, value, colour = variable)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
    geom_line(linewidth = .7) + scale_colour_manual(values = PAL[c(1, 2, 7)]) + pct_axis(1) +
    labs(title = "Номінальна та реальна вартість фондування", subtitle = "Ex post реальні ставки: (1+i)/(1+π р/р) − 1",
         x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
  save_fig(add_regimes(p), "F03b_real_cost.png")
  fact("real_cod4_min", sprintf("%s%% (%s)", p2(min(cost_wide$real_cod4, na.rm = TRUE)), cost_wide[which.min(real_cod4), month]))
  fact("real_cod4_last", p2(cost_wide[month == LAST, real_cod4]))
}
for (pr in c("corp_demand", "corp_term", "hh_demand", "hh_term"))
  fact(paste0("rate_", pr, "_start_last"), paste0(p2(cost_wide[is.finite(get(pr))][1][[pr]]), "% → ", p2(cost_wide[month == LAST][[pr]]), "%"))
fact("cod4_min", sprintf("%s%% (%s)", p2(min(sys$cod4_total, na.rm = TRUE)), sys[which.min(cod4_total), month]))
fact("cod4_max", sprintf("%s%% (%s)", p2(max(sys$cod4_total, na.rm = TRUE)), sys[which.max(cod4_total), month]))
fact("cod4_last", p2(sys[month == LAST, cod4_total]))

# Annual and regime tables of product rates.
cost_wide[, year := year(month)]
save_tab(cost_wide[, lapply(.SD, mean, na.rm = TRUE), by = year, .SDcols = c("corp_demand", "corp_term", "hh_demand", "hh_term", "cod4_total", "policy_avg")],
         "T04b_cost_products_annual.csv", "Cost_annual")

# Marginal (new business, MIR) vs book (OSB) — the pricing pipeline.
if (has(sys, "nb_dep_hh_term_total", "nb_dep_nfc_term_total")) {
  mb <- merge(cost_wide[, .(month, hh_term, corp_term, hh_demand)],
              sys[, .(month, nb_hh_term = nb_dep_hh_term_total, nb_nfc_term = nb_dep_nfc_term_total,
                      policy_avg)], by = "month")
  mb[, `:=`(gap_hh_term = nb_hh_term - hh_term, gap_corp_term = nb_nfc_term - corp_term)]
  save_tab(mb, "T04c_marginal_vs_book.csv", "Marginal_vs_book")
  ml <- melt(mb[, .(month, `ФО строкові: нові (MIR)` = nb_hh_term, `ФО строкові: книжкова (OSB)` = hh_term,
                    `СГ строкові: нові (MIR)` = nb_nfc_term, `СГ строкові: книжкова (OSB)` = corp_term)], id.vars = "month")
  ml[, sector := fifelse(grepl("^ФО", variable), "Населення", "Суб'єкти господарювання")]
  ml[, kind := fifelse(grepl("нові", variable), "Нові операції (маржинальна)", "Портфель (книжкова)")]
  # NA months (MIR not published in 2022–2023) are kept so the line breaks instead of interpolating.
  ml <- ml[CJ(month = unique(ml$month), variable = unique(ml$variable)), on = .(month, variable)]
  ml[, `:=`(sector = fifelse(grepl("^ФО", variable), "Населення", "Суб'єкти господарювання"),
            kind = fifelse(grepl("нові", variable), "Нові операції (маржинальна)", "Портфель (книжкова)"))]
  p <- ggplot(ml, aes(month, value, colour = kind)) + geom_line(linewidth = .65, na.rm = TRUE) + facet_wrap(~sector) +
    scale_colour_manual(values = PAL[c(2, 1)]) + pct_axis(1) +
    labs(title = "Маржинальна vs книжкова вартість строкових депозитів",
         subtitle = "Розрив = «незавершене переоцінювання» портфеля; MIR — усі валюти, OSB — усі валюти",
         x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
  save_fig(add_regimes(p), "F04_marginal_vs_book.png", 9, 4.6)
  fact("gap_hh_term_max", sprintf("%s в.п. (%s)", p2(max(mb$gap_hh_term, na.rm = TRUE)), mb[which.max(gap_hh_term), month]))
  fact("gap_hh_term_min", sprintf("%s в.п. (%s)", p2(min(mb$gap_hh_term, na.rm = TRUE)), mb[which.min(gap_hh_term), month]))
  fact("gap_hh_term_last", p2(mb[month == LAST, gap_hh_term]))
  status("2.1.3 marginal vs book", "ok")
} else status("2.1.3 marginal vs book", "skipped", "немає MIR")

# ======================= 2.1.4 PRICE / MIX DECOMPOSITION =====================
# c_t = sum_j s_jt r_jt ;  Δc_t = Σ s̄ Δr (price) + Σ r̄ Δs (mix) — exact.
# Cells: 4 products (all currencies) and, when the currency split exists,
# 8 cells product x currency (then mix also captures UAH<->FX migration).
midpoint <- function(dt) {  # dt: month, cell, rate, w (avg stock)
  dt <- copy(dt)[is.finite(rate) & is.finite(w)]
  dt[, s := w / sum(w), by = month]
  setorder(dt, cell, month)
  dt[, `:=`(r_l = shift(rate), s_l = shift(s), m_l = shift(month)), by = cell]
  dt <- dt[m_l == add_months(month, -1L)]
  dt[, `:=`(price = (s + s_l) / 2 * (rate - r_l), mix = (rate + r_l) / 2 * (s - s_l))]
  dt
}
do_decomp <- function(cells, tag) {
  d <- midpoint(cells)
  m <- d[, .(price = sum(price), mix = sum(mix)), by = month]
  lv <- cells[is.finite(rate) & is.finite(w), .(c = sum(w * rate) / sum(w)), by = month]
  m <- merge(m, lv[, .(month, c, c_l = shift(c))], by = "month")
  m[, `:=`(delta = c - c_l, residual = c - c_l - price - mix)]
  m <- merge(m, reg[, .(month, regime, cycle)], by = "month", all.x = TRUE)
  setorder(m, month)
  m[, `:=`(cum_price = cumsum(price), cum_mix = cumsum(mix), cum_delta = cumsum(delta))]
  byc <- m[, .(start = min(month), end = max(month), months = .N, delta = sum(delta), price = sum(price), mix = sum(mix)), by = .(cycle, regime)]
  byc[, mix_share := mix / (abs(price) + abs(mix))]
  bycell <- merge(d[, .(price = sum(price), mix = sum(mix)), by = .(cell, cycle = reg$cycle[match(month, reg$month)])],
                  byc[, .(cycle, regime, start, end)], by = "cycle")
  list(month = m, cycle = byc, cell = bycell, max_resid = max(abs(m$residual), na.rm = TRUE), tag = tag)
}
cells4 <- prod[ccy == "total" & core4 == TRUE, .(month, cell = product, rate, w = avg_stock)]
D4 <- do_decomp(cells4, "4 products")
save_tab(D4$month, "T05_price_mix_monthly_4products.csv", "PriceMix_monthly")
save_tab(D4$cycle, "T06_price_mix_by_cycle_4products.csv", "PriceMix_cycles")
save_tab(D4$cell, "T06b_price_mix_by_cycle_and_product.csv", "PriceMix_cells")
fact("decomp_max_abs_residual", formatC(D4$max_resid, format = "e", digits = 1), "точність тотожності")
fact("decomp_cum_total", p2(sum(D4$month$delta, na.rm = TRUE)), "в.п., весь період")
fact("decomp_cum_price", p2(sum(D4$month$price, na.rm = TRUE)), "в.п.")
fact("decomp_cum_mix", p2(sum(D4$month$mix, na.rm = TRUE)), "в.п.")

cl <- melt(D4$month[, .(month, `Ціновий ефект` = cum_price, `Ефект структури (mix)` = cum_mix, `Загальна зміна` = cum_delta)], id.vars = "month")
p <- ggplot(cl, aes(month, value, colour = variable, linewidth = variable)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
  geom_line() + scale_colour_manual(values = c(PAL[1], PAL[2], INK)) + scale_linewidth_manual(values = c(.7, .7, 1), guide = "none") +
  pct_axis(.5) + labs(title = "Накопичена декомпозиція зміни вартості депозитів: ціна vs структура",
                      subtitle = "Ланцюгова midpoint-декомпозиція по 4 продуктах; сума компонент = фактична зміна (залишок ≈ 0)",
                      x = NULL, y = "в.п. до 2018-02", caption = CAP) + theme_thesis()
save_fig(add_regimes(p), "F05_cumulative_price_mix.png")

REG_SHORT <- c("Tightening" = "Посилення", "Easing" = "Пом'якшення", "High-rate plateau" = "Плато високих ставок", "Stable/low-rate" = "Стабільні / низькі ставки")
bc <- melt(D4$cycle[months >= 3, .(lab = sprintf("%s\n%s–%s", REG_SHORT[regime], format(start, "%m.%y"), format(end, "%m.%y")), cycle,
                                   `Ціновий` = price, `Структура` = mix)], id.vars = c("lab", "cycle"))
bc[, lab := factor(lab, levels = unique(lab[order(cycle)]))]
p <- ggplot(bc, aes(lab, value, fill = variable)) + geom_col(position = position_dodge(.75), width = .7, colour = "white", linewidth = .5) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + scale_fill_manual(values = PAL[1:2]) + pct_axis(.1) +
  labs(title = "Ціновий та структурний внески у зміну вартості депозитів за фазами циклу",
       x = NULL, y = "в.п.", caption = CAP) + theme_thesis() + theme(axis.text.x = element_text(size = 7.5))
save_fig(p, "F06_price_mix_by_cycle.png", 10, 5)

# ======================= 2.1.5 CURRENCY STRUCTURE =============================
if (has(sys, "cod4_uah", "cod4_fx")) {
  cells8 <- prod[ccy %in% c("uah", "fx") & core4 == TRUE, .(month, cell = paste(product, ccy), rate, w = avg_stock)]
  D8 <- do_decomp(cells8, "8 cells")
  ccy2 <- sys[, .(month, uah = cod4_uah, fx = cod4_fx, w_uah = cod4_uah_avgstock, w_fx = cod4_fx_avgstock)]
  Dc <- do_decomp(melt(ccy2, id.vars = "month", measure.vars = list(c("uah", "fx"), c("w_uah", "w_fx")),
                       value.name = c("rate", "w"))[, .(month, cell = c("UAH", "FX")[variable], rate, w)], "UAH/FX")
  save_tab(D8$cycle, "T07_price_mix_by_cycle_8cells.csv", "PriceMix_8cells")
  save_tab(Dc$month, "T08_currency_price_mix_monthly.csv", "Currency_decomp")
  save_tab(Dc$cycle, "T08b_currency_price_mix_by_cycle.csv", "Currency_cycles")
  p <- ggplot(melt(ccy2[, .(month, `Гривневі депозити` = uah, `Валютні депозити` = fx)], id.vars = "month")[is.finite(value)],
              aes(month, value, colour = variable)) + geom_line(linewidth = .7) + scale_colour_manual(values = PAL[c(1, 2)]) + pct_axis(1) +
    labs(title = "Номінальна книжкова вартість гривневого та валютного фондування",
         subtitle = "Без тіньової вартості резервних вимог (розділ 2.4); валютний розподіл витрат оцінено за нарахуваннями 26x8",
         x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
  save_fig(add_regimes(p), "F07_uah_fx_cost.png")
  fact("cod_uah_last", p2(sys[month == LAST, cod4_uah])); fact("cod_fx_last", p2(sys[month == LAST, cod4_fx]))
  status("2.1.5 currency", "ok")
} else status("2.1.5 currency", "skipped",
              "OSB зберігає процентні витрати лише в UAH; потрібні рахунки нарахованих витрат 2608/2618/2628/2638 (01_nbu_download_v6.R)")

# ======================= 2.1.6 MARKET BENCHMARKS ==============================
bm_cols <- c(cod4 = "cod4_total", policy = "policy_avg", cd_on = "cd_on_rate_avg", uonia = "uonia_avg",
             ovdp_primary = "ovdp_uah_all", cd_book = "y_cd_total", refin_book = "y_refin_total", zc_1y = "zc_uah_1y_avg")
bm_cols <- bm_cols[vapply(bm_cols, function(v) has(sys, v), logical(1))]
bm <- sys[, c("month", unname(bm_cols)), with = FALSE]; setnames(bm, c("month", names(bm_cols)))
for (b in setdiff(names(bm_cols), "cod4")) bm[, (paste0("wedge_", b)) := cod4 - get(b)]
if ("cd_book" %in% names(bm)) bm[, carry_cd := cd_book - cod4]
if ("refin_book" %in% names(bm)) bm[, carry_refin := refin_book - cod4]
bm <- merge(bm, reg[, .(month, regime)], by = "month")
save_tab(bm, "T09_market_benchmarks_wedges.csv", "Benchmarks")
save_tab(bm[, lapply(.SD, mean, na.rm = TRUE), by = regime, .SDcols = setdiff(names(bm), c("month", "regime"))],
         "T09b_benchmarks_by_regime.csv", "Benchmarks_regime")
lab_bm <- c(cod4 = "Вартість депозитів (книжкова)", policy = "Облікова ставка", cd_on = "ДС овернайт НБУ", uonia = "UONIA",
            ovdp_primary = "ОВДП, первинний ринок", cd_book = "Дохідність ДС (книжкова)", refin_book = "Дохідність ЦП, що рефін. (книжкова)",
            zc_1y = "Безкупонна ставка 1Y (NSS)")
bl <- melt(bm[, c("month", names(bm_cols)), with = FALSE], id.vars = "month")[is.finite(value)]
bl[, variable := factor(lab_bm[as.character(variable)], levels = lab_bm[names(bm_cols)])]
p <- ggplot(bl, aes(month, value, colour = variable, linewidth = variable == lab_bm["cod4"])) + geom_line() +
  scale_linewidth_manual(values = c(.55, 1.1), guide = "none") + scale_colour_manual(values = c(INK, PAL)[seq_along(bm_cols)]) + pct_axis(1) +
  labs(title = "Вартість фондування в системі ринкових процентних орієнтирів",
       subtitle = "Книжкова вартість депозитів проти маржинальних альтернатив розміщення ліквідності", x = NULL, y = "Річна ставка", caption = CAP) +
  theme_thesis() + guides(colour = guide_legend(nrow = 2))
save_fig(add_regimes(p), "F08_benchmarks.png", 9.5, 5.2)
if ("carry_cd" %in% names(bm)) {
  p <- ggplot(bm[is.finite(carry_cd)], aes(month, carry_cd)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
    geom_line(colour = PAL[1], linewidth = .8) + pct_axis(1) +
    labs(title = "Ліквідний керрі: дохідність депсертифікатів НБУ мінус вартість депозитів",
         subtitle = "Маржа «безризикового» розміщення залученого ресурсу в інструменти НБУ (без кредитного ризику)",
         x = NULL, y = "в.п.", caption = CAP) + theme_thesis()
  save_fig(add_regimes(p), "F09_cd_carry.png")
  fact("carry_cd_mean_war", p2(bm[month >= WAR_START, mean(carry_cd, na.rm = TRUE)]), "в.п., з 2022-02")
  fact("carry_cd_mean_prewar", p2(bm[month < WAR_START, mean(carry_cd, na.rm = TRUE)]), "в.п., до 2022-02")
  fact("carry_cd_last", p2(bm[month == LAST, carry_cd]))
}
fact("wedge_policy_last", p2(bm[month == LAST, wedge_policy]))
fact("ratio_cod_policy_last", sprintf("%.2f", bm[month == LAST, cod4 / policy]))
for (s in c("uonia", "ovdp_primary")) if (!s %in% names(bm_cols)) status(paste("2.1.6", s), "skipped", "немає даних (01_nbu_download_v6.R / local_inputs)")

# Descriptive cycle ratio: Δ cost / Δ policy within each cycle (NOT a beta).
cr <- merge(cycles, sys[, .(month, cod4_total, hh_term = NA_real_)][, .(start = month, cod_start = cod4_total)], by = "start")
cr <- merge(cr, sys[, .(end = month, cod_end = cod4_total)], by = "end")
cr[, `:=`(d_policy = policy_end - policy_start, d_cod = cod_end - cod_start)]
cr[, cycle_ratio := fifelse(abs(d_policy) >= 0.01, d_cod / d_policy, NA_real_)]
save_tab(cr[order(cycle)], "T10_cycle_ratios_descriptive.csv", "Cycle_ratios")

# ======================= 2.1.7 TERM STRUCTURE =================================
mats <- c("3m", "6m", "1y", "2y", "3y", "5y", "7y", "10y")
zc <- paste0("zc_uah_", mats)
if (has(sys, zc[1], zc[8])) {
  cv <- sys[, c("month", zc, "policy_avg"), with = FALSE][complete.cases(sys[, ..zc])]
  cv[, `:=`(slope_10y_1y = zc_uah_10y - zc_uah_1y, slope_1y_3m = zc_uah_1y - zc_uah_3m,
            curvature = 2 * zc_uah_2y - zc_uah_6m - zc_uah_5y,
            fwd_1y1y = exp(2 * log1p(zc_uah_2y) - log1p(zc_uah_1y)) - 1,
            fwd_2y3y = exp((5 * log1p(zc_uah_5y) - 2 * log1p(zc_uah_2y)) / 3) - 1,
            zc3m_minus_policy = zc_uah_3m - policy_avg)]
  X <- as.matrix(cv[, ..zc]); pca <- prcomp(X, center = TRUE, scale. = FALSE)
  ev <- data.table(component = paste0("PC", 1:8), share = pca$sdev^2 / sum(pca$sdev^2)); ev[, cumulative := cumsum(share)]
  ld <- data.table(maturity = factor(mats, levels = mats), pca$rotation[, 1:3])
  # sign convention: PC1 loads positively on average; PC2 positive on 10y
  if (mean(ld$PC1) < 0) { ld[, PC1 := -PC1]; pca$x[, 1] <- -pca$x[, 1] }
  if (ld[maturity == "10y", PC2] < 0) { ld[, PC2 := -PC2]; pca$x[, 2] <- -pca$x[, 2] }
  if (ld[maturity == "2y", PC3] < 0) { ld[, PC3 := -PC3]; pca$x[, 3] <- -pca$x[, 3] }
  cv <- cbind(cv, data.table(PC1 = pca$x[, 1], PC2 = pca$x[, 2], PC3 = pca$x[, 3]))
  cv[, `:=`(cor_pc1_level = NA_real_)]
  fact("pca_explained_pc1_pc3", paste(p2(ev$share[1], 1), p2(ev$share[2], 1), p2(ev$share[3], 1), sep = "% / "))
  fact("corr_PC1_vs_mean_level", sprintf("%.3f", cor(cv$PC1, rowMeans(X))))
  fact("corr_PC2_vs_slope", sprintf("%.3f", cor(cv$PC2, cv$slope_10y_1y)))
  fact("corr_PC3_vs_curvature", sprintf("%.3f", cor(cv$PC3, cv$curvature)))
  fact("slope_last", p2(cv[.N, slope_10y_1y])); fact("slope_min", sprintf("%s (%s)", p2(min(cv$slope_10y_1y)), cv[which.min(slope_10y_1y), month]))
  save_tab(cv, "T11_yield_curve_factors.csv", "Curve_factors"); save_tab(ev, "T12_pca_explained.csv", "PCA_explained")
  save_tab(ld, "T13_pca_loadings.csv", "PCA_loadings")

  p <- ggplot(melt(cv[, c("month", zc[c(1, 3, 5, 8)]), with = FALSE], id.vars = "month")[, variable := factor(sub("zc_uah_", "", variable), levels = mats)],
              aes(month, value, colour = variable)) + geom_line(linewidth = .65) + scale_colour_manual(values = PAL[1:4]) + pct_axis(1) +
    labs(title = "Гривнева безкупонна крива НБУ (NSS): спот-ставки", subtitle = "Кінець місяця, ефективні річні ставки",
         x = NULL, y = "Спот-ставка", caption = CAP) + theme_thesis()
  save_fig(add_regimes(p), "F10_zero_curve_levels.png")
  snaps <- unique(c(as.Date(c("2018-12-01", "2020-12-01", "2022-12-01", "2023-12-01", "2024-12-01")), max(cv$month)))
  sn <- melt(cv[month %in% snaps, c("month", zc), with = FALSE], id.vars = "month")
  sn[, `:=`(t = c(.25, .5, 1, 2, 3, 5, 7, 10)[match(sub("zc_uah_", "", variable), mats)], lab = format(month, "%Y-%m"))]
  p <- ggplot(sn, aes(t, value, colour = lab)) + geom_line(linewidth = .7) + geom_point(size = 1.6) +
    scale_colour_manual(values = PAL[seq_along(snaps)]) + pct_axis(1) + scale_x_continuous(breaks = c(.25, 1, 2, 3, 5, 7, 10)) +
    labs(title = "Форма гривневої кривої в ключові моменти циклу", x = "Строк, років", y = "Спот-ставка", caption = CAP) + theme_thesis()
  save_fig(p, "F11_curve_snapshots.png")
  p <- ggplot(melt(cv[, .(month, `Нахил 10Y–1Y` = slope_10y_1y, `Нахил 1Y–3M` = slope_1y_3m, `Кривизна 2×2Y–6M–5Y` = curvature)], id.vars = "month"),
              aes(month, value, colour = variable)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + geom_line(linewidth = .65) +
    scale_colour_manual(values = PAL[1:3]) + pct_axis(1) +
    labs(title = "Нахил і кривизна кривої: ринкові очікування щодо траєкторії ставок", x = NULL, y = "в.п.", caption = CAP) + theme_thesis()
  save_fig(add_regimes(p), "F12_slope_curvature.png")
  p <- ggplot(melt(ld, id.vars = "maturity"), aes(maturity, value, colour = variable, group = variable)) +
    geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + geom_line(linewidth = .7) + geom_point(size = 1.8) +
    scale_colour_manual(values = PAL[1:3]) +
    labs(title = "Навантаження головних компонент кривої", subtitle = sprintf("Пояснена дисперсія: PC1 %s%%, PC2 %s%%, PC3 %s%%",
         p2(ev$share[1], 1), p2(ev$share[2], 1), p2(ev$share[3], 1)), x = "Строк", y = "Навантаження", caption = CAP) + theme_thesis()
  save_fig(p, "F13_pca_loadings.png")
  status("2.1.7 curve", "ok")
} else status("2.1.7 curve", "skipped", "немає NSS-параметрів")

# ======================= 2.1.8 HETEROGENEITY ==================================
bk <- bank[is.finite(cod4_total) & dep_core4_total > 0 & !dq_rate_out_of_bounds]
disp <- bk[, {q <- quantile(cod4_total, c(.1, .25, .5, .75, .9)); .(n = .N, p10 = q[1], p25 = q[2], median = q[3], p75 = q[4], p90 = q[5],
           eq_mean = mean(cod4_total), w_mean = sum(cod4_total * avg_dep_core4_total, na.rm = TRUE) / sum(avg_dep_core4_total[is.finite(cod4_total)], na.rm = TRUE))},
           by = month]
disp <- merge(disp, sys[, .(month, system = cod4_total)], by = "month")
# Granularity (Gabaix): system - equal-weighted mean = N * cov(w, r).
disp[, `:=`(granular_gap = system - eq_mean, iqr = p75 - p25, p90_p10 = p90 - p10)]
save_tab(disp, "T14_cross_section_dispersion.csv", "Dispersion")
p <- ggplot(disp, aes(month)) + geom_ribbon(aes(ymin = p10, ymax = p90, fill = "P10–P90"), alpha = .9) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "P25–P75"), alpha = .9) +
  geom_line(aes(y = median, colour = "Медіанний банк"), linewidth = .8) + geom_line(aes(y = system, colour = "Система (зважено)"), linewidth = .8) +
  scale_fill_manual(values = c("P10–P90" = "#dceafa", "P25–P75" = "#a9c9ef")) + scale_colour_manual(values = c(PAL[1], INK)) + pct_axis(1) +
  labs(title = "Міжбанківська дисперсія фактичної вартості депозитів", subtitle = "Банки з коректно визначеною ставкою; система — агрегат потоків/залишків",
       x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
save_fig(p, "F14_dispersion.png")
fact("granular_gap_mean", p2(mean(disp$granular_gap, na.rm = TRUE)), "в.п.: система − середнє по банках")
fact("median_vs_system_last", sprintf("медіана %s%% vs система %s%%", p2(disp[month == LAST, median]), p2(disp[month == LAST, system])))
fact("iqr_mean_prewar_war", sprintf("%s / %s в.п.", p2(disp[month < WAR_START, mean(iqr)]), p2(disp[month >= WAR_START, mean(iqr)])))

grp_rate <- function(by_var) {
  z <- bank[!is.na(get(by_var)) & is.finite(exp_core4_total) & is.finite(avg_dep_core4_total) & avg_dep_core4_total >= 1,
            .(rate = ann[1] * sum(exp_core4_total) / sum(avg_dep_core4_total), dep = sum(dep_core4_total, na.rm = TRUE),
              demand_share = sum(dep_demand4_total) / sum(dep_core4_total), hh_share = sum(dep_hh_total) / sum(dep_core4_total),
              fx_share = sum(dep_core4_fx) / sum(dep_core4_total), cd_to_dep = sum(cd_nbu_total) / sum(dep_core4_total),
              ovdp_to_dep = sum(ovdp_total) / sum(dep_core4_total),
              median_bank_rate = median(cod4_total[!dq_rate_out_of_bounds], na.rm = TRUE), n = uniqueN(nkb)),
            by = c("month", by_var)]
  z[, dep_share := dep / sum(dep), by = month]; z
}
own <- grp_rate("owner"); save_tab(own, "T15_ownership_groups.csv", "Ownership")
OWN_LAB <- c(State = "Державні", Foreign = "Іноземних груп", Private = "Приватні")
p <- ggplot(own, aes(month, rate, colour = OWN_LAB[owner])) + geom_line(linewidth = .75) + scale_colour_manual(values = PAL[1:3]) + pct_axis(1) +
  labs(title = "Вартість депозитів за групами власності банків", subtitle = "Агрегати потоків і залишків у межах групи", x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
save_fig(add_regimes(p), "F15_cost_by_ownership.png")
sz <- grp_rate("size_q"); save_tab(sz, "T16_size_quartiles.csv", "Size_quartiles")
p <- ggplot(sz, aes(month, rate, colour = paste0("Q", size_q))) + geom_line(linewidth = .7) + scale_colour_manual(values = PAL[1:4]) + pct_axis(1) +
  labs(title = "Вартість депозитів за квартилями розміру банку (активи)", subtitle = "Q4 — найбільші банки", x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
save_fig(add_regimes(p), "F16_cost_by_size.png")
ol <- own[month == LAST]; for (i in seq_len(nrow(ol))) fact(paste0("own_", ol$owner[i], "_last"),
  sprintf("ставка %s%%, частка депозитів %s%%, на вимогу %s%%", p2(ol$rate[i]), p2(ol$dep_share[i], 1), p2(ol$demand_share[i], 1)))

# Between/within decomposition of cross-bank variance (deposit-weighted).
vd <- bk[!is.na(owner), {
  w <- avg_dep_core4_total / sum(avg_dep_core4_total); mu <- sum(w * cod4_total)
  gm <- tapply(w * cod4_total, owner, sum) / tapply(w, owner, sum); gw <- tapply(w, owner, sum)
  .(total_var = sum(w * (cod4_total - mu)^2), between_var = sum(gw * (gm - mu)^2))
}, by = month][, between_share := between_var / total_var]
save_tab(vd, "T17_variance_between_within_ownership.csv", "Var_decomp")
fact("between_owner_share_mean", p2(mean(vd$between_share, na.rm = TRUE), 1), "% дисперсії між групами власності")

# Top-5 influence: system cost with and without the 5 largest deposit takers.
t5 <- bank[is.finite(exp_core4_total) & is.finite(avg_dep_core4_total) & avg_dep_core4_total >= 1][
  , rk := frank(-dep_core4_total, ties.method = "first"), by = month][
  , .(all = ann[1] * sum(exp_core4_total) / sum(avg_dep_core4_total),
      ex_top5 = ann[1] * sum(exp_core4_total[rk > 5]) / sum(avg_dep_core4_total[rk > 5]),
      top5 = ann[1] * sum(exp_core4_total[rk <= 5]) / sum(avg_dep_core4_total[rk <= 5])), by = month]
save_tab(t5, "T17b_top5_influence.csv", "Top5")
fact("top5_vs_rest_mean_war", sprintf("топ-5 %s%% vs інші %s%%", p2(t5[month >= WAR_START, mean(top5)]), p2(t5[month >= WAR_START, mean(ex_top5)])))

# Persistence of bank pricing position (rank correlation year over year).
rk <- bk[, .(nkb, r = frank(cod4_total) / .N), by = month]
rk <- merge(rk, rk[, .(nkb, month = add_months(month, 12L), r_l12 = r)], by = c("nkb", "month"))
pers <- rk[, .(spearman_12m = cor(r, r_l12, method = "spearman"), n = .N), by = month]
save_tab(pers, "T17c_rank_persistence_12m.csv", "Persistence")
fact("rank_persistence_12m_mean", sprintf("%.2f", mean(pers$spearman_12m, na.rm = TRUE)))

# ======================= 2.1.9 STRUCTURAL BREAKS ==============================
bp_series <- c(cod4_total = "Вартість депозитів", demand_share = "Частка коштів на вимогу", fx_share = "Валютна частка",
               cd_to_dep = "ДС НБУ / депозити", ovdp_to_dep = "ОВДП / депозити", policy_avg = "Облікова ставка")
bp_series <- bp_series[names(bp_series) %in% names(sys)]
sys[, wedge_policy := cod4_total - policy_avg]; bp_series <- c(bp_series, wedge_policy = "Клин CoD − облікова")
bp_out <- rbindlist(lapply(names(bp_series), function(v) {
  z <- sys[is.finite(get(v)), .(month, y = get(v))]
  if (nrow(z) < 36) return(NULL)
  h <- max(6L, floor(.10 * nrow(z)))
  bp <- tryCatch(breakpoints(y ~ 1, data = z, h = h), error = function(e) NULL); if (is.null(bp)) return(NULL)
  bic <- summary(bp)$RSS["BIC", ]; m <- as.integer(names(bic)[which.min(bic)])
  if (m == 0) return(data.table(series = bp_series[v], n_breaks = 0L))
  b <- breakpoints(bp, breaks = m)$breakpoints
  ci <- tryCatch(confint(bp, breaks = m)$confint, error = function(e) matrix(NA, m, 3))
  seg <- c(0, b, nrow(z))
  data.table(series = bp_series[v], n_breaks = m, break_month = z$month[b],
             ci_low = z$month[pmax(1, ci[, 1])], ci_high = z$month[pmin(nrow(z), ci[, 3])],
             mean_before = vapply(seq_along(b), function(k) mean(z$y[(seg[k] + 1):seg[k + 1]]), 0),
             mean_after  = vapply(seq_along(b), function(k) mean(z$y[(seg[k + 1] + 1):seg[k + 2]]), 0))
}), fill = TRUE)
save_tab(bp_out, "T19_structural_breaks_bai_perron.csv", "Breaks")
if (nrow(bp_out) && "break_month" %in% names(bp_out)) {
  bpp <- bp_out[!is.na(break_month)]
  p <- ggplot(bpp, aes(y = series)) + geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = .25, colour = PAL[1], linewidth = .5) +
    geom_point(aes(x = break_month), colour = INK, size = 2.2) + geom_vline(xintercept = WAR_START, linetype = 2, colour = PAL[8]) +
    labs(title = "Структурні розриви (Bai–Perron, кількість за BIC) та 95% довірчі інтервали",
         subtitle = "Модель зсуву середнього; червона лінія — початок повномасштабної війни", x = NULL, y = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "F17_structural_breaks.png", 9, 4.6)
}
reg_sum <- merge(sys[, .(month, regime, cod4_total, demand_share, fx_share, cd_to_dep, ovdp_to_dep, policy_avg)],
                 D4$month[, .(month, delta, price, mix)], by = "month", all.x = TRUE)[
  , c(.(months = .N), lapply(.SD, mean, na.rm = TRUE)), by = regime, .SDcols = c("cod4_total", "demand_share", "fx_share", "cd_to_dep", "ovdp_to_dep", "policy_avg", "delta", "price", "mix")]
save_tab(reg_sum, "T20_regime_summary.csv", "Regime_summary")

# ======================= 2.1.10 DESCRIPTIVE PANEL ASSOCIATIONS ===============
# Within estimator with bank FE (+ month FE), cluster-robust (bank) SE; uses
# {fixest} when installed, otherwise an exact alternating-projections solver.
fe_ols <- function(dt, y, x, fe, cluster, weights = NULL) {
  cols_all <- c(y, x, fe, cluster); cols_yx <- c(y, x)
  d <- dt[complete.cases(dt[, ..cols_all])]
  w <- if (is.null(weights)) rep(1, nrow(d)) else d[[weights]] / mean(d[[weights]])
  Z <- as.matrix(d[, ..cols_yx])
  for (it in 1:500) {
    Z0 <- Z
    for (f in fe) { g <- d[[f]]; Z <- Z - apply(Z * w, 2, function(col) (rowsum(col, g) / rowsum(w, g))[as.character(g), 1]) }
    if (max(abs(Z - Z0)) < 1e-10) break
  }
  yv <- Z[, 1]; X <- Z[, -1, drop = FALSE]
  XtWX <- crossprod(X * sqrt(w)); b <- solve(XtWX, crossprod(X * w, yv)); u <- as.vector(yv - X %*% b)
  cl <- d[[cluster]]; G <- length(unique(cl)); n <- nrow(X); k <- ncol(X)
  meat <- crossprod(rowsum(X * (w * u), cl)); bread <- solve(XtWX)
  V <- bread %*% meat %*% bread * G / (G - 1) * (n - 1) / (n - k)
  se <- sqrt(diag(V))
  data.table(term = colnames(X), estimate = as.vector(b), std.error = se, t = as.vector(b) / se,
             p.value = 2 * pt(-abs(as.vector(b) / se), df = G - 1), n_obs = n, n_banks = G,
             within_r2 = 1 - sum(w * u^2) / sum(w * (yv - weighted.mean(yv, w))^2))
}
rd <- bank[is.finite(cod4_total) & !dq_rate_out_of_bounds & dep_core4_total > 0]
rd[, `:=`(y = winsor(cod4_total), demand = winsor(demand_share), fx = winsor(fx_share), hh = winsor(hh_share),
          size = log(pmax(size_base, 1)), cd = winsor(cd_to_dep), ovdp = winsor(ovdp_to_dep), month_f = as.character(month))]
rd <- merge(rd, sys[, .(month, policy = policy_avg)], by = "month")
X0 <- c("demand", "fx", "hh", "size", "cd", "ovdp")
models <- list(
  A_bankFE_policy      = list(x = c(X0, "policy"), fe = "nkb", w = NULL),
  B_bank_monthFE       = list(x = X0, fe = c("nkb", "month_f"), w = NULL),
  C_bank_monthFE_wtd   = list(x = X0, fe = c("nkb", "month_f"), w = "avg_dep_core4_total"),
  D_bank_monthFE_war   = list(x = X0, fe = c("nkb", "month_f"), w = NULL, sub = quote(month >= WAR_START)))
reg_out <- rbindlist(lapply(names(models), function(nm) {
  m <- models[[nm]]; d <- if (!is.null(m$sub)) rd[eval(m$sub)] else rd
  r <- tryCatch(fe_ols(d, "y", m$x, m$fe, "nkb", m$w), error = function(e) { warning(nm, ": ", conditionMessage(e)); NULL })
  if (!is.null(r)) r[, model := nm]; r
}), fill = TRUE)
save_tab(reg_out, "T21_panel_associations.csv", "Panel_FE")

# ======================= EXPORT ===============================================
save_tab(STATUS, "T00_blocks_status.csv", "Status")
save_tab(FACTS, "T00_key_facts.csv", "Key_facts")
wb <- openxlsx::createWorkbook()
hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#1F4E78", fontColour = "white", wrapText = TRUE)
for (nm in names(TABLES)) {
  sh <- substr(nm, 1, 31); openxlsx::addWorksheet(wb, sh)
  openxlsx::writeData(wb, sh, as.data.frame(TABLES[[nm]]), headerStyle = hs, withFilter = TRUE)
  openxlsx::freezePane(wb, sh, firstRow = TRUE)
}
openxlsx::saveWorkbook(wb, file.path(OUT, "Section_2_1_v2.xlsx"), overwrite = TRUE)
print(STATUS); print(FACTS)
log_msg("SECTION 2.1 v2 COMPLETE: ", length(list.files(FIG)), " figures, ", length(TABLES), " tables -> ", OUT)
