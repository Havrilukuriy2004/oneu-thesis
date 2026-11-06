# ============================================================================
# 05_section_2_1_assets_joint.R — ASSET SIDE (loans, NBU CDs, OVDP) and
#                                   JOINT balance-sheet analysis
# ----------------------------------------------------------------------------
# Run after 02 (marts) and 03 (cycles table T18). Independent of 04.
# Output: output_nbu/section_2_1_v2/{tables,figures}/A*  + Section_2_1_v2_assets.xlsx
#
# Design of the lending-premium block follows the author's correspondence with
# K. Vilerts (May 2026) and Vilerts et al. (ECB Research Bulletin, Oct 2025):
#   lending rate = relevant reference ("risk-free") rate + benchmark-specific premium.
#   Ukraine has no OIS curve, therefore several benchmarks side by side:
#     (1) NBU overnight CD rate (= policy rate under the floor system) — BASELINE
#         for short-term UAH lending and liquidity allocation;
#     (2) maturity-matched OVDP zero rate (NBU NSS curve) — alternative asset /
#         robustness for longer loans (contains sovereign + liquidity risk);
#     (3) UIRD 3M (posted household deposit rates; local file UIRD.xls) or its MIR proxy;
#     (4) FX loans: NBU NS curve for FX OVDP (sovereign-adjusted FX rate).
#   5-7-9 loans: the bank earns the programme base rate (UIRD 3M + margin), the
#   borrower pays 0/5/7/9 % and the state compensates the difference. Without
#   loan-level compensation data the base rate is rebuilt from the programme rules;
#   volumes/outstanding come from the ФРП weekly report (local_inputs/5-7-9.xlsx).
#
# Blocks
#   A1  Structure of interest-earning assets (system, ownership groups)
#   A2  Book yields: loans (corp/HH), NBU CDs, refinanceable securities, total IEA
#   A3  New-business loan rates (MIR) by sector × currency × maturity; book vs new;
#       stock-flow repricing speed λ of loan books
#   A4  Benchmark-specific lending premia (multi-benchmark), UAH and FX
#   A5  Exact price/mix decomposition of the asset yield
#   A6  NIM bridge: yield on IEA − cost of IBL; decomposition of ΔNIM
#   A7  Credit cost, NPL and risk-adjusted lending spread vs CD carry (→ ch. 3)
#   A8  Asset-side convexity & asymmetry; revealed repricing gap preview (→ 2.3)
#   A9  Bank-level matching of asset and liability rate sensitivities (DSS 2021)
#   A10 5-7-9 programme: share in new loans and in the book; MIR rate corrected to
#       the economic (bank-received) rate; premia raw vs corrected
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_models_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_local_inputs_v6.R"), encoding = "UTF-8")
.need("sandwich")
init_output("A")
CAP <- "Джерело: розрахунки автора за даними НБУ (OSB, фінзвітність банків, MIR, NSS-крива, облікова ставка)."

sys  <- store_read("mart_system_month_v6");  sys[, month := as.Date(month)]
bank <- store_read("mart_bank_month_v6");    bank[, month := as.Date(month)]
sys <- sys[month >= START_MONTH]; bank <- bank[month >= START_MONTH]; setorder(sys, month)
has <- function(dt, v) v %in% names(dt) && any(is.finite(dt[[v]]))
col_or_na <- function(dt, v) if (v %in% names(dt)) dt[[v]] else rep(NA_real_, nrow(dt))
econ_month <- function(dt) add_months(month_floor(parse_nbu_date(dt)), -1L)
cyc <- tryCatch({ z <- fread(file.path(TAB, "T18_policy_cycles.csv")); z[, `:=`(start = as.Date(start), end = as.Date(end))]; z },
                error = function(e) data.table())

# ====================== 0. BANK FINANCIAL STATEMENTS (banksfinrep) ===========
fin <- unique(store_read("raw_banksfinrep"))
FIN_IDS <- c(loans_le = "BS1_AssetsLoansLE", loans_hh = "BS1_AssetsLoansIndiv", loans_all = "BS1_AssetsLoansClients",
             prov_le = "BS1_AssetsProvLoansLE", prov_hh = "BS1_AssetsProvLoansIndiv",
             sec_ac = "BS1_AssetsSecurAC", sec_fvoci = "BS1_AssetsSecurFVOCI", sec_fvpl = "BS1_FinAssetsFairVal",
             other_banks = "BS1_AssetsOtherBanks", due_nbu = "BS1_AssetsAmountsDueNBU", cash = "BS1_AssetsCash",
             assets = "BS1_AssetsTotal", liab_cust = "BS1_LiabCust", liab_banks = "BS1_LiabBanks", liab_nbu = "BS1_LiabNBU",
             liab_debtsec = "BS1_LiabDebtSecur", liab_sub = "BS1_LiabSubDebt", liab_oth = "BS1_LiabOtherBorrFunds",
             equity = "BS1_CapitalTotal",
             ii = "BS1_InterestIncome", ii_le = "BS1_InterestIncomeLE", ii_hh = "BS1_InterestIncomeIndiv", ii_sec = "BS1_InterestIncomeSec",
             ie = "BS1_InterestCosts", nii = "BS1_NetInterIncomeCosts", alloc_prov = "BS1_AllocProv")
YTD <- c("ii", "ii_le", "ii_hh", "ii_sec", "ie", "nii", "alloc_prov")
fb <- fin[id_api %in% FIN_IDS, .(value = sum(num(value), na.rm = TRUE)), by = .(month = econ_month(dt), nkb = as.character(nkb), id_api)]
fb[, var := names(FIN_IDS)[match(id_api, FIN_IDS)]]
fb <- dcast(fb, month + nkb ~ var, value.var = "value", fill = NA)
for (v in setdiff(names(FIN_IDS), names(fb))) fb[, (v) := NA_real_]
setorder(fb, nkb, month)
# YTD -> monthly flows (January = YTD; otherwise difference with the previous calendar month)
fb[, prev_m := shift(month), by = nkb]
for (v in YTD) fb[, (paste0(v, "_m")) := fifelse(month(month) == 1L, get(v),
                                         fifelse(prev_m == add_months(month, -1L), get(v) - shift(get(v)), NA_real_)), by = nkb]
fb[, `:=`(sec_all = rowSums(cbind(sec_ac, sec_fvoci, sec_fvpl), na.rm = TRUE),
          iea = rowSums(cbind(loans_all, sec_ac, sec_fvoci, sec_fvpl, other_banks), na.rm = TRUE),
          ibl = rowSums(cbind(liab_cust, liab_banks, liab_nbu, liab_debtsec, liab_sub, liab_oth), na.rm = TRUE))]
# OSB stocks: NBU CDs and OVDP (cleanly identified by account)
osb_assets <- bank[, .(month, nkb, cd_osb = cd_nbu_total, ovdp_osb = ovdp_total, inc_cd = col_or_na(bank, "inc_cd_nbu_total"),
                       inc_refin = col_or_na(bank, "inc_refin_sec_total"), dep4 = dep_core4_total, owner, cod4_total, exp_core4_total,
                       avg_dep_core4_total, demand_share, size_base,
                       y_loans_corp_osb = col_or_na(bank, "y_loans_corp_total"), y_loans_hh_osb = col_or_na(bank, "y_loans_hh_total"))]
fb <- merge(fb, osb_assets, by = c("month", "nkb"), all.x = TRUE)
fb[, sec_other := pmax(sec_all - fcoalesce(cd_osb, 0) - fcoalesce(ovdp_osb, 0), 0)]
avg2 <- function(x, m) { xl <- shift(x); ml <- shift(m); fifelse(ml == add_months(m, -1L), (x + xl) / 2, NA_real_) }
for (v in c("loans_le", "loans_hh", "loans_all", "sec_all", "iea", "ibl", "assets", "cd_osb", "ovdp_osb", "sec_other"))
  fb[, (paste0("avg_", v)) := avg2(get(v), month), by = nkb]
fb[, ann := ann_factor(month)]
store_write(fb, "mart_bank_assets_v6")

# System aggregates on a matched sample (flow and average stock both present).
sr <- function(flow, stock, ann) { ok <- is.finite(flow) & is.finite(stock) & stock > 1; ann[1] * sum(flow[ok]) / sum(stock[ok]) }
S <- fb[, .(
  loans_le = sum(loans_le, na.rm = TRUE), loans_hh = sum(loans_hh, na.rm = TRUE), prov_le = sum(prov_le, na.rm = TRUE), prov_hh = sum(prov_hh, na.rm = TRUE),
  cd = sum(cd_osb, na.rm = TRUE), ovdp = sum(ovdp_osb, na.rm = TRUE), sec_other = sum(sec_other, na.rm = TRUE), sec_all = sum(sec_all, na.rm = TRUE),
  other_banks = sum(other_banks, na.rm = TRUE), due_nbu_cash = sum(due_nbu, na.rm = TRUE) + sum(cash, na.rm = TRUE),
  assets = sum(assets, na.rm = TRUE), iea = sum(iea, na.rm = TRUE), ibl = sum(ibl, na.rm = TRUE),
  y_le = sr(ii_le_m, avg_loans_le, ann), y_hh = sr(ii_hh_m, avg_loans_hh, ann), y_sec = sr(ii_sec_m, avg_sec_all, ann),
  y_iea = sr(ii_m, avg_iea, ann), c_ibl = sr(ie_m, avg_ibl, ann), nim = sr(nii_m, avg_iea, ann),
  credit_cost = sr(alloc_prov_m, avg_loans_all, ann),
  y_cd = sr(inc_cd, avg_cd_osb, ann), y_ovdp = sr(inc_refin, avg_ovdp_osb, ann),
  nii_m = sum(nii_m, na.rm = TRUE)), by = month]
setorder(S, month)
# Break in banksfinrep: "BS1_InterestIncomeSec" exists only from report date 2020-10; before that,
# income on securities sat inside the legal-entity line. Finrep-based corporate-loan and securities
# yields are therefore comparable only from 2021-01 (first full YTD year). OSB yields have no break.
FINREP_BREAK <- as.Date("2021-01-01")
S[month < FINREP_BREAK, `:=`(y_le = NA_real_, y_sec = NA_real_)]
status("A2 finrep break", "info", "дохідність кредитів СГ і ЦП за фінзвітністю — з 2021-01 (до 2020-10 дохід за ЦП входив у рядок юросіб)")
S[, `:=`(npl_le_cov = -prov_le / loans_le, npl_hh_cov = -prov_hh / loans_hh)]
# Macro / market
mac_cols <- intersect(c("month", "policy_avg", "policy_eom", "cd_on_rate_avg", "refi_on_rate_avg", "uonia_avg", "cod4_total", "cod4_uah", "cod4_fx",
                        "cpi_yoy", "nb_dep_hh_lt1y_uah", "nb_dep_hh_term_uah",
                        grep("^zc_(uah|fx)_.*_avg$", names(sys), value = TRUE),
                        grep("^ovdp_uah_", names(sys), value = TRUE), "usd_eom"), names(sys))
S <- merge(S, sys[, ..mac_cols], by = "month", all.x = TRUE)
# OSB-based book loan yields if loan principal accounts were downloaded
if (has(sys, "y_loans_corp_total")) S <- merge(S, sys[, .(month, y_le_osb = y_loans_corp_total, y_hh_osb = y_loans_hh_total)], by = "month", all.x = TRUE)
store_write(S, "mart_system_assets_v6")   # system asset yields, NIM, credit cost — used by 06 (dynamics)

# ====================== A1. STRUCTURE OF INTEREST-EARNING ASSETS ==============
st <- S[, .(month, `Кредити СГ (валові)` = loans_le, `Кредити ФО (валові)` = loans_hh, `Депсертифікати НБУ` = cd, `ОВДП` = ovdp,
            `Інші ЦП` = sec_other, `Кошти в інших банках` = other_banks)]
stl <- melt(st, id.vars = "month"); stl[, share := value / sum(value), by = month]
save_tab(dcast(stl, month ~ variable, value.var = "share"), "A1_iea_structure_shares.csv", "A1_structure")
p <- ggplot(stl, aes(month, share, fill = variable)) + geom_area(colour = "white", linewidth = .2) + scale_fill_manual(values = PAL[1:6]) +
  scale_y_continuous(labels = label_percent(1)) +
  labs(title = "Структура процентних активів банківської системи", subtitle = "Кредити — валові (фінзвітність); ДС і ОВДП — OSB (рахунки 143–145, 140–142)",
       x = NULL, y = "Частка", caption = CAP) + theme_thesis()
save_fig(p, "A1_iea_structure.png")
f0 <- stl[month == min(month)]; fL <- stl[month == max(month)]
for (v in unique(stl$variable)) fact(paste0("A1_share_", v), sprintf("%s%% → %s%%", p2(f0[variable == v, share], 1), p2(fL[variable == v, share], 1)))
# Ownership groups (latest and war-period average)
og <- fb[!is.na(owner), .(loans = sum(loans_all, na.rm = TRUE), cd = sum(cd_osb, na.rm = TRUE), ovdp = sum(ovdp_osb, na.rm = TRUE),
                          sec_other = sum(sec_other, na.rm = TRUE), assets = sum(assets, na.rm = TRUE)), by = .(month, owner)]
og[, `:=`(loans_sh = loans / assets, cd_sh = cd / assets, ovdp_sh = ovdp / assets)]
save_tab(og, "A1b_assets_by_ownership.csv", "A1_ownership")
p <- ggplot(melt(og[, .(month, owner, `Кредити / активи` = loans_sh, `ДС НБУ / активи` = cd_sh, `ОВДП / активи` = ovdp_sh)], id.vars = c("month", "owner")),
            aes(month, value, colour = owner)) + geom_line(linewidth = .7) + facet_wrap(~variable, scales = "free_y") +
  scale_colour_manual(values = PAL[1:3], labels = c(Foreign = "Іноземні", Private = "Приватні", State = "Державні")) + scale_y_continuous(labels = label_percent(1)) +
  labs(title = "Розподіл активів за групами власності", x = NULL, y = NULL, caption = CAP) + theme_thesis()
save_fig(p, "A1b_assets_by_ownership.png", 10, 4.6)

# ====================== A2. BOOK YIELDS ======================================
yl <- S[, .(month, `Кредити СГ` = fcoalesce(col_or_na(S, "y_le_osb"), y_le), `Кредити ФО` = fcoalesce(col_or_na(S, "y_hh_osb"), y_hh),
            `ДС НБУ` = y_cd, `ЦП, що рефінансуються (ОВДП)` = y_ovdp, `Усі процентні активи` = y_iea, `Вартість депозитів (CoD)` = cod4_total,
            `Облікова ставка` = policy_avg)]
save_tab(yl, "A2_book_yields.csv", "A2_yields")
yll <- melt(yl, id.vars = "month")[is.finite(value)]
p <- ggplot(yll, aes(month, value, colour = variable, linewidth = variable %in% c("Усі процентні активи", "Вартість депозитів (CoD)"))) + geom_line() +
  scale_linewidth_manual(values = c(.55, 1), guide = "none") + scale_colour_manual(values = c(PAL[1:5], INK, PAL[7])) +
  scale_y_continuous(labels = label_percent(1)) + guides(colour = guide_legend(nrow = 3)) +
  labs(title = "Книжкова дохідність процентних активів і вартість фондування",
       subtitle = "Кредити: процентний дохід / середній валовий портфель (фінзвітність; OSB, якщо завантажено); ДС і ЦП — OSB",
       x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
save_fig(p, "A2_book_yields.png", 9, 5.4)
L <- S[.N]
fact("A2_yields_last", sprintf("кредити СГ %s%%, ФО %s%%, ДС %s%%, ОВДП %s%%, усі активи %s%%, CoD %s%%",
     p2(L$y_le), p2(L$y_hh), p2(L$y_cd), p2(L$y_ovdp), p2(L$y_iea), p2(L$cod4_total)))
fact("A2_ovdp_book_vs_policy_mean_war", p2(S[month >= WAR_START, mean(y_ovdp - policy_avg, na.rm = TRUE)]), "в.п.: книжкова дохідність ОВДП мінус облікова")

# ====================== A3. NEW-BUSINESS LOAN RATES (MIR) ====================
mir <- unique(store_read("raw_mir"))
LOAN_IDS <- c(New_Loans_NFinCorp = "СГ", New_Loans_Households = "ФО")
ML <- data.table()
if (nrow(mir)) {
  ml <- mir[id_api %in% names(LOAN_IDS) & odkodter == "total" & odk111 == "total" & odf074 == "total" & odr030 %in% c("01", "02", "total")]
  # k140 = borrower size class; "#" = all borrowers (earlier versions took a random class in ~1/3 of cells)
  if ("k140" %in% names(ml)) ml <- ml[is.na(k140) | k140 %in% c("#", "total", "")]
  ml <- unique(ml, by = c("dt", "id_api", "odr030", "ods180", "tzep"))
  ml <- ml[, .(value = num(value)[1]), by = .(month = econ_month(dt), id_api, odr030, ods180, tzep)]
  ML <- dcast(ml, month + id_api + odr030 + ods180 ~ tzep, value.var = "value")
  setnames(ML, c("f_m_ir", "f_m_v"), c("rate", "vol"), skip_absent = TRUE)
  ML[, `:=`(rate = rate / 100, sector = LOAN_IDS[id_api], ccy = c("01" = "UAH", "02" = "FX", total = "Усі")[odr030],
            bucket = c("10" = "овердрафт", "20" = "без овердрафту", "30" = "до 1 року", "40" = "1–5 років", "50" = "понад 5 років", total = "усього")[ods180])]
  save_tab(ML, "A3_mir_new_loans.csv", "A3_mir_loans")
  q <- ML[ccy == "UAH" & sector == "СГ" & bucket %in% c("овердрафт", "до 1 року", "1–5 років", "понад 5 років")]
  q <- q[CJ(month = seq(min(q$month), max(q$month), by = "month"), bucket = unique(q$bucket)), on = .(month, bucket)]   # MIR gap = break
  p <- ggplot(q, aes(month, rate, colour = bucket)) + geom_line(linewidth = .65, na.rm = TRUE) + scale_colour_manual(values = PAL[1:4]) +
    scale_y_continuous(labels = label_percent(1)) +
    labs(title = "Ставки за новими гривневими кредитами нефінансовим корпораціям за строками", subtitle = "MIR НБУ, середньозважені ставки нових операцій",
         x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
  save_fig(p, "A3_new_loan_rates_nfc_uah.png")
  vol <- ML[sector == "СГ" & ccy == "UAH" & bucket %in% c("овердрафт", "до 1 року", "1–5 років", "понад 5 років") & is.finite(vol)]
  vol[, share := vol / sum(vol), by = month]
  save_tab(dcast(vol, month ~ bucket, value.var = "share"), "A3b_nfc_uah_new_loans_maturity_mix.csv", "A3_maturity_mix")
  lastq <- vol[month >= add_months(max(month), -2L), .(vol = sum(vol), rate = wmean(rate, vol)), by = bucket][, share := vol / sum(vol)]
  fact("A3_nfc_uah_new_last3m", paste(sprintf("%s: %s%% обсягу, %s%%", lastq$bucket, p2(lastq$share, 1), p2(lastq$rate)), collapse = "; "))
}
# Stock-flow repricing speed of loan books: Δbook = λ (new − book_{t−1})
nb_pick <- function(sec, cc = "Усі") ML[sector == sec & ccy == cc & bucket == "усього", .(month, new = rate)]
lam_tab <- rbindlist(lapply(list(c("СГ", "y_le"), c("ФО", "y_hh")), function(z) {
  if (!nrow(ML)) return(NULL)
  # finrep book yields are YTD-differenced and noisy: smooth both series (3-month MA);
  # new-business rate excludes overdrafts, UAH (closest to the bulk of the book).
  osb_book <- col_or_na(S, paste0(z[2], "_osb"))
  bk <- S[, .(month, book = frollmean(fcoalesce(osb_book, get(z[2])), 3))]
  nb <- ML[sector == z[1] & ccy == "UAH" & bucket == "без овердрафту", .(month, new = rate)]
  setorder(nb, month); nb[, new := frollmean(new, 3)]
  d <- merge(bk, nb, by = "month"); setorder(d, month)
  d[, `:=`(db = book - shift(book), gap = new - shift(book))]; d <- d[is.finite(db) & is.finite(gap)]
  if (nrow(d) < 24) return(NULL)
  rbindlist(lapply(list(all = d, prewar = d[month < WAR_START], war = d[month >= WAR_START]), function(s) {
    l <- sum(s$gap * s$db) / sum(s$gap^2); r <- s$db - l * s$gap
    data.table(lambda = l, se = sqrt(sum(r^2) / (nrow(s) - 1) / sum(s$gap^2)), n = nrow(s), horizon_m = 1 / l)
  }), idcol = "period")[, `:=`(sector = z[1], source = if (all(is.na(osb_book))) "фінзвітність (згладжено)" else "OSB")]
}))
if (nrow(lam_tab)) lam_tab[, reliable := lambda > 0.01 & lambda < 0.6 & lambda > 2 * se]
status("A3 lambda loans", if (nrow(lam_tab) && all(lam_tab[period == "all", reliable])) "ok" else "unreliable",
       "λ кредитів надійно оцінюється лише з OSB-рахунками кредитів (20xx–24xx) — після 01_nbu_download_v6.R")
save_tab(lam_tab, "A3c_loan_book_repricing_lambda.csv", "A3_lambda")
for (s in unique(lam_tab$sector)) { z <- lam_tab[sector == s]
  fact(paste0("A3_lambda_loans_", s), sprintf("λ=%.3f (≈%.1f міс.); до війни %.1f, у війну %.1f міс.; надійно: %s", z[period == "all", lambda], z[period == "all", horizon_m],
                                             z[period == "prewar", horizon_m], z[period == "war", horizon_m], z[period == "all", reliable])) }

# UIRD (monthly, decimal): prefer the macro mart (02), else read the local file directly
UIRD_SRC <- "macro_monthly_v6"
UIRD_M <- if (has(sys, "uird_uah_3m_avg")) sys[, .(month, uird3m = uird_uah_3m_avg, uird12m = col_or_na(sys, "uird_uah_12m_avg"))] else {
  u <- read_uird_local()
  if (is.null(u)) data.table(month = as.Date(character()), uird3m = numeric(), uird12m = numeric()) else {
    UIRD_SRC <- u$file; dcast(u$monthly[ccy == "uah" & tenor %in% c("3m", "12m")], month ~ paste0("uird", tenor), value.var = "avg") }
}

# ====================== A4. BENCHMARK-SPECIFIC LENDING PREMIA ================
# premium(b, m) = new loan rate in maturity bucket m − benchmark b matched to m.
if (nrow(ML)) {
  B <- S[, .(month, policy = policy_avg, cd_on = fcoalesce(col_or_na(S, "cd_on_rate_avg"), policy_avg),
             uonia = col_or_na(S, "uonia_avg"),
             zc6m = col_or_na(S, "zc_uah_6m_avg"), zc1y = col_or_na(S, "zc_uah_1y_avg"), zc3y = col_or_na(S, "zc_uah_3y_avg"), zc7y = col_or_na(S, "zc_uah_7y_avg"),
             fx1y = col_or_na(S, "zc_fx_1y_avg"), fx3y = col_or_na(S, "zc_fx_3y_avg"),
             uird_proxy = col_or_na(S, "nb_dep_hh_lt1y_uah"))]
  # UIRD 3M: macro mart (02) or directly from data_nbu/local_inputs/UIRD.xls; calendar month = month of the quote
  B <- merge(B, UIRD_M[, .(month, uird3m)], by = "month", all.x = TRUE)
  if (any(is.finite(B$uird3m))) status("A4 UIRD", "ok", UIRD_SRC) else
    status("A4 UIRD", "proxy", "UIRD.xls не знайдено в data_nbu/local_inputs; використано MIR: нові депозити ФО до 1 року, UAH")
  B[, uird := fcoalesce(uird3m, uird_proxy)]
  ov <- B[is.finite(uird3m) & is.finite(uird_proxy)]
  if (nrow(ov) > 24) fact("A4_uird_vs_mir_proxy", sprintf("UIRD 3M − MIR (нові депозити ФО <1 р., UAH): у середньому %s в.п.; кореляція рівнів %.2f, місячних змін %.2f; %d міс.",
       p2(mean(ov$uird3m - ov$uird_proxy)), cor(ov$uird3m, ov$uird_proxy), cor(diff(ov$uird3m), diff(ov$uird_proxy)), nrow(ov)))
  match_ovdp <- function(bucket, B) switch(bucket, "овердрафт" = B$cd_on, "до 1 року" = fcoalesce(B$zc6m, B$zc1y),
                                         "1–5 років" = B$zc3y, "понад 5 років" = B$zc7y, rep(NA_real_, nrow(B)))
  P <- rbindlist(lapply(c("овердрафт", "до 1 року", "1–5 років", "понад 5 років"), function(bk) {
    r <- ML[sector == "СГ" & ccy == "UAH" & bucket == bk, .(month, rate, vol)]
    z <- merge(r, B, by = "month")
    rbind(z[, .(month, bucket = bk, benchmark = "ДС НБУ овернайт (базовий)", premium = rate - cd_on, vol)],
          z[, .(month, bucket = bk, benchmark = "ОВДП, узгоджений строк", premium = rate - match_ovdp(bk, z), vol)],
          z[, .(month, bucket = bk, benchmark = "UIRD 3M (оголошені ставки ФО)", premium = rate - uird, vol)])
  }))
  PF <- rbindlist(lapply(c("до 1 року", "1–5 років"), function(bk) {
    r <- ML[sector == "СГ" & ccy == "FX" & bucket == bk, .(month, rate, vol)]; z <- merge(r, B, by = "month")
    z[, .(month, bucket = bk, benchmark = "FX-крива ОВДП НБУ (NS)", premium = rate - (if (bk == "до 1 року") fx1y else fx3y), vol)]
  }))
  save_tab(rbind(P[, ccy := "UAH"], PF[, ccy := "FX"]), "A4_lending_premia_multi_benchmark.csv", "A4_premia")
  pa <- P[is.finite(premium), .(premium = wmean(premium, vol)), by = .(month, benchmark)]
  pa <- pa[CJ(month = seq(min(pa$month), max(pa$month), by = "month"), benchmark = unique(pa$benchmark)), on = .(month, benchmark)]  # keep MIR gaps as breaks
  p <- ggplot(pa, aes(month, premium, colour = benchmark)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + geom_line(linewidth = .75, na.rm = TRUE) +
    scale_colour_manual(values = PAL[c(1, 2, 3)]) + scale_y_continuous(labels = label_percent(1)) + guides(colour = guide_legend(nrow = 2)) +
    labs(title = "Кредитна премія залежить від обраного орієнтира: нові гривневі кредити СГ",
         subtitle = "Нові кредити (MIR, зважено за строковими кошиками) мінус орієнтир. MIR містить 5-7-9 — корекцію див. A10",
         x = NULL, y = "в.п.", caption = CAP) + theme_thesis()
  save_fig(p, "A4_premia_by_benchmark.png")
  pb <- P[is.finite(premium) & month >= add_months(max(month), -11L), .(premium = mean(premium)), by = .(bucket, benchmark)]
  save_tab(dcast(pb, bucket ~ benchmark, value.var = "premium"), "A4b_premia_last12m_by_maturity.csv", "A4_premia_last12m")
  for (b in unique(pa$benchmark)) fact(paste0("A4_premium_", b), sprintf("останні 12 міс.: %s в.п.; з 2022-02: %s в.п.; 2018-01–2022-01: %s в.п.",
     p2(pa[benchmark == b & month >= add_months(max(month), -11L), mean(premium, na.rm = TRUE)]), p2(pa[benchmark == b & month >= WAR_START, mean(premium, na.rm = TRUE)]),
     p2(pa[benchmark == b & month < WAR_START, mean(premium, na.rm = TRUE)])))
  # Pass-through of benchmark vs premium in each cycle (Vilerts et al. logic): Δrate = Δbenchmark + Δpremium
  if (nrow(cyc)) {
    bb <- merge(ML[sector == "СГ" & ccy == "UAH" & bucket == "без овердрафту", .(month, rate)], B[, .(month, cd_on)], by = "month")
    bb[, cycle := cyc$cycle[findInterval(as.numeric(month), as.numeric(cyc$start))]]
    cd <- bb[, .(start = min(month), end = max(month), d_rate = rate[.N] - rate[1], d_benchmark = cd_on[.N] - cd_on[1]), by = cycle]
    cd[, d_premium := d_rate - d_benchmark]; cd <- merge(cd, cyc[, .(cycle, regime)], by = "cycle")
    save_tab(cd, "A4c_rate_change_benchmark_vs_premium_by_cycle.csv", "A4_cycles")
  }
}

# ====================== A5. ASSET YIELD: PRICE / MIX =========================
cells <- rbind(S[, .(month, cell = "loans_le", rate = fcoalesce(col_or_na(S, "y_le_osb"), y_le), w = loans_le)],
               S[, .(month, cell = "loans_hh", rate = fcoalesce(col_or_na(S, "y_hh_osb"), y_hh), w = loans_hh)],
               S[, .(month, cell = "cd", rate = y_cd, w = cd)],
               S[, .(month, cell = "ovdp", rate = y_ovdp, w = ovdp)])
# keep only months where all four classes have a yield and a stock (no entry/exit of cells)
cells <- cells[is.finite(rate) & is.finite(w) & w > 0][, if (.N == 4L) .SD, by = month]
DA <- midpoint_decomp(cells)
DA$month[, `:=`(cum_price = cumsum(fcoalesce(price, 0)), cum_mix = cumsum(fcoalesce(mix, 0)), cum_delta = cumsum(fcoalesce(delta, 0)))]
save_tab(DA$month, "A5_asset_yield_price_mix.csv", "A5_price_mix")
if (nrow(cyc)) {
  DA$month[, cycle := cyc$cycle[findInterval(as.numeric(month), as.numeric(cyc$start))]]
  ac <- merge(DA$month[, .(price = sum(price), mix = sum(mix), delta = sum(delta)), by = cycle], cyc[, .(cycle, regime, start, end, months)], by = "cycle")
  save_tab(ac, "A5b_asset_price_mix_by_cycle.csv", "A5_cycles")
}
p <- ggplot(melt(DA$month[, .(month, `Ціновий ефект` = cum_price, `Ефект структури` = cum_mix, `Загальна зміна` = cum_delta)], id.vars = "month"),
            aes(month, value, colour = variable)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + geom_line(linewidth = .8) +
  scale_colour_manual(values = c(PAL[1], PAL[2], INK)) + scale_y_continuous(labels = label_percent(.5)) +
  labs(title = "Дохідність процентних активів: накопичений ціновий і структурний ефекти",
       subtitle = "4 класи: кредити СГ, кредити ФО, ДС НБУ, ОВДП; точна midpoint-декомпозиція", x = NULL, y = "в.п.", caption = CAP) + theme_thesis()
save_fig(p, "A5_asset_price_mix.png")
fact("A5_asset_decomp_total", sprintf("Δ %s в.п. = ціна %s + структура %s; max|залишок| = %.1e", p2(sum(DA$month$delta, na.rm = TRUE)),
     p2(sum(DA$month$price, na.rm = TRUE)), p2(sum(DA$month$mix, na.rm = TRUE)), max(abs(DA$month$residual), na.rm = TRUE)))

# ====================== A6. NIM BRIDGE =======================================
# NIM = y_IEA − c_IBL · (IBL / IEA); ΔNIM = Δy_IEA − Δ[c_IBL · IBL/IEA]
nm <- S[, .(month, y_iea, c_ibl, lev = ibl / iea, nim)]
nm[, nim_identity := y_iea - c_ibl * lev]
setorder(nm, month)
nm[, `:=`(d_nim = nim_identity - shift(nim_identity), d_yield = y_iea - shift(y_iea),
          d_cost = -((lev + shift(lev)) / 2) * (c_ibl - shift(c_ibl)),
          d_leverage = -((c_ibl + shift(c_ibl)) / 2) * (lev - shift(lev)))]
nm[, `:=`(cum_yield = cumsum(fcoalesce(d_yield, 0)), cum_cost = cumsum(fcoalesce(d_cost, 0)), cum_leverage = cumsum(fcoalesce(d_leverage, 0)))]
save_tab(nm, "A6_nim_bridge.csv", "A6_nim")
p <- ggplot(melt(nm[, .(month, `Дохідність процентних активів` = y_iea, `Вартість процентних зобов'язань` = c_ibl, `NIM (ЧПД / процентні активи)` = nim)], id.vars = "month")[is.finite(value)],
            aes(month, value, colour = variable)) + geom_line(linewidth = .8) + scale_colour_manual(values = c(PAL[1], PAL[2], INK)) +
  scale_y_continuous(labels = label_percent(1)) +
  labs(title = "Чиста процентна маржа: дохідність активів проти вартості зобов'язань", subtitle = "Фінзвітність банків, місячні потоки в річному вимірі",
       x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
save_fig(p, "A6_nim.png")
fact("A6_nim_last", sprintf("NIM %s%% (дохідність %s%%, вартість %s%%)", p2(nm[.N, nim]), p2(nm[.N, y_iea]), p2(nm[.N, c_ibl])))
fact("A6_nim_prewar_war", sprintf("%s%% до війни / %s%% з 2022-02", p2(nm[month < WAR_START, mean(nim, na.rm = TRUE)]), p2(nm[month >= WAR_START, mean(nim, na.rm = TRUE)])))
fact("A6_bridge_since_start", sprintf("ΔNIM %s в.п.: дохідність %s, вартість %s, структура фондування %s", p2(sum(nm$d_nim, na.rm = TRUE)),
     p2(sum(nm$d_yield, na.rm = TRUE)), p2(sum(nm$d_cost, na.rm = TRUE)), p2(sum(nm$d_leverage, na.rm = TRUE))))

# ====================== A7. CREDIT COST, NPL, RISK-ADJUSTED SPREAD ===========
klk <- unique(store_read("raw_klk"))
if (nrow(klk)) {
  kk <- klk[id_api %in% c("Klk_UO", "Klk_UO_NPL", "Klk_FO", "Klk_FO_NPL"), .(value = sum(num(value), na.rm = TRUE)), by = .(month = econ_month(dt), id_api)]
  kk <- dcast(kk, month ~ id_api, value.var = "value")
  kk[, `:=`(npl_le = Klk_UO_NPL / Klk_UO, npl_hh = Klk_FO_NPL / Klk_FO)]
  S <- merge(S, kk[, .(month, npl_le, npl_hh)], by = "month", all.x = TRUE)
}
S[, credit_cost_12m := frollmean(credit_cost, 12)]
S[, `:=`(ra_spread_le = y_le - credit_cost_12m - y_cd, ra_spread_hh = y_hh - credit_cost_12m - y_cd, carry_cd = y_cd - cod4_total)]
save_tab(S[, .(month, y_le, y_hh, y_cd, cod4_total, credit_cost, credit_cost_12m, npl_le = col_or_na(S, "npl_le"), npl_hh = col_or_na(S, "npl_hh"),
               ra_spread_le, ra_spread_hh, carry_cd)], "A7_risk_adjusted_spreads.csv", "A7_risk_adj")
p <- ggplot(melt(S[, .(month, `Кредити СГ: дохідність − кредитна вартість − ДС` = ra_spread_le, `Кредити ФО: те саме` = ra_spread_hh,
                       `Керрі ДС: ДС − вартість депозитів` = carry_cd)], id.vars = "month")[is.finite(value)],
            aes(month, value, colour = variable)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + geom_line(linewidth = .8) +
  scale_colour_manual(values = PAL[c(1, 3, 2)]) + scale_y_continuous(labels = label_percent(1)) + guides(colour = guide_legend(nrow = 3)) +
  labs(title = "Чи винагороджує кредит ризик порівняно з депсертифікатами НБУ (попередньо до розділу 3)",
       subtitle = "Кредитна вартість = відрахування до резервів (12-міс. середнє) / валові кредити; книжкові дохідності",
       x = NULL, y = "в.п.", caption = CAP) + theme_thesis()
save_fig(p, "A7_risk_adjusted_spread.png", 9, 5.2)
fact("A7_ra_spread_le_last", p2(S[is.finite(ra_spread_le)][.N, ra_spread_le]), "в.п.: кредити СГ понад ДС після кредитної вартості")
if ("npl_le" %in% names(S)) fact("A7_npl_last", sprintf("СГ %s%%, ФО %s%%", p2(S[is.finite(npl_le)][.N, npl_le], 1), p2(S[is.finite(npl_hh)][.N, npl_hh], 1)))

# ====================== A8. ASSET-SIDE CONVEXITY, ASYMMETRY, GAP PREVIEW =====
ASER <- c(y_iea = "Усі процентні активи (книжкова)", y_le = "Кредити СГ (книжкова)", y_hh = "Кредити ФО (книжкова)", y_cd = "ДС НБУ (книжкова)",
          y_ovdp = "ОВДП (книжкова)", cod4_total = "Вартість депозитів", c_ibl = "Вартість процентних зобов'язань", nim = "NIM")
nb_le <- if (nrow(ML)) ML[sector == "СГ" & ccy == "UAH" & bucket == "без овердрафту", .(month, nb_le_uah = rate)] else data.table(month = as.Date(character()), nb_le_uah = numeric())
nb_hh <- if (nrow(ML)) ML[sector == "ФО" & ccy == "UAH" & bucket == "без овердрафту", .(month, nb_hh_uah = rate)] else data.table(month = as.Date(character()), nb_hh_uah = numeric())
S <- merge(merge(S, nb_le, by = "month", all.x = TRUE), nb_hh, by = "month", all.x = TRUE); setorder(S, month)
ASER <- c(ASER, nb_le_uah = "Нові кредити СГ, UAH (MIR)", nb_hh_uah = "Нові кредити ФО, UAH (MIR)")
ASER <- ASER[vapply(names(ASER), function(v) has(S, v) && sum(is.finite(S[[v]])) >= 48, logical(1))]
a8 <- rbindlist(lapply(names(ASER), function(v) tryCatch(ld_beta(S[[v]], S$policy_avg, 12)[, `:=`(series = v, label = ASER[[v]])], error = function(e) NULL)))
save_tab(a8, "A8_asset_convexity_betas.csv", "A8_convexity")
p <- ggplot(a8, aes(i0, beta)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
  geom_ribbon(aes(ymin = lo90, ymax = hi90), fill = PAL[1], alpha = .15) + geom_line(colour = PAL[1], linewidth = .8) + geom_point(colour = PAL[1], size = 1.3) +
  facet_wrap(~label, ncol = 3) + scale_x_continuous(labels = label_percent(1)) +
  labs(title = "12-місячна бета активів і пасивів залежно від рівня облікової ставки",
       subtitle = "Δ12 y = γ1·Δ12 i + γ2·Δ12 i·i(t−12); 90% інтервали Newey–West", x = "Облікова на початку вікна", y = "Бета", caption = CAP) + theme_thesis()
save_fig(p, "A8_asset_convexity.png", 9.5, 7)
# LP: cumulative betas of asset yield and liability cost -> revealed repricing gap preview
gapv <- intersect(c("y_iea", "c_ibl", "cod4_total", "nim", "y_le", "y_cd"), names(ASER))
lp <- rbindlist(lapply(gapv, function(v) lp_path(S[[v]], S$policy_avg, 24)[, series := v]))
lpa <- rbindlist(lapply(gapv, function(v) lp_path(S[[v]], S$policy_avg, 24, asym = TRUE)[, series := v]))
save_tab(lp, "A8b_lp_assets_liabilities.csv", "A8_LP"); save_tab(lpa, "A8c_lp_asymmetric.csv", "A8_LP_asym")
if (all(c("y_iea", "c_ibl") %in% lp$series)) {
  g <- merge(lp[series == "y_iea", .(h, bA = b, seA = se)], lp[series == "c_ibl", .(h, bL = b, seL = se)], by = "h")
  g[, gap := bA - bL]
  save_tab(g, "A8d_revealed_repricing_gap_preview.csv", "A8_gap")
  p <- ggplot(melt(g[, .(h, `Активи (дохідність процентних активів)` = bA, `Зобов'язання (вартість процентних зобов'язань)` = bL, `Розрив = активи − зобов'язання` = gap)], id.vars = "h"),
              aes(h, value, colour = variable, linewidth = variable == "Розрив = активи − зобов'язання")) +
    geom_hline(yintercept = 0, colour = INK2, linewidth = .3) + geom_line() + scale_linewidth_manual(values = c(.7, 1.1), guide = "none") +
    scale_colour_manual(values = c(PAL[1], PAL[2], INK)) + guides(colour = guide_legend(nrow = 3)) +
    labs(title = "Виявлений розрив переоцінювання: кумулятивна передача облікової ставки в активи та зобов'язання",
         subtitle = "Локальні проєкції на системних рядах (попередньо до 2.3); в.п. на 1 в.п. облікової ставки", x = "Горизонт, місяців", y = "Кумулятивна бета", caption = CAP) + theme_thesis()
  save_fig(p, "A8_repricing_gap_preview.png", 9, 5)
  fact("A8_gap_h6_h12_h24", sprintf("h=6: %.2f; h=12: %.2f; h=24: %.2f (активи %.2f vs зобов'язання %.2f при h=12)",
       g[h == 6, gap], g[h == 12, gap], g[h == 24, gap], g[h == 12, bA], g[h == 12, bL]))
}
for (v in intersect(c("y_iea", "y_le", "y_cd", "nb_le_uah"), unique(a8$series))) { z <- a8[series == v]
  fact(paste0("A8_convexity_", v), sprintf("β(6%%)=%.2f, β(15%%)=%.2f, β(25%%)=%.2f; p(γ2)=%.3f", z[i0 == .06, beta], z[i0 == .15, beta], z[i0 == .25, beta], z$p_gamma2[1])) }

# ====================== A9. BANK-LEVEL MATCHING (DSS 2021) ===================
# Bank-specific 12-month betas of interest expense rate and interest income rate;
# DSS: banks with less rate-sensitive funding hold less rate-sensitive assets (slope ≈ 1).
bk <- fb[, .(month, nkb, owner = fcoalesce(owner, "Exited"), y_a = ann * ii_m / avg_iea, c_l = ann * ie_m / avg_ibl, cd_sh = cd_osb / assets, ovdp_sh = ovdp_osb / assets,
             loans_sh = loans_all / assets, demand_share, assets)]
bk <- merge(bk, S[, .(month, i = policy_avg)], by = "month"); setorder(bk, nkb, month)
bk[, `:=`(y_a = winsor(y_a), c_l = winsor(c_l)), by = month]
betas <- bk[, {
  m12 <- add_months(month, -12L); j <- match(m12, month)
  dya <- y_a - y_a[j]; dcl <- c_l - c_l[j]; di <- i - i[j]; ok <- is.finite(dya) & is.finite(dcl) & is.finite(di) & abs(di) > 0.005
  if (sum(ok) >= 24) .(beta_assets = sum(di[ok] * dya[ok]) / sum(di[ok]^2), beta_liab = sum(di[ok] * dcl[ok]) / sum(di[ok]^2), n = sum(ok),
                       cd_sh = mean(cd_sh, na.rm = TRUE), ovdp_sh = mean(ovdp_sh, na.rm = TRUE), loans_sh = mean(loans_sh, na.rm = TRUE),
                       demand_share = mean(demand_share, na.rm = TRUE), assets = mean(assets, na.rm = TRUE), owner = owner[.N]) else NULL
}, by = nkb]
betas <- betas[is.finite(beta_assets) & is.finite(beta_liab)]
betas[, `:=`(ba_w = winsor(beta_assets, c(.05, .95)), bl_w = winsor(beta_liab, c(.05, .95)))]
save_tab(betas, "A9_bank_betas_matching.csv", "A9_matching")
if (nrow(betas) >= 20) {
  w <- betas$assets / mean(betas$assets)
  m1 <- lm(ba_w ~ bl_w, data = betas); m2 <- lm(ba_w ~ bl_w, data = betas, weights = w)
  cf <- function(m) { V <- sandwich::vcovHC(m, type = "HC1"); c(slope = coef(m)[["bl_w"]], se = sqrt(V["bl_w", "bl_w"])) }
  mt <- rbind(data.table(spec = "OLS", t(cf(m1))), data.table(spec = "WLS (активи)", t(cf(m2))))
  mt[, `:=`(n_banks = nrow(betas), r2 = c(summary(m1)$r.squared, summary(m2)$r.squared))]
  save_tab(mt, "A9b_matching_regression.csv", "A9_regression")
  p <- ggplot(betas, aes(bl_w, ba_w, size = assets, colour = owner)) + geom_abline(slope = 1, intercept = 0, linetype = 3, colour = INK2) +
    geom_point(alpha = .7) + geom_smooth(aes(group = 1, weight = assets), method = "lm", formula = y ~ x, se = FALSE, colour = INK, linewidth = .7) +
    scale_colour_manual(values = c(Foreign = PAL[1], Private = PAL[2], State = PAL[3], Exited = INK2),
                      labels = c(Foreign = "Іноземні", Private = "Приватні", State = "Державні", Exited = "Вийшли з ринку")) + scale_size_area(max_size = 9, guide = "none") +
    labs(title = "Узгодження чутливості активів і пасивів банків (тест Drechsler–Savov–Schnabl)",
         subtitle = sprintf("Банківські 12-міс. бети процентних доходів і витрат; нахил WLS = %.2f (s.e. %.2f); пунктир — повне узгодження",
                            mt[spec == "WLS (активи)", slope], mt[spec == "WLS (активи)", se]),
         x = "Бета вартості зобов'язань", y = "Бета дохідності активів", caption = CAP) + theme_thesis()
  save_fig(p, "A9_matching.png", 8, 6)
  fact("A9_matching_slope", sprintf("OLS %.2f (s.e. %.2f), WLS %.2f (s.e. %.2f), %d банків", mt$slope[1], mt$se[1], mt$slope[2], mt$se[2], nrow(betas)))
  fact("A9_cor_beta_liab_vs_cd_ovdp", sprintf("corr(β зобов'язань, частка ДС+ОВДП) = %.2f; corr(β зобов'язань, частка на вимогу) = %.2f",
       cor(betas$bl_w, betas$cd_sh + betas$ovdp_sh, use = "complete.obs"), cor(betas$bl_w, betas$demand_share, use = "complete.obs")))
}

# ====================== A10. 5-7-9 PROGRAMME ================================
# Data: ФРП weekly report (data_nbu/local_inputs/5-7-9.xlsx): cumulative signed
# contracts (mn UAH) and outstanding debt (mn UAH). No rates, no compensation.
# The bank's rate is rebuilt from the programme rules (Vilerts: price the loan at
# the implicit rate the bank earns, then deduct the reference rate):
#   bank rate  = UIRD 3M (reviewed quarterly) + margin(size, period), cap 30 %
#   borrower   = 0 / 5 / 7 / 9 % (+0.1 % for the first 2 years in some groups)
#   state      = bank rate − borrower rate
# Margins below are the programme CAPS. Banks may price under the cap, so the
# corrected rates are an upper bound; the band lo–hi spans the size classes.
# MIR assumption (switch below): new 5-7-9 loans enter MIR at the borrower rate.
P579_MARGIN <- data.table(
  start  = as.Date(c("2020-02-01", "2022-07-01", "2024-10-01")),
  micro  = c(7, 14, 10) / 100, small = c(6, 12, 8) / 100, medium = c(5, 10, 6) / 100,
  source = c("Програма ФРП, ред. 18.03.2022, п. 13.1: UIRD 3M + 7 / 6 / 5 в.п. (мікро / малі / середні)",
             "Рестарт 2022: маржа +≈7 в.п. (≈ UIRD + 14); дата 2022-07 — припущення, узгоджено з −4 в.п. у 2024",
             "Постанова КМУ № 1054 від 13.09.2024: UIRD 3M + 10 / 8 / 6 в.п."))
P579_CAP <- 0.30
P579_BORROWER <- c(lo = 0.00, central = 0.05, hi = 0.09)
MIR_579_AT_BORROWER_RATE <- TRUE

g579 <- read_579_local()
if (!is.null(g579) && nrow(ML) && nrow(UIRD_M)) {
  g <- copy(g579$monthly)
  # UIRD 3M reference reset at quarter start = average of the last month of the previous quarter
  qs <- function(m) as.Date(sprintf("%d-%02d-01", year(m), 3L * ((month(m) - 1L) %/% 3L) + 1L))
  g[, uird_ref := UIRD_M$uird3m[match(add_months(qs(month), -1L), UIRD_M$month)]]
  mg <- P579_MARGIN[findInterval(as.numeric(g$month), as.numeric(P579_MARGIN$start))]
  g[, `:=`(bank_lo = pmin(uird_ref + mg$medium, P579_CAP), bank_c = pmin(uird_ref + mg$small, P579_CAP), bank_hi = pmin(uird_ref + mg$micro, P579_CAP))]
  # MIR: new UAH loans to NFCs excluding overdrafts (5-7-9 has no overdrafts) and total
  mir_nfc <- ML[sector == "СГ" & ccy == "UAH" & bucket == "без овердрафту", .(month, mir_rate = rate, mir_vol = vol)]
  g <- merge(g, mir_nfc, by = "month", all.x = TRUE)
  g <- merge(g, S[, .(month, cd_on = fcoalesce(col_or_na(S, "cd_on_rate_avg"), policy_avg), policy = policy_avg, loans_le,
                      y_le_book = fcoalesce(col_or_na(S, "y_le_osb"), y_le))], by = "month", all.x = TRUE)
  g[, `:=`(s_new = new_contracts_mn / mir_vol, s_stock = debt_mn / loans_le)]
  g[, s_new_flag := is.finite(s_new) & s_new > 0.5]               # contract amounts ≫ MIR flow: do not correct
  w <- if (MIR_579_AT_BORROWER_RATE) 1 else 0
  sn <- fifelse(g$s_new_flag, NA_real_, g$s_new)
  g[, `:=`(mir_adj_c  = mir_rate + w * sn * (bank_c  - P579_BORROWER[["central"]]),
           mir_adj_lo = mir_rate + w * sn * (bank_lo - P579_BORROWER[["hi"]]),
           mir_adj_hi = mir_rate + w * sn * (bank_hi - P579_BORROWER[["lo"]]),
           mir_market = (mir_rate - w * sn * P579_BORROWER[["central"]]) / (1 - w * sn))]
  # Book: banks accrue the compensation as interest income -> book yield already "economic";
  # implied yield on the non-programme corporate book (all currencies, finrep from 2021-01)
  g[, y_le_non579 := (y_le_book - s_stock * bank_c) / (1 - s_stock)]
  g[, `:=`(prem_raw = mir_rate - cd_on, prem_adj = mir_adj_c - cd_on, prem_adj_lo = mir_adj_lo - cd_on, prem_adj_hi = mir_adj_hi - cd_on,
           prem_market = mir_market - cd_on, prem_579 = bank_c - cd_on)]
  save_tab(g, "A10_579_monthly.csv", "A10_579")
  save_tab(P579_MARGIN, "A10a_579_margin_rules.csv", "A10_rules")

  win <- list(`останні 12 міс.` = g[month >= add_months(max(month[is.finite(mir_rate)]), -11L)],
              `війна (з 2022-03)` = g[month >= as.Date("2022-03-01")], `до війни` = g[month < as.Date("2022-03-01")])
  sm <- rbindlist(lapply(names(win), function(k) { z <- win[[k]]
    data.table(window = k, months = sum(is.finite(z$prem_raw)), s_new = mean(z$s_new[!z$s_new_flag], na.rm = TRUE), s_stock = mean(z$s_stock, na.rm = TRUE),
               uird_ref = mean(z$uird_ref, na.rm = TRUE), bank_rate_579 = mean(z$bank_c, na.rm = TRUE), cd_on = mean(z$cd_on, na.rm = TRUE),
               prem_raw = mean(z$prem_raw, na.rm = TRUE), prem_adj = mean(z$prem_adj, na.rm = TRUE), prem_adj_lo = mean(z$prem_adj_lo, na.rm = TRUE),
               prem_adj_hi = mean(z$prem_adj_hi, na.rm = TRUE), prem_market = mean(z$prem_market, na.rm = TRUE), prem_579 = mean(z$prem_579, na.rm = TRUE)) }))
  save_tab(sm, "A10b_579_premia_summary.csv", "A10_summary")

  # Figure 1: volumes and shares
  v1 <- melt(g[, .(month, `Нові договори 5-7-9, млрд грн/міс.` = new_contracts_mn / 1e3, `Заборгованість 5-7-9, млрд грн` = debt_mn / 1e3,
                   `Частка в нових кредитах СГ, UAH (MIR, без овердрафтів), %` = 100 * fifelse(s_new_flag, NA_real_, s_new),
                   `Частка в портфелі кредитів СГ (усі валюти), %` = 100 * s_stock)], id.vars = "month")
  p <- ggplot(v1, aes(month, value)) + geom_line(colour = PAL[1], linewidth = .7, na.rm = TRUE) + facet_wrap(~variable, scales = "free_y", ncol = 2) +
    labs(title = "Програма «Доступні кредити 5-7-9%»: обсяги і вага в кредитуванні бізнесу",
         subtitle = "ФРП: підписані договори (накопичено; місячний приріст) і поточна заборгованість; частки — до MIR та фінзвітності банків",
         x = NULL, y = NULL, caption = "Джерело: ФРП (тижневий звіт), НБУ (MIR, фінзвітність банків); розрахунки автора.") + theme_thesis()
  save_fig(p, "A10a_579_volumes_shares.png", 10, 6)

  # Figure 2: rates — MIR raw vs corrected, programme bank rate, references
  r2 <- g[, .(month, `MIR: нові кредити СГ, UAH (як звітовано)` = mir_rate, `MIR, скориговано на 5-7-9 (ставка банку)` = mir_adj_c,
              `Ставка банку за 5-7-9 (UIRD 3M + маржа малих)` = bank_c, `ДС НБУ овернайт` = cd_on, `UIRD 3M` = uird_ref)]
  r2 <- melt(r2, id.vars = "month")   # NA kept: MIR gap 2023-02…2024-04 shows as a break
  r2 <- r2[CJ(month = seq(min(g$month), max(g$month), by = "month"), variable = unique(r2$variable)), on = .(month, variable)]
  band <- g[, .(month, lo = mir_adj_lo, hi = mir_adj_hi)]   # NA rows break the ribbon at the MIR gap
  p <- ggplot() + geom_ribbon(data = band, aes(month, ymin = lo, ymax = hi), fill = PAL[2], alpha = .15) +
    geom_line(data = r2, aes(month, value, colour = variable), linewidth = .7, na.rm = TRUE) +
    scale_colour_manual(values = c(PAL[1], PAL[2], PAL[4], INK, PAL[3])) + scale_y_continuous(labels = label_percent(1)) +
    guides(colour = guide_legend(nrow = 3)) +
    labs(title = "Скільки насправді заробляє банк на новому кредиті бізнесу",
         subtitle = "Смуга: розмах маржі (середні–мікро) і ставки позичальника (0–9%). Маржа — граничні рівні програми, тож корекція — верхня межа",
         x = NULL, y = "Річна ставка", caption = CAP) + theme_thesis()
  save_fig(p, "A10b_579_rates_corrected.png", 9.5, 5.6)

  L12 <- sm[window == "останні 12 міс."]; WR <- sm[window == "війна (з 2022-03)"]
  fact("A10_579_volume_last", sprintf("заборгованість %s млрд грн (%s), нові договори за останні 12 міс. %s млрд грн",
       round(g[.N, debt_mn] / 1e3, 1), format(g[.N, month], "%Y-%m"), round(sum(tail(g$new_contracts_mn, 12), na.rm = TRUE) / 1e3, 1)))
  fact("A10_579_shares", sprintf("частка в нових кредитах СГ (MIR UAH, без овердрафтів): %s%% (12 міс.), %s%% (війна); у портфелі СГ: %s%%",
       p2(L12$s_new, 1), p2(WR$s_new, 1), p2(L12$s_stock, 1)))
  fact("A10_579_bank_rate", sprintf("ставка банку за 5-7-9 ≈ %s%% (UIRD 3M %s%% + маржа) проти ДС овернайт %s%% — останні 12 міс.",
       p2(L12$bank_rate_579), p2(L12$uird_ref), p2(L12$cd_on)))
  fact("A10_premium_raw_vs_adj", sprintf("премія до ДС: як звітовано %s в.п. → скориговано %s в.п. [%s; %s]; ринкові кредити %s в.п. (останні 12 міс.); у війну: %s → %s в.п.",
       p2(L12$prem_raw), p2(L12$prem_adj), p2(L12$prem_adj_lo), p2(L12$prem_adj_hi), p2(L12$prem_market), p2(WR$prem_raw), p2(WR$prem_adj)))
  fact("A10_flags", sprintf("місяців, де договори 5-7-9 > 50%% обсягу MIR (корекція не застосована): %d; останній місяць ФРП неповний: %s",
       sum(g$s_new_flag, na.rm = TRUE), g[.N, partial]))
  status("A10 5-7-9", "ok", sprintf("%s; ставка банку відновлена за правилами програми (UIRD 3M + маржа), компенсація не спостерігається", g579$file))
} else status("A10 5-7-9", "skipped", "потрібні 5-7-9.xlsx (тижневий звіт ФРП) і UIRD.xls у data_nbu/local_inputs")

# ====================== EXPORT ===============================================
save_tab(STATUS, "A0_blocks_status.csv", "Status"); save_tab(FACTS, "A0_key_facts_assets.csv", "Key_facts")
write_workbook("Section_2_1_v2_assets.xlsx")
print(STATUS); print(FACTS)
log_msg("ASSETS & JOINT COMPLETE: ", length(list.files(FIG, pattern = "^A")), " figures, ", length(TABLES), " tables")
