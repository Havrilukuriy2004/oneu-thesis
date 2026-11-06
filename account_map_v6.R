# ============================================================================
# account_map_v6.R — explicit OSB account mapping (NBU Chart of Accounts,
# Постанова Правління НБУ від 11.09.2017 № 89, з наступними змінами)
# ----------------------------------------------------------------------------
# rule:
#   P    = passive balance (t025 = 2)                     -> liability stock
#   A    = active balance (t025 = 1)                      -> asset stock
#   NETA = active - passive balance                       -> АП adjustment accounts
#   EXP  = debit turnover - credit turnover (t025 5 - 6)   -> monthly expense
#   INC  = credit turnover - debit turnover (t025 6 - 5)   -> monthly income
#   C    = credit turnover only (t025 = 6)                 -> accrual of interest payable
#
# IMPORTANT: income/expense accounts (class 6/7) are kept ONLY in hryvnia
# (r034 = 1) — FX interest is converted at accrual. The currency split of
# interest expense is therefore estimated from the credit turnover of the
# accrued-interest accounts 2608/2618/2628/2638/2658, which ARE kept by currency
# (see 02_build_marts_v6.R, section 2b). v5.x reported FX expense = 0 and hence
# a zero FX funding cost and an over-stated UAH cost.
#
# v5.x used GROSS turnovers (t025 = 5 for expenses, t025 = 6 for income).
# Gross debit turnover on 70xx includes amounts later reversed (storno), which
# produced spurious spikes (e.g. 2019-05 and 2023-07, nkb = 46). Net turnover
# coincides with the change in the YTD balance of the income/expense account.
# ============================================================================

.acc <- function(accounts, variable, rule, block, note = "")
  data.table::data.table(account = sprintf("%04d", as.integer(accounts)),
                         variable = variable, rule = rule, block = block, note = note)

ACCOUNT_MAP_V6 <- data.table::rbindlist(list(
  # ---------------- LIABILITIES: client deposits (principal) ---------------
  .acc(c(2600:2606),            "dep_corp_demand", "P", "liability", "гр.260: кошти на вимогу СГ (2605 до 2018-12)"),
  .acc(2610,                    "dep_corp_term",   "P", "liability", "гр.261: строкові депозити СГ (без репо 2611)"),
  .acc(c(2620:2622, 2624, 2625),"dep_hh_demand",   "P", "liability", "гр.262: кошти на вимогу ФО (2625 до 2018-12)"),
  .acc(2630,                    "dep_hh_term",     "P", "liability", "гр.263: строкові депозити ФО (без репо 2631)"),
  .acc(c(2650, 2652, 2654),     "dep_nbfi_demand", "P", "liability", "гр.265: кошти НБФУ на вимогу"),
  .acc(2651,                    "dep_nbfi_term",   "P", "liability", "гр.265: строкові депозити НБФУ"),
  .acc(c(2512, 2513, 2520, 2523, 2525, 2526, 2541, 2542, 2544:2546,
         2550:2556, 2560:2562, 2565), "dep_budget", "P", "liability", "розд.25: бюджетні кошти"),
  .acc(c(2611, 2631),           "repo_clients",    "P", "liability", "репо з клієнтами"),
  .acc(3320,                    "savings_certs",   "P", "liability", "ощадні сертифікати"),
  # ---------------- Accrued interest payable (by currency) -----------------
  .acc(2608, "accrC_corp_demand", "C", "accrual", "кредитовий оборот: нараховані витрати, кошти на вимогу СГ"),
  .acc(2618, "accrC_corp_term",   "C", "accrual", "кредитовий оборот: нараховані витрати, строкові кошти СГ"),
  .acc(2628, "accrC_hh_demand",   "C", "accrual", "кредитовий оборот: нараховані витрати, кошти на вимогу ФО"),
  .acc(2638, "accrC_hh_term",     "C", "accrual", "кредитовий оборот: нараховані витрати, строкові кошти ФО"),
  .acc(2658, "accrC_nbfi",        "C", "accrual", "кредитовий оборот: нараховані витрати, кошти НБФУ"),
  .acc(2608, "accrB_corp_demand", "P", "accrual", "залишок нарахованих витрат"),
  .acc(2618, "accrB_corp_term",   "P", "accrual", "залишок нарахованих витрат"),
  .acc(2628, "accrB_hh_demand",   "P", "accrual", "залишок нарахованих витрат"),
  .acc(2638, "accrB_hh_term",     "P", "accrual", "залишок нарахованих витрат"),
  .acc(2658, "accrB_nbfi",        "P", "accrual", "залишок нарахованих витрат"),
  # ---------------- LIABILITIES: wholesale / other -------------------------
  .acc(c(1310:1312, 1321:1324, 1334), "liab_nbu", "P", "liability", "рефінансування НБУ"),
  .acc(c(1600, 1602, 1610, 1613, 1621:1623), "liab_interbank", "P", "liability", "кошти інших банків"),
  .acc(c(2701, 2703),           "liab_ifi",        "P", "liability", "кредити МФО"),
  .acc(c(3300:3303, 3305, 3310, 3312:3314), "liab_own_sec", "P", "liability", "власні боргові ЦП"),
  .acc(c(3660, 3661),           "liab_subdebt",    "P", "liability", "субординований борг"),

  # ---------------- ASSETS: NBU instruments and securities ----------------
  .acc(c(1430, 1440, 1450),     "cd_nbu",          "A",    "asset", "депсертифікати НБУ (FVOCI/AC/FVTPL)"),
  .acc(c(1435, 1436, 1446, 1455, 1456), "cd_nbu_adj", "NETA", "asset", "переоцінка/дисконт ДС НБУ"),
  .acc(c(1400, 1410, 1420),     "ovdp",            "A",    "asset", "ОВДП, що рефінансуються НБУ (FVTPL/FVOCI/AC)"),
  .acc(c(1401:1404, 1411:1414, 1421:1424), "refin_other", "A", "asset", "інші ЦП, що рефінансуються НБУ (ОЗДП, місцеві, банки, корп.)"),
  .acc(c(1405, 1406, 1415, 1416, 1426), "refin_adj", "NETA", "asset", "переоцінка/дисконт ЦП, що рефінансуються"),
  .acc(c(3010:3014, 3110:3114, 3210:3214), "sec_other", "A", "asset", "інші боргові ЦП"),
  .acc(c(3015, 3016, 3115, 3116, 3216), "sec_other_adj", "NETA", "asset", "переоцінка/дисконт інших боргових ЦП"),
  .acc(c(1211, 1212),           "nbu_placements",  "A", "asset", "депозити/репо в НБУ"),
  .acc(c(1200, 1203),           "nbu_corr_reserves","A", "asset", "коррахунок і обов'язкові резерви в НБУ (безпроцентні)"),
  .acc(c(1500, 1502, 1510, 1513, 1520:1522, 1524, 1532, 1533, 1542, 1543), "interbank_assets", "A", "asset", "кошти/кредити в інших банках"),
  # ---------------- ASSETS: loans (gross principal) ------------------------
  .acc(c(2010, 2020, 2030, 2040:2045, 2063, 2071, 2083,
         2301, 2303, 2310, 2311, 2320, 2321, 2330, 2331, 2340, 2341, 2351, 2353,
         2390:2395),            "loans_corp",      "A", "asset", "кредити СГ (AC/FVOCI/FVTPL, включно з POCI)"),
  .acc(2600,                    "loans_corp_od",   "A", "asset", "овердрафти СГ (активне сальдо 2600)"),
  .acc(2650,                    "loans_nbfi_od",   "A", "asset", "овердрафти НБФУ (активне сальдо 2650)"),
  .acc(c(2103, 2113, 2123, 2133, 2140:2143, 2360:2363, 2370:2373, 2380:2383),
                                "loans_gov",       "A", "asset", "кредити органам влади"),
  .acc(c(2203, 2211, 2220, 2233, 2240:2244, 2260,
         2401, 2403, 2410, 2411, 2420, 2421, 2431, 2433, 2440, 2441, 2450:2454),
                                "loans_hh",        "A", "asset", "кредити ФО"),
  .acc(c(2620, 2621),           "loans_hh_od",     "A", "asset", "овердрафти/картки ФО (активне сальдо 2620/2621)"),

  # ---------------- INCOME FLOWS (net) --------------------------------------
  .acc(c(6000, 6002, 6003),     "inc_nbu",          "INC", "income"),
  .acc(c(6010:6019),            "inc_interbank",    "INC", "income"),
  .acc(c(6020, 6022:6027, 6030:6035, 6070:6079, 6090:6095), "inc_loans_corp", "INC", "income"),
  .acc(6021,                    "inc_loans_nbfi_od","INC", "income"),
  .acc(c(6040:6047, 6080:6087, 6096), "inc_loans_gov", "INC", "income"),
  .acc(c(6050, 6052:6056, 6060:6064, 6100:6114), "inc_loans_hh", "INC", "income"),
  .acc(c(6120:6122),            "inc_refin_sec",    "INC", "income", "ширше за ОВДП: усі ЦП гр.140-142"),
  .acc(c(6123:6125),            "inc_sec_other",    "INC", "income"),
  .acc(c(6126:6128),            "inc_cd_nbu",       "INC", "income"),
  .acc(c(6130, 6140, 6141),     "inc_other",        "INC", "income"),

  # ---------------- EXPENSE FLOWS (net) -------------------------------------
  .acc(7020, "exp_corp_demand", "EXP", "expense"),
  .acc(7021, "exp_corp_term",   "EXP", "expense"),
  .acc(7040, "exp_hh_demand",   "EXP", "expense"),
  .acc(7041, "exp_hh_term",     "EXP", "expense"),
  .acc(7070, "exp_nbfi_demand", "EXP", "expense"),
  .acc(7071, "exp_nbfi_term",   "EXP", "expense"),
  .acc(7030, "exp_budget",      "EXP", "expense"),
  .acc(7042, "exp_repo_hh",     "EXP", "expense"),
  .acc(c(7000, 7002, 7003, 7004, 7006), "exp_nbu", "EXP", "expense"),
  .acc(c(7010:7012, 7014:7017), "exp_interbank", "EXP", "expense"),
  .acc(7060, "exp_ifi",         "EXP", "expense"),
  .acc(c(7120:7125), "exp_own_sec", "EXP", "expense"),
  .acc(c(7140, 7142), "exp_subdebt", "EXP", "expense"),
  .acc(c(7130, 7141), "exp_other", "EXP", "expense"),
  .acc(c(7028, 7048), "exp_lease", "EXP", "expense", "IFRS 16 — не є вартістю фондування")
))

# Products used in the four/six-product funding identity of Section 2.1.
FUNDING_PRODUCTS <- data.table::data.table(
  product = c("corp_demand", "corp_term", "hh_demand", "hh_term", "nbfi_demand", "nbfi_term"),
  stock   = c("dep_corp_demand", "dep_corp_term", "dep_hh_demand", "dep_hh_term", "dep_nbfi_demand", "dep_nbfi_term"),
  flow    = c("exp_corp_demand", "exp_corp_term", "exp_hh_demand", "exp_hh_term", "exp_nbfi_demand", "exp_nbfi_term"),
  sector  = c("corp", "corp", "hh", "hh", "nbfi", "nbfi"),
  maturity= c("demand", "term", "demand", "term", "demand", "term"),
  core4   = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  accr    = c("corp_demand", "corp_term", "hh_demand", "hh_term", "nbfi", "nbfi")
)

# Asset-yield pairs (flow / average stock) used for the asset side of 2.1.
ASSET_YIELDS <- data.table::data.table(
  asset = c("cd_nbu", "refin_sec", "loans_corp", "loans_hh", "loans_gov", "interbank", "nbu_placements", "sec_other"),
  stock = list("cd_nbu", c("ovdp", "refin_other", "refin_adj"), c("loans_corp", "loans_corp_od", "loans_nbfi_od"),
               c("loans_hh", "loans_hh_od"), "loans_gov", "interbank_assets", "nbu_placements", c("sec_other", "sec_other_adj")),
  flow  = list("inc_cd_nbu", "inc_refin_sec", c("inc_loans_corp", "inc_loans_nbfi_od"),
               "inc_loans_hh", "inc_loans_gov", "inc_interbank", "inc_nbu", "inc_sec_other")
)

OSB_ACCOUNTS_V6 <- sort(unique(ACCOUNT_MAP_V6$account))
