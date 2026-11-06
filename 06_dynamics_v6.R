# ============================================================================
# 06_dynamics_v6.R — ПОКАЗНИКИ ДИНАМІКИ для всіх показників
# ----------------------------------------------------------------------------
# Reads only the marts (run after 02 and 05). Output:
#   output_nbu/section_2_1_v2/Dynamics_indicators.xlsx  + tables G*.csv + figures G*.png
#
# 1) Класичні показники динаміки за роками (для тексту і додатків):
#    рівень ряду; абсолютний приріст (ланцюговий, базисний); темп зростання і
#    темп приросту (ланцюгові, базисні); абсолютне значення 1% приросту;
#    середні: середній рівень, середній абсолютний приріст, середній темп
#    зростання (геометричний), середній темп приросту.
#    Річний рівень: для обсягів — на кінець року (грудень), для ставок і
#    часток — середнє за рік. Поточний рік — за наявні місяці (позначка *).
#    Для ставок і часток прирости — у відсоткових пунктах (в.п.).
# 2) Поточна (місячна) динаміка: останнє значення; зміни за 1, 3, 12 міс.;
#    з початку року; з початку війни (база 2022-01); з початку поточного
#    циклу облікової ставки; мін./макс. за 12 міс.; z-оцінка відносно 36 міс.;
#    перцентиль в історії з 2018 р.
# 3) Те саме для ВСІХ числових рядів мартів (аркуш «Усі_показники», тип ряду
#    визначається автоматично).
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_models_v6.R"), encoding = "UTF-8")
init_output("G")
CAP <- "Джерело: розрахунки автора за даними НБУ."

# ------------------------------ data -----------------------------------------
col_or_na <- function(dt, v) if (v %in% names(dt)) dt[[v]] else rep(NA_real_, nrow(dt))
sys <- store_read("mart_system_month_v6"); sys[, month := as.Date(month)]
prod <- store_read("mart_system_product_v6"); prod[, month := as.Date(month)]
ast <- store_read("mart_system_assets_v6"); if (nrow(ast)) ast[, month := as.Date(month)]
pw <- if (nrow(prod)) dcast(prod[core4 == TRUE], month ~ paste0("r_", product, "_", ccy), value.var = "rate") else data.table(month = sys$month)
P <- merge(sys, pw, by = "month", all.x = TRUE)
if (nrow(ast)) { keep <- c("month", setdiff(intersect(names(ast), c("y_le", "y_hh", "y_sec", "y_iea", "c_ibl", "nim", "credit_cost", "loans_le", "loans_hh", "assets", "iea", "ibl")), names(P)))
  P <- merge(P, ast[, ..keep], by = "month", all.x = TRUE) }
P[, `:=`(real_policy = col_or_na(P, "policy_avg") - col_or_na(P, "cpi_yoy"), real_cod4 = col_or_na(P, "cod4_total") - col_or_na(P, "cpi_yoy"))]
P <- P[month >= START_MONTH]; setorder(P, month)

# ------------------------------ dictionary of key indicators ------------------
# type: rate (decimal, changes in п.п.), share (decimal, п.п.), level (mn UAH, changes in %), count
K <- rbindlist(list(
  list("policy_avg",          "Облікова ставка НБУ (середня за міс.)",               "rate",  "Монетарні умови"),
  list("cd_on_rate_avg",      "Ставка ДС НБУ овернайт",                               "rate",  "Монетарні умови"),
  list("refi_on_rate_avg",    "Ставка рефінансування овернайт",                       "rate",  "Монетарні умови"),
  list("uonia_avg",           "UONIA",                                                "rate",  "Монетарні умови"),
  list("cpi_yoy",             "Інфляція ІСЦ, р/р",                                    "rate",  "Монетарні умови"),
  list("core_cpi_yoy",        "Базова інфляція, р/р",                                 "rate",  "Монетарні умови"),
  list("real_policy",         "Реальна облікова ставка (ex post)",                    "rate",  "Монетарні умови"),
  list("cod4_total",          "Вартість депозитів, книжкова (4 продукти)",            "rate",  "Вартість фондування"),
  list("cod4_uah",            "Вартість депозитів, UAH",                              "rate",  "Вартість фондування"),
  list("cod4_fx",             "Вартість депозитів, іноземна валюта",                  "rate",  "Вартість фондування"),
  list("cod6_total",          "Вартість депозитів, книжкова (6 продуктів, з НБФУ)",   "rate",  "Вартість фондування"),
  list("real_cod4",           "Реальна вартість депозитів (ex post)",                 "rate",  "Вартість фондування"),
  list("r_hh_demand_total",   "ФО: кошти на вимогу, книжкова ставка",                 "rate",  "Вартість фондування"),
  list("r_hh_term_total",     "ФО: строкові депозити, книжкова ставка",               "rate",  "Вартість фондування"),
  list("r_corp_demand_total", "СГ: кошти на вимогу, книжкова ставка",                 "rate",  "Вартість фондування"),
  list("r_corp_term_total",   "СГ: строкові депозити, книжкова ставка",               "rate",  "Вартість фондування"),
  list("nb_dep_hh_term_uah",  "Нові строкові депозити ФО, UAH (MIR)",                 "rate",  "Маржинальні ставки"),
  list("nb_dep_nfc_term_uah", "Нові строкові депозити СГ, UAH (MIR)",                 "rate",  "Маржинальні ставки"),
  list("nb_dep_hh_demand_uah","Нові кошти на вимогу ФО, UAH (MIR)",                   "rate",  "Маржинальні ставки"),
  list("uird_uah_3m_avg",     "UIRD 3M, UAH",                                         "rate",  "Маржинальні ставки"),
  list("uird_uah_12m_avg",    "UIRD 12M, UAH",                                        "rate",  "Маржинальні ставки"),
  list("uird_usd_12m_avg",    "UIRD 12M, USD",                                        "rate",  "Маржинальні ставки"),
  list("nb_loan_nfc_all_uah", "Нові кредити СГ, UAH (MIR)",                           "rate",  "Маржинальні ставки"),
  list("nb_loan_hh_all_uah",  "Нові кредити ФО, UAH (MIR)",                           "rate",  "Маржинальні ставки"),
  list("zc_uah_1y_avg",       "Дохідність ОВДП 1 рік (NSS)",                          "rate",  "Ринок ОВДП"),
  list("zc_uah_3y_avg",       "Дохідність ОВДП 3 роки (NSS)",                         "rate",  "Ринок ОВДП"),
  list("zc_uah_5y_avg",       "Дохідність ОВДП 5 років (NSS)",                        "rate",  "Ринок ОВДП"),
  list("zc_fx_1y_avg",        "Дохідність валютних ОВДП 1 рік (NS)",                  "rate",  "Ринок ОВДП"),
  list("y_cd_total",          "Дохідність депсертифікатів НБУ (книжкова)",            "rate",  "Дохідність активів"),
  list("y_refin_total",       "Дохідність ЦП, що рефінансуються (ОВДП)",              "rate",  "Дохідність активів"),
  list("y_le",                "Дохідність кредитів СГ (фінзвітність)",                "rate",  "Дохідність активів"),
  list("y_hh",                "Дохідність кредитів ФО (фінзвітність)",                "rate",  "Дохідність активів"),
  list("y_iea",               "Дохідність процентних активів",                        "rate",  "Дохідність активів"),
  list("c_ibl",               "Вартість процентних зобов'язань",                      "rate",  "Дохідність активів"),
  list("nim",                 "Чиста процентна маржа (NIM)",                          "rate",  "Дохідність активів"),
  list("credit_cost",         "Вартість ризику (відрахування в резерви / кредити)",   "rate",  "Дохідність активів"),
  list("dep_core4_total",     "Депозити ФО і СГ (4 продукти), усього",                "level", "Обсяги"),
  list("dep_core4_uah",       "Депозити ФО і СГ, UAH",                                "level", "Обсяги"),
  list("dep_core4_fx",        "Депозити ФО і СГ, іноземна валюта",                    "level", "Обсяги"),
  list("dep_hh_total",        "Депозити ФО",                                          "level", "Обсяги"),
  list("dep_corp_total",      "Депозити СГ",                                          "level", "Обсяги"),
  list("dep_demand4_total",   "Кошти на вимогу (ФО + СГ)",                            "level", "Обсяги"),
  list("dep_term4_total",     "Строкові депозити (ФО + СГ)",                          "level", "Обсяги"),
  list("cd_nbu_total",        "Депозитні сертифікати НБУ в активах банків",           "level", "Обсяги"),
  list("ovdp_total",          "ОВДП у портфелі банків",                               "level", "Обсяги"),
  list("loans_le",            "Кредити СГ (валові, фінзвітність)",                    "level", "Обсяги"),
  list("loans_hh",            "Кредити ФО (валові, фінзвітність)",                    "level", "Обсяги"),
  list("assets",              "Активи банківської системи",                           "level", "Обсяги"),
  list("liq_certdepnbu",      "Залишки ДС НБУ (дані ліквідності, середні)",           "level", "Ліквідність"),
  list("liq_bankscoracc",     "Кошти на кореспондентських рахунках",                  "level", "Ліквідність"),
  list("demand_share",        "Частка коштів на вимогу",                              "share", "Структура"),
  list("term_share",          "Частка строкових депозитів",                           "share", "Структура"),
  list("fx_share",            "Частка депозитів в іноземній валюті",                  "share", "Структура"),
  list("hh_share",            "Частка депозитів ФО",                                  "share", "Структура"),
  list("cd_to_dep",           "ДС НБУ / депозити",                                    "share", "Структура"),
  list("ovdp_to_dep",         "ОВДП / депозити",                                      "share", "Структура"),
  list("top5_dep",            "Частка 5 найбільших банків у депозитах",               "share", "Структура"),
  list("hhi_dep",             "Індекс Герфіндаля–Гіршмана (депозити)",                "share", "Структура"),
  list("n_banks",             "Кількість банків у вибірці",                           "count", "Структура"),
  # expectations of economic agents (NBU surveys, market curve)
  list("pie_hh",              "Інфляційні очікування домогосподарств (12 міс.)",      "rate",  "Очікування"),
  list("pie_firms",           "Інфляційні очікування підприємств (12 міс.)",          "rate",  "Очікування"),
  list("pie_banks",           "Інфляційні очікування банків (12 міс.)",               "rate",  "Очікування"),
  list("pie_analysts",        "Інфляційні очікування фінансових аналітиків (12 міс.)","rate",  "Очікування"),
  list("edep_hh",             "Очікувана девальвація гривні: домогосподарства",       "rate",  "Очікування"),
  list("edep_analysts",       "Очікувана девальвація гривні: аналітики",              "rate",  "Очікування"),
  list("rr_policy",           "Реальна облікова ставка (ex ante)",                    "rate",  "Очікування"),
  list("rr_dep_hh",           "Реальна ставка нових депозитів ФО до 1 р. (ex ante)",  "rate",  "Очікування"),
  list("uip_hh",              "Надлишкова дохідність грн депозиту над валютним",      "rate",  "Очікування"),
  list("exp_path_1y",         "Очікувана зміна облікової на 1 рік (крива ОВДП)",      "rate",  "Очікування"),
  list("ioda",                "Індекс очікувань ділової активності (ІОДА)",           "count", "Очікування"),
  list("bls_dem_nfc",         "BLS: попит СГ на кредити (баланс відповідей)",         "count", "Очікування"),
  list("bls_std_nfc",         "BLS: стандарти кредитування СГ (баланс, + = жорсткіші)","count", "Очікування"),
  # full NBU interest-rate statistics (UAH, new and outstanding)
  list("fm_dep_hh_uah_1y",    "Нові депозити ФО до 1 року, грн (статистика НБУ)",     "rate",  "Ставки НБУ (повна статистика)"),
  list("fm_dep_hh_fx_1y",     "Нові депозити ФО до 1 року, валюта",                   "rate",  "Ставки НБУ (повна статистика)"),
  list("fm_out_dep_hh_uah",   "Непогашені депозити ФО, грн",                          "rate",  "Ставки НБУ (повна статистика)"),
  list("fm_dep_nfc_uah_1y",   "Нові депозити СГ до 1 року, грн",                      "rate",  "Ставки НБУ (повна статистика)"),
  list("fm_loan_nfc_uah_xo",  "Нові кредити СГ, грн (без овердрафту)",                "rate",  "Ставки НБУ (повна статистика)"),
  list("fm_out_loan_nfc",     "Непогашені кредити СГ",                                "rate",  "Ставки НБУ (повна статистика)"),
  list("fm_ib_uah",           "Міжбанківські кредити, грн",                           "rate",  "Ставки НБУ (повна статистика)"),
  # monetary statistics
  list("mon_m0",              "Готівка поза банками (M0)",                            "level", "Грошова статистика"),
  list("mon_m1",              "Грошовий агрегат M1",                                  "level", "Грошова статистика"),
  list("mon_m3",              "Грошова маса M3",                                      "level", "Грошова статистика"),
  list("mon_dep_uah",         "Депозити в M3, гривня",                                "level", "Грошова статистика"),
  list("mon_dep_fx",          "Депозити в M3, іноземна валюта",                       "level", "Грошова статистика"),
  list("mon_dollar",          "Доларизація депозитів у M3",                           "share", "Грошова статистика"),
  list("mon_hh_dollar",       "Доларизація депозитів ФО",                             "share", "Грошова статистика"),
  list("mon_hh_term_share_uah","Частка строкових у гривневих депозитах ФО",           "share", "Грошова статистика"),
  list("mon_mult",            "Грошовий мультиплікатор M3/M0",                        "count", "Грошова статистика")))
setnames(K, c("var", "label", "type", "group"))
K <- K[var %in% names(P)]
K <- K[vapply(var, function(v) sum(is.finite(P[[v]]) & P[[v]] != 0) >= 12, logical(1))]

# automatic typing for "all indicators"
auto_type <- function(v, x) {
  x <- x[is.finite(x)]
  if (grepl("(_n|nbanks|n_banks|n_eff)$", v)) return("count")
  if (grepl("share|_to_|hhi|top5|coverage|ms_.*_in_fx", v)) return("share")
  if (grepl("^(cod|y_|r_|nb_|zc_|uird_|policy|cd_on|refi|uonia|cpi|core_cpi|real_|nim|c_ibl|credit_cost)", v) && length(x) && max(abs(x)) < 1.5) return("rate")
  "level"
}
skip_auto <- function(v) grepl("(_flow|_avgstock|_nbanks|_eom)$", v)

# ------------------------------ annual classical indicators -------------------
annual_level <- function(v, type) {
  d <- P[, .(month, x = get(v))][is.finite(x)]
  if (!nrow(d)) return(NULL)
  d[, year := year(month)]
  a <- if (type == "level") d[, .(value = x[which.max(month)], months = .N, last_m = max(month)), by = year] else
       d[, .(value = mean(x), months = .N, last_m = max(month)), by = year]
  a[, partial := year == max(year) & (months < 12 | month(last_m) < 12)]     # only the current (incomplete) year is starred
  a[, year_lab := paste0(year, fifelse(partial, "*", ""))]
  a[]
}
classic <- function(v, type, label) {
  a <- annual_level(v, type); if (is.null(a) || nrow(a) < 2) return(NULL)
  k <- if (type %in% c("rate", "share")) 100 else 1                 # rates in %, changes in п.п.
  lev <- a$value * k; b <- lev[1]
  data.table(var = v, label = label, type = type, year = a$year_lab, level = lev,
             abs_chain = c(NA, diff(lev)), abs_base = lev - b,
             growth_chain = c(NA, 100 * lev[-1] / lev[-length(lev)]), growth_base = 100 * lev / b,
             incr_chain = c(NA, 100 * lev[-1] / lev[-length(lev)] - 100), incr_base = 100 * lev / b - 100,
             one_pct = c(NA, lev[-length(lev)] / 100), months = a$months)
}
classic_summary <- function(z) {
  if (is.null(z) || nrow(z) < 2) return(NULL)
  n <- nrow(z); y1 <- z$level[1]; yn <- z$level[n]
  gm <- if (y1 > 0 && yn > 0) (yn / y1)^(1 / (n - 1)) else NA_real_
  data.table(var = z$var[1], label = z$label[1], type = z$type[1], period = paste(z$year[1], "–", z$year[n]),
             mean_level = mean(z$level), mean_abs_incr = (yn - y1) / (n - 1),
             mean_growth = 100 * gm, mean_incr = 100 * gm - 100,
             max_chain_incr = z$abs_chain[which.max(abs(z$abs_chain))], year_max_change = z$year[which.max(abs(z$abs_chain))])
}
G1 <- rbindlist(lapply(seq_len(nrow(K)), function(i) classic(K$var[i], K$type[i], K$label[i])))
G1 <- merge(G1, K[, .(var, group)], by = "var", sort = FALSE)
G2 <- rbindlist(lapply(split(G1, G1$var), classic_summary))
G2 <- merge(K[, .(var, group, label)], G2[, !"label"], by = "var", sort = FALSE)

# ------------------------------ monthly current dynamics -----------------------
pol <- P[, .(month, i = policy_avg)][is.finite(i)]
pol[, di := sign(round(i - shift(i), 4))]
turn <- { nz <- pol[di != 0]; if (nrow(nz) > 1) { last_dir <- nz$di[nrow(nz)]; k <- max(which(nz$di != last_dir), 0L)
          if (k == 0) min(pol$month) else add_months(nz$month[k + 1], -1L) } else min(pol$month) }
WAR_BASE <- as.Date("2022-01-01")
monthly_dyn <- function(v, type, label = v) {
  d <- P[, .(month, x = get(v))][is.finite(x)]
  if (nrow(d) < 13) return(NULL)
  k <- if (type %in% c("rate", "share")) 100 else 1
  last <- d[.N]; at <- function(m) d[month == m, x][1]
  ch <- function(base) { if (is.na(base) || !is.finite(base)) return(NA_real_)
    if (type == "level") 100 * (last$x / base - 1) else if (type == "count") last$x - base else k * (last$x - base) }
  h36 <- d[month > add_months(last$month, -36L), x]; h12 <- d[month > add_months(last$month, -12L), x]
  data.table(var = v, label = label, type = type, unit_change = if (type == "level") "%" else if (type == "count") "од." else "в.п.",
             last_month = format(last$month, "%Y-%m"), last_value = if (type == "level") last$x / 1e3 else k * last$x,
             unit_value = switch(type, level = "млрд грн", count = "од.", "%"),
             d_1m = ch(at(add_months(last$month, -1L))), d_3m = ch(at(add_months(last$month, -3L))),
             d_12m = ch(at(add_months(last$month, -12L))),
             d_ytd = ch(at(as.Date(sprintf("%d-12-01", year(last$month) - 1L)))),
             d_war = ch(at(WAR_BASE)), d_cycle = ch(at(turn)),
             min_12m = (if (type == "level") 1e-3 else if (type == "count") 1 else k) * min(h12),
             max_12m = (if (type == "level") 1e-3 else if (type == "count") 1 else k) * max(h12),
             z_36m = if (length(h36) > 6 && sd(h36) > 0) (last$x - mean(h36)) / sd(h36) else NA_real_,
             pct_rank_hist = 100 * mean(d$x <= last$x), n_months = nrow(d))
}
G3 <- rbindlist(lapply(seq_len(nrow(K)), function(i) monthly_dyn(K$var[i], K$type[i], K$label[i])))
G3 <- merge(K[, .(var, group)], G3, by = "var", sort = FALSE)
numv <- names(P)[vapply(P, is.numeric, logical(1))]
numv <- numv[!skip_auto(numv) & vapply(numv, function(v) sum(is.finite(P[[v]]) & P[[v]] != 0) >= 13, logical(1))]
G4 <- rbindlist(lapply(numv, function(v) monthly_dyn(v, auto_type(v, P[[v]]), K$label[match(v, K$var)] %||% v)))
G4[is.na(label), label := var]
G5 <- rbindlist(lapply(numv, function(v) { t <- auto_type(v, P[[v]]); classic(v, t, v) }))

fact("G_cycle_base", format(turn, "%Y-%m"), "база для «з початку поточного циклу облікової ставки»")
fact("G_last_month", format(max(P$month), "%Y-%m"))

# ------------------------------ tables (Ukrainian headers) ---------------------
UK1 <- c(group = "Блок", label = "Показник", year = "Рік", level = "Рівень (млн грн / %)", abs_chain = "Абс. приріст ланцюговий",
         abs_base = "Абс. приріст базисний", growth_chain = "Темп зростання ланцюговий, %", growth_base = "Темп зростання базисний, %",
         incr_chain = "Темп приросту ланцюговий, %", incr_base = "Темп приросту базисний, %", one_pct = "Абс. значення 1% приросту", months = "Місяців у році")
UK2 <- c(group = "Блок", label = "Показник", period = "Період", mean_level = "Середній рівень", mean_abs_incr = "Середній абсолютний приріст за рік",
         mean_growth = "Середній темп зростання, %", mean_incr = "Середній темп приросту, %", max_chain_incr = "Найбільший річний приріст", year_max_change = "Рік найбільшої зміни")
UK3 <- c(group = "Блок", label = "Показник", last_month = "Останній місяць", last_value = "Значення", unit_value = "Одиниця",
         d_1m = "Зміна за 1 міс.", d_3m = "Зміна за 3 міс.", d_12m = "Зміна за 12 міс.", d_ytd = "З початку року", d_war = "Від 2022-01 (до війни)",
         d_cycle = "Від початку поточного циклу ставки", unit_change = "Одиниця зміни", min_12m = "Мін. за 12 міс.", max_12m = "Макс. за 12 міс.",
         z_36m = "z-оцінка (36 міс.)", pct_rank_hist = "Перцентиль в історії, %", n_months = "Місяців даних")
rn <- function(d, map) { d <- copy(d); keep <- intersect(names(map), names(d)); d <- d[, ..keep]; setnames(d, keep, map[keep]); d }
save_tab(rn(G3, UK3), "G1_dynamics_monthly_key.csv", "Поточна_динаміка")
save_tab(rn(G1, UK1), "G2_dynamics_annual_key.csv", "Річні_показники")
save_tab(rn(G2, UK2), "G3_dynamics_annual_means_key.csv", "Середні_показники")
# wide annual table: indicator × year (level and chain increment)
wl <- dcast(G1, group + label ~ year, value.var = "level")
wi <- dcast(G1, group + label ~ year, value.var = "abs_chain")
save_tab(wl, "G4_annual_levels_wide.csv", "Рівні_за_роками")
save_tab(wi, "G5_annual_abs_change_wide.csv", "Абс_прирости_за_роками")
save_tab(rn(G4, c(var = "Код ряду", UK3)), "G6_dynamics_monthly_all.csv", "Усі_показники_поточна")
save_tab(rn(G5, c(var = "Код ряду", UK1)), "G7_dynamics_annual_all.csv", "Усі_показники_річна")

# ------------------------------ figures --------------------------------------
# G1: volume indices (2022-01 = 100) — what grew and what shrank since the full-scale war
vol <- intersect(c("dep_core4_uah", "dep_core4_fx", "dep_term4_total", "cd_nbu_total", "ovdp_total", "loans_le", "loans_hh"), K$var)
if (length(vol)) {
  ix <- rbindlist(lapply(vol, function(v) { d <- P[, .(month, x = get(v))][is.finite(x) & x > 0]; b <- d[month == WAR_BASE, x]
    if (!length(b)) return(NULL); d[, .(month, index = 100 * x / b, series = K[var == v, label])] }))
  if (nrow(ix)) {
    p <- ggplot(ix[month >= as.Date("2020-01-01")], aes(month, index, colour = series)) + geom_hline(yintercept = 100, colour = INK2, linewidth = .3) +
      geom_line(linewidth = .75) + scale_colour_manual(values = PAL) + guides(colour = guide_legend(nrow = 3)) +
      labs(title = "Динаміка обсягів балансу банків, 2022-01 = 100", subtitle = "Номінальні залишки, млн грн; базисний індекс",
           x = NULL, y = "Індекс (2022-01 = 100)", caption = CAP) + theme_thesis()
    save_fig(p, "G1_volume_indices.png", 9.5, 5.4)
  }
}
# G2: annual chain growth of key volumes (bars by year)
gv <- G1[type == "level" & var %in% vol & is.finite(incr_chain)]
if (nrow(gv)) {
  p <- ggplot(gv, aes(year, incr_chain, fill = label)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
    geom_col(position = position_dodge(width = .8), width = .75) + scale_fill_manual(values = PAL) + guides(fill = guide_legend(nrow = 3)) +
    labs(title = "Ланцюгові темпи приросту обсягів за роками", subtitle = "Рівень на кінець року; * — поточний рік, останній доступний місяць",
         x = NULL, y = "Темп приросту, %", caption = CAP) + theme_thesis()
  save_fig(p, "G2_annual_growth_volumes.png", 10, 5.4)
}
# G3: annual absolute changes of key rates (п.п.)
rr <- intersect(c("policy_avg", "cod4_total", "nb_dep_hh_term_uah", "nb_loan_nfc_all_uah", "y_iea", "nim", "zc_uah_1y_avg"), K$var)
gr <- G1[var %in% rr & is.finite(abs_chain)]
if (nrow(gr)) {
  p <- ggplot(gr, aes(year, abs_chain, fill = label)) + geom_hline(yintercept = 0, colour = INK2, linewidth = .3) +
    geom_col(position = position_dodge(width = .85), width = .8) + scale_fill_manual(values = PAL) + guides(fill = guide_legend(nrow = 4)) +
    labs(title = "Абсолютні ланцюгові прирости ставок за роками", subtitle = "Середньорічні рівні; в.п.; * — поточний рік",
         x = NULL, y = "Абсолютний приріст, в.п.", caption = CAP) + theme_thesis()
  save_fig(p, "G3_annual_change_rates.png", 10, 5.6)
}
# G4a / G4b: 12-month changes — volumes (%) and rates/shares (п.п.) as two figures with own scales
dd <- G3[is.finite(d_12m) & type != "count"]
if (nrow(dd)) {
  for (k in list(list("level", "G4a_change_12m_volumes.png", "Зміна обсягів за 12 місяців", "Зміна за 12 міс., %", 4.6),
                 list(c("rate", "share"), "G4b_change_12m_rates.png", "Зміна ставок і часток за 12 місяців", "Зміна за 12 міс., в.п.", 10))) {
    z <- dd[type %in% k[[1]]]; if (!nrow(z)) next
    z[, label := factor(label, levels = rev(unique(label)))]
    p <- ggplot(z, aes(d_12m, label, fill = d_12m > 0)) + geom_vline(xintercept = 0, colour = INK2, linewidth = .3) + geom_col(width = .7) +
      scale_fill_manual(values = c(`TRUE` = PAL[1], `FALSE` = PAL[2]), guide = "none") +
      labs(title = sprintf("%s (до %s)", k[[3]], format(max(P$month), "%Y-%m")), x = k[[4]], y = NULL, caption = CAP) +
      theme_thesis() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = GRID, linewidth = .25))
    save_fig(p, k[[2]], 8.5, k[[5]])
  }
}

# ------------------------------ Excel with formatting ---------------------------
wb <- openxlsx::createWorkbook()
hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#1F4E78", fontColour = "white", wrapText = TRUE, valign = "center")
n2 <- openxlsx::createStyle(numFmt = "0.00"); n1 <- openxlsx::createStyle(numFmt = "#,##0.0")
add <- function(name, d, note) {
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, note, startRow = 1); openxlsx::addStyle(wb, name, openxlsx::createStyle(textDecoration = "italic", fontColour = "#52514e"), 1, 1)
  openxlsx::writeData(wb, name, as.data.frame(d), startRow = 3, headerStyle = hs, withFilter = TRUE)
  numc <- which(vapply(d, is.numeric, logical(1)))
  if (length(numc)) openxlsx::addStyle(wb, name, n2, rows = 4:(nrow(d) + 3), cols = numc, gridExpand = TRUE)
  openxlsx::setColWidths(wb, name, cols = 1:ncol(d), widths = c(18, 46, rep(13, max(0, ncol(d) - 2)))[1:ncol(d)])
  openxlsx::setRowHeights(wb, name, rows = 3, heights = 45); openxlsx::freezePane(wb, name, firstActiveRow = 4, firstActiveCol = 3)
  for (cn in grep("Зміна за 12|Темп приросту ланцюговий|Абс. приріст ланцюговий", names(d))) {
    openxlsx::conditionalFormatting(wb, name, cols = cn, rows = 4:(nrow(d) + 3), type = "colourScale", style = c("#e34948", "#ffffff", "#2a78d6"))
  }
}
NOTE <- "Ставки і частки: рівні у %, прирости у в.п.; обсяги: рівні у млн грн (річні) / млрд грн (поточні), темпи у %. * — неповний рік. Джерело: НБУ, розрахунки автора."
add("Поточна_динаміка", rn(G3, UK3), paste("Поточна динаміка ключових показників.", NOTE, sprintf("База циклу: %s.", format(turn, "%Y-%m"))))
add("Річні_показники", rn(G1, UK1), paste("Класичні показники динаміки за роками.", NOTE))
add("Середні_показники", rn(G2, UK2), paste("Середні показники динаміки (середній темп зростання — геометричний).", NOTE))
add("Рівні_за_роками", wl, "Рівні за роками: обсяги — на кінець року (млн грн), ставки і частки — середньорічні (%).")
add("Абс_прирости_за_роками", wi, "Абсолютні ланцюгові прирости за роками: обсяги — млн грн, ставки і частки — в.п.")
add("Усі_показники_поточна", rn(G4, c(var = "Код ряду", UK3)), paste("Усі числові ряди мартів (тип визначено автоматично).", NOTE))
add("Усі_показники_річна", rn(G5, c(var = "Код ряду", UK1)), paste("Усі числові ряди мартів, річні показники динаміки.", NOTE))
openxlsx::saveWorkbook(wb, file.path(OUT, "Dynamics_indicators.xlsx"), overwrite = TRUE)
save_tab(FACTS, "G0_key_facts_dynamics.csv", "Key_facts")
log_msg("DYNAMICS COMPLETE: ", nrow(K), " key indicators, ", length(numv), " series in total -> ", file.path(OUT, "Dynamics_indicators.xlsx"))
