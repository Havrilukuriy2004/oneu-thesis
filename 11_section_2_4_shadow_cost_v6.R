# ============================================================================
# 11_section_2_4_shadow_cost_v6.R — ПІДРОЗДІЛ 2.4
# Shadow Funding Cost: економічна вартість депозитного ресурсу з урахуванням обов'язкових резервів,
# внесків до ФГВФО і валютного ризику
# ----------------------------------------------------------------------------
# Reads marts (02). Output: output_nbu/section_2_4/ (tables/S24_*, figures/S24_*, Section_2_4.xlsx)
# and the inputs of the dynamic Excel (S24_rr_schedule.csv, S24_bank_products_latest.csv).
#
# Logic (Brock 1989; Reinhart & Reinhart 1999; Montoro & Moreno 2011; Federico, Vegh & Vuletin 2014; Glocker & Towbin 2015;
# Alper, Binici & Demiralp 2018): a bank that raises 1 UAH of deposits keeps ρ in required reserves (in hryvnia, for all currencies);
# a share ρ_cov may be held in benchmark OVDP (yield y_b), the rest ρ_cash sits unremunerated on the correspondent account; household
# deposits also carry the DGF fee f. Against the risk-free alternative i (NBU overnight CD):
#   economic cost      EC  = r + ρ_cash·i + ρ_cov·(i − y_b) + f            (nominal rate + implicit tax τ)
#   hurdle (SFC)       SFC = (r + f − ρ_cov·y_b) / (1 − ρ)                (break-even yield on the investable funds)
#   FX in UAH terms    r_ueq = (1 + r_fx)(1 + E[Δs]) − 1                   (E[Δs]: banks' expected depreciation, NBU surveys)
# The nominal cost of deposits (2.1) understates the economic cost exactly when the NBU uses reserve requirements as a policy tool
# (2023–2024: ratios raised five times, OVDP coverage introduced) — and FX funding looks cheap only before reserves and depreciation.
#
# Blocks
#  R1  reserve-requirement schedule 2018–2026 (data_nbu/local_inputs/reserve_requirements.csv, created on first run; edit if the NBU changes ratios)
#  R2  product-level EC, SFC and tax per bank-month (ФО/СГ × на вимогу/строкові × грн/валюта)
#  R3  bank and system economic cost vs nominal CoD; tax decomposition (correspondent account, OVDP, DGF); periods, ownership, latest by bank
#  R4  UAH vs FX funding in UAH terms: is FX funding cheaper once reserves and expected depreciation are counted?
#  R5  carry of CD "parking": i − SFC vs i − r (input for chapter 3), inside R3
#  R6  reserve-requirement reforms as quasi-experiments (2023-03, 2023-05, 2023-09, 2024-10): continuous DiD, exposure = pre-reform share of the
#      newly reservable base; outcomes: deposit rates, CD and OVDP holdings, deposit volumes, corporate loans
#  R7  plausibility: cash reserves implied by the schedule vs balances on correspondent accounts
#  R8  dollarization: FX share of deposits on the depositor return gap and the reserve-tax gap
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_models_v6.R"), encoding = "UTF-8")
.need(c("sandwich"))
init_output("S24", folder = "section_2_4")
CAP <- "Джерело: розрахунки автора за даними НБУ (OSB, нормативи обов'язкового резервування, ФГВФО, опитування очікувань)."
col_or_na <- function(dt, v) if (v %in% names(dt)) dt[[v]] else rep(NA_real_, nrow(dt))
z0 <- function(x) fifelse(is.finite(x), x, 0)
blk <- function(name, expr) { t0 <- Sys.time()
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) { status(name, "error", conditionMessage(e)); message("  [", name, "] ", conditionMessage(e)); FALSE })
  if (ok) status(name, "ok", sprintf("%.1f с", as.numeric(difftime(Sys.time(), t0, units = "secs")))) }
t0_all <- Sys.time()

PAR <- list(
  share_hh_term_le92 = 0.30,  # share of HH term deposits with initial maturity ≤ 92 days (not split in the data)
  dgf_uah = 0.005,            # ФГВФО regular fee, base annual rate on HH deposits in UAH (Law "On the DGS", art. 23)
  dgf_fx = 0.008,             #   … in foreign currency
  dgf_risk_mult = 1.0,        # differentiated fee ≥ base rate; 1 = base
  fx_alt_yield = 0.03,        # yield on FX placements abroad / nostro if no NS curve
  noncov_hh = 0.10)           # part of the ratio for HH demand / ≤92 days not coverable by OVDP (10 pp of the base)

# ------------------------------ R1. reserve requirement schedule ---------------
# Sources: NBU Board decisions No. 926-рш (12.12.2019), 587-рш (16.12.2022), 40-рш (30.01.2023), 71-рш (23.02.2023),
# 115 (24.03.2023), 275-рш (09.08.2023), 345-рш (20.09.2024); exclusions of 10.10.2025 do not change ratios.
RR_DEFAULT <- fread(text = "
from,group,ccy,ratio,cov_max,note
2018-01-01,hh_demand,uah,0.065,0,поточні/вимоги 6.5%
2018-01-01,hh_demand,fx,0.065,0,
2018-01-01,corp_demand,uah,0.065,0,
2018-01-01,corp_demand,fx,0.065,0,
2018-01-01,hh_term_short,uah,0.03,0,строкові 3%
2018-01-01,hh_term_short,fx,0.03,0,
2018-01-01,hh_term_long,uah,0.03,0,
2018-01-01,hh_term_long,fx,0.03,0,
2018-01-01,corp_term,uah,0.03,0,
2018-01-01,corp_term,fx,0.03,0,
2020-03-10,hh_demand,uah,0,0,926-рш: гривня 0% / валюта 10%
2020-03-10,hh_demand,fx,0.10,0,
2020-03-10,corp_demand,uah,0,0,
2020-03-10,corp_demand,fx,0.10,0,
2020-03-10,hh_term_short,uah,0,0,
2020-03-10,hh_term_short,fx,0.10,0,
2020-03-10,hh_term_long,uah,0,0,
2020-03-10,hh_term_long,fx,0.10,0,
2020-03-10,corp_term,uah,0,0,
2020-03-10,corp_term,fx,0.10,0,
2023-01-11,hh_demand,uah,0.05,0.5,587-рш: вимоги +5 в.п.; покриття бенчмарк-ОВДП до 50%
2023-01-11,hh_demand,fx,0.15,0.5,
2023-01-11,corp_demand,uah,0.05,0.5,
2023-01-11,corp_demand,fx,0.15,0.5,
2023-01-11,hh_term_short,uah,0,0.5,
2023-01-11,hh_term_short,fx,0.10,0.5,
2023-01-11,hh_term_long,uah,0,0.5,
2023-01-11,hh_term_long,fx,0.10,0.5,
2023-01-11,corp_term,uah,0,0.5,
2023-01-11,corp_term,fx,0.10,0.5,
2023-02-11,hh_demand,uah,0.10,0.5,40-рш: вимоги +5 в.п.
2023-02-11,hh_demand,fx,0.20,0.5,
2023-02-11,corp_demand,uah,0.10,0.5,
2023-02-11,corp_demand,fx,0.20,0.5,
2023-03-11,hh_demand,uah,0.20,0.5,71-рш: поточні ФО +10 в.п. (не покриваються ОВДП)
2023-03-11,hh_demand,fx,0.30,0.5,
2023-05-11,hh_term_short,uah,0.20,0.5,115: депозити ФО до 92 днів = як поточні
2023-05-11,hh_term_short,fx,0.30,0.5,
2023-09-11,corp_term,uah,0.10,0.5,275-рш: строкові СГ 0→10% / 10→20%
2023-09-11,corp_term,fx,0.20,0.5,
2024-10-11,hh_demand,uah,0.25,0.6,345-рш: +5 в.п.; покриття ОВДП до 60%
2024-10-11,hh_demand,fx,0.35,0.6,
2024-10-11,hh_term_short,uah,0.25,0.6,
2024-10-11,hh_term_short,fx,0.35,0.6,
2024-10-11,hh_term_long,uah,0,0.6,
2024-10-11,hh_term_long,fx,0.15,0.6,
2024-10-11,corp_demand,uah,0.15,0.6,
2024-10-11,corp_demand,fx,0.25,0.6,
2024-10-11,corp_term,uah,0.15,0.6,
2024-10-11,corp_term,fx,0.25,0.6,
", colClasses = list(character = c("group", "ccy", "note")))
RR_FILE <- file.path(LOCAL_DIR, "reserve_requirements.csv")
if (!file.exists(RR_FILE)) fwrite(RR_DEFAULT, RR_FILE, bom = TRUE)
RR <- tryCatch(fread(RR_FILE, encoding = "UTF-8", colClasses = list(character = c("group", "ccy", "note"))), error = function(e) RR_DEFAULT)
RR[, from := as.Date(from)]
setorder(RR, group, ccy, from)
rr_at <- function(month, g, c) {                       # ratio and OVDP cap in force at mid-month
  z <- RR[group == g & ccy == c]; i <- findInterval(as.numeric(month + 14), as.numeric(z$from))
  list(ratio = fifelse(i > 0, z$ratio[pmax(i, 1)], NA_real_), cov = fifelse(i > 0, z$cov_max[pmax(i, 1)], 0)) }
save_tab(RR, "S24_rr_schedule.csv", "R1_rr_schedule")

# ------------------------------ data --------------------------------------
B <- store_read("mart_bank_month_v6"); B[, `:=`(month = as.Date(month), nkb = as.character(nkb))]
# Capital controls are exported separately; they do not replace the reserve base.
capcols <- intersect(c("nkb", "month", "report_date", "capital_regime", "headroom_total_min_pp", "headroom_tier1_min_pp", "lcr_all_pct", "nsfr_pct", "capital_source_conflict"), names(B))
save_tab(B[, ..capcols], "S24_prudential_controls.csv", "prudential_controls")
S <- store_read("mart_system_month_v6"); S[, month := as.Date(month)]
MAC <- S[, .(month, policy = policy_avg, cd_on = fcoalesce(col_or_na(S, "cd_on_rate_avg"), policy_avg),
             y_bench = fcoalesce(col_or_na(S, "zc_uah_3y_avg"), col_or_na(S, "fm_ovdp_prim"), col_or_na(S, "ovdp_uah_all")),
             y_fx = fcoalesce(col_or_na(S, "zc_fx_1y_avg"), PAR$fx_alt_yield),
             edep = fcoalesce(col_or_na(S, "edep_banks"), col_or_na(S, "edep_mean"), col_or_na(S, "edep_hh")))]
setorder(MAC, month); MAC[, edep := nafill(edep, "locf")]
B <- merge(B, MAC, by = "month", all.x = TRUE); setorder(B, nkb, month)

PROD <- rbindlist(list(
  list("hh_demand",   "hh_demand",   "dep_hh_demand",                "r_hh_demand",   TRUE),
  list("hh_term",     NA,            "dep_hh_term",                  "r_hh_term",     TRUE),
  list("corp_demand", "corp_demand", "dep_corp_demand+dep_nbfi_demand+dep_budget", "r_corp_demand", FALSE),
  list("corp_term",   "corp_term",   "dep_corp_term+dep_nbfi_term",  "r_corp_term",   FALSE)))
setnames(PROD, c("prod", "group", "stock", "rate", "insured"))
sumc <- function(D, base, ccy) { vs <- paste0(strsplit(base, "+", fixed = TRUE)[[1]], "_", ccy); vs <- intersect(vs, names(D)); if (!length(vs)) return(rep(NA_real_, nrow(D))); Reduce(`+`, lapply(vs, function(v) z0(D[[v]]))) }

# ------------------------------ R2. product SFC --------------------------------
blk("R2 продуктова SFC", {
  out <- list()
  for (k in seq_len(nrow(PROD))) for (cc in c("uah", "fx")) {
    p <- PROD[k]; st <- sumc(B, p$stock, cc); r <- col_or_na(B, paste0(p$rate, "_", cc))
    if (p$prod == "hh_term") {                                   # mix of ≤92 days and ≥93 days
      a <- rr_at(B$month, "hh_term_short", cc); b <- rr_at(B$month, "hh_term_long", cc); s <- PAR$share_hh_term_le92
      rho <- s * a$ratio + (1 - s) * b$ratio; cov <- a$cov
      nc <- fifelse(B$month >= as.Date("2023-05-01"), s * pmin(PAR$noncov_hh, a$ratio), 0)
    } else {
      a <- rr_at(B$month, p$group, cc); rho <- a$ratio; cov <- a$cov
      nc <- if (p$group == "hh_demand") fifelse(B$month >= as.Date("2023-03-01"), pmin(PAR$noncov_hh, rho), 0) else 0
    }
    rho_cov <- pmin(cov * rho, pmax(rho - nc, 0)); rho_cash <- rho - rho_cov
    f <- if (p$insured) (if (cc == "uah") PAR$dgf_uah else PAR$dgf_fx) * PAR$dgf_risk_mult else 0
    yb <- B$y_bench                      # reserves are kept in hryvnia for all currencies → coverage by UAH benchmark OVDP
    r_ueq <- if (cc == "fx") (1 + r) * (1 + B$edep) - 1 else r
    out[[length(out) + 1]] <- data.table(nkb = B$nkb, month = B$month, prod = p$prod, ccy = cc, stock = st, r = r, r_ueq = r_ueq, rho = rho, rho_cov = rho_cov,
                                         rho_cash = rho_cash, dgf = f, y_cov = yb, edep = B$edep, i_alt = B$cd_on)
  }
  P <- rbindlist(out)
  P <- P[is.finite(r) & is.finite(rho) & is.finite(stock) & stock > 0]
  # (a) break-even hurdle on investable funds:  SFC = (r + f − ρ_cov·y_b) / (1 − ρ)   (Reinhart & Reinhart 1999)
  P[, `:=`(sfc = (r + dgf - fifelse(rho_cov == 0, 0, rho_cov * y_cov)) / (1 - rho), sfc_ueq = (r_ueq + dgf - fifelse(rho_cov == 0, 0, rho_cov * y_cov)) / (1 - rho))]
  # (b) economic cost per unit of deposits = r + implicit tax; tax = forgone income vs the NBU overnight CD (risk-free alternative):
  #     reserves on the correspondent account ρ_cash·i,  OVDP-covered reserves ρ_cov·(i − y_b),  DGF fee f
  P[, `:=`(tax_cash = fifelse(rho_cash == 0, 0, rho_cash * i_alt), tax_cov = fifelse(rho_cov == 0, 0, rho_cov * (i_alt - y_cov)), tax_dgf = dgf)]
  P[, `:=`(tax = tax_cash + tax_cov + tax_dgf)]
  P[, `:=`(ec = r + tax, ec_ueq = r_ueq + tax)]
  PRODSFC <<- P
  wm <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  sysp <- P[, .(stock = sum(stock), r = wm(r, stock), r_ueq = wm(r_ueq, stock), rho = wm(rho, stock), rho_cov = wm(rho_cov, stock), sfc = wm(sfc, stock),
                sfc_ueq = wm(sfc_ueq, stock), tax_cash = wm(tax_cash, stock), tax_cov = wm(tax_cov, stock), tax_dgf = wm(tax_dgf, stock), tax = wm(tax, stock),
                ec = wm(ec, stock), ec_ueq = wm(ec_ueq, stock)), by = .(month, prod, ccy)]
  save_tab(sysp, "S24_R2_products_system_monthly.csv", "R2_products_system")
  lt <- sysp[month == max(month)]
  save_tab(lt[, .(`Продукт` = prod, `Валюта` = ccy, `Залишок, млрд грн` = round(stock / 1e3, 1), `Ставка, %` = round(100 * r, 2), `Норматив, %` = round(100 * rho, 1),
                  `Резерви на кор. рахунку, в.п.` = round(100 * tax_cash, 2), `Резерви в ОВДП, в.п.` = round(100 * tax_cov, 2), `ФГВФО, в.п.` = round(100 * tax_dgf, 2),
                  `Економічна вартість, %` = round(100 * ec, 2), `Економічна вартість у грн-еквіваленті, %` = round(100 * ec_ueq, 2),
                  `Поріг дохідності вільних коштів (SFC, грн-екв.), %` = round(100 * sfc_ueq, 2))], "S24_R2_products_latest.csv", "R2_latest")
})

# ------------------------------ R3. bank and system shadow CoD -------------------
blk("R3 тіньова вартість банків", {
  P <- PRODSFC; wm <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  BK <- P[, .(dep = sum(stock), cod_nom = wm(r, stock), cod_ueq = wm(r_ueq, stock), ec = wm(ec, stock), ec_ueq = wm(ec_ueq, stock), sfc = wm(sfc_ueq, stock),
              tax_cash = wm(tax_cash, stock), tax_cov = wm(tax_cov, stock), tax_dgf = wm(tax_dgf, stock), rho_eff = wm(rho, stock),
              fx_share = sum(stock[ccy == "fx"]) / sum(stock)), by = .(nkb, month)]
  BK[, wedge := ec - cod_nom]
  BK <- merge(BK, unique(B[, .(nkb, month, bank = fullname, owner, cd_on)]), by = c("nkb", "month"), all.x = TRUE)
  BK[, `:=`(carry_cd = cd_on - sfc, carry_cd_nominal = cd_on - cod_nom)]
  SHADOW <<- BK
  SYS <- BK[, .(banks = .N, dep = sum(dep), cod_nom = wm(cod_nom, dep), cod_ueq = wm(cod_ueq, dep), ec = wm(ec, dep), ec_ueq = wm(ec_ueq, dep), sfc = wm(sfc, dep),
                tax_cash = wm(tax_cash, dep), tax_cov = wm(tax_cov, dep), tax_dgf = wm(tax_dgf, dep), rho_eff = wm(rho_eff, dep), cd_on = mean(cd_on)), by = month][order(month)]
  SYS[, `:=`(wedge = ec - cod_nom, carry_cd = cd_on - sfc, carry_cd_nominal = cd_on - cod_nom)]
  save_tab(SYS, "S24_R3_system_monthly.csv", "R3_system")
  latest <- max(BK$month)
  L <- BK[month == latest][order(-dep)][, .(nkb, bank, owner, dep_bn = dep / 1e3, cod_nom = 100 * cod_nom, ec = 100 * ec, ec_ueq = 100 * ec_ueq, wedge_pp = 100 * wedge,
                                             tax_cash = 100 * tax_cash, tax_cov = 100 * tax_cov, tax_dgf = 100 * tax_dgf, sfc = 100 * sfc, rho_eff = 100 * rho_eff,
                                             fx_share = 100 * fx_share, carry_cd = 100 * carry_cd)]
  save_tab(L, "S24_R3_banks_latest.csv", "R3_banks")
  OWN <- BK[month == latest & !is.na(owner) & nzchar(owner), .(banks = .N, dep_bn = sum(dep) / 1e3, cod_nom = 100 * wm(cod_nom, dep), ec = 100 * wm(ec, dep),
             wedge_pp = 100 * wm(wedge, dep), sfc = 100 * wm(sfc, dep), rho_eff = 100 * wm(rho_eff, dep), fx_share = 100 * wm(fx_share, dep), carry_cd = 100 * wm(carry_cd, dep)), by = owner]
  save_tab(OWN, "S24_R3_by_ownership_latest.csv", "R3_ownership")
  per <- SYS[, .(months = .N, cod_nom = 100 * mean(cod_nom), ec = 100 * mean(ec), wedge = 100 * mean(wedge), tax_cash = 100 * mean(tax_cash), tax_cov = 100 * mean(tax_cov),
                 tax_dgf = 100 * mean(tax_dgf), rho_eff = 100 * mean(rho_eff), sfc = 100 * mean(sfc), carry_cd = 100 * mean(carry_cd, na.rm = TRUE),
                 carry_cd_nominal = 100 * mean(carry_cd_nominal, na.rm = TRUE)),
             by = .(period = fcase(month < as.Date("2020-03-01"), "2018-01–2020-02", month < as.Date("2023-01-01"), "2020-03–2022-12",
                                   month < as.Date("2024-10-01"), "2023-01–2024-09", default = "з 2024-10"))]
  save_tab(per, "S24_R3_periods.csv", "R3_periods")
  z <- SYS[month == latest]
  fact("R3_latest", sprintf("%s: номінальна вартість депозитів %.2f%%, економічна %.2f%% (клин %+.2f в.п.: резерви на кор. рахунку %+.2f, резерви в ОВДП %+.2f, ФГВФО %+.2f); ефективний норматив %.1f%%; поріг дохідності вільних коштів %.2f%%",
       format(latest, "%m.%Y"), 100 * z$cod_nom, 100 * z$ec, 100 * z$wedge, 100 * z$tax_cash, 100 * z$tax_cov, 100 * z$tax_dgf, 100 * z$rho_eff, 100 * z$sfc))
  sl <- melt(SYS[, .(month, `Номінальна вартість депозитів` = 100 * cod_nom, `Економічна вартість (з резервами і ФГВФО)` = 100 * ec, `Ставка ДС овернайт` = 100 * cd_on)], id.vars = "month")
  p <- ggplot(sl, aes(month, value, colour = variable, linetype = variable)) + geom_line(linewidth = .7) +
    labs(title = "Номінальна і економічна вартість депозитного фондування", subtitle = "Економічна = r + ρ_кор·i + ρ_ОВДП·(i − y_ОВДП) + f; середньозважена за депозитами банків",
         x = NULL, y = "%", colour = NULL, linetype = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "S24_R3_nominal_vs_shadow.png", 8.5, 4.6)
  dl <- melt(SYS[, .(month, `Резерви на кор. рахунку` = 100 * tax_cash, `Резерви в бенчмарк-ОВДП` = 100 * tax_cov, `ФГВФО` = 100 * tax_dgf)], id.vars = "month")
  p <- ggplot(dl, aes(month, value, fill = variable)) + geom_col(width = 25) + geom_hline(yintercept = 0, linewidth = .3) +
    scale_fill_grey(start = .25, end = .75) +
    labs(title = "Неявний податок на депозитне фондування", subtitle = "в.п. до номінальної вартості депозитів", x = NULL, y = "в.п.", fill = NULL, caption = CAP) + theme_thesis()
  save_fig(p, "S24_R3_wedge_decomposition.png", 8.5, 4.6)
})

# ------------------------------ R4. UAH vs FX ---------------------------------
blk("R4 гривня vs валюта", {
  P <- PRODSFC
  W <- P[, .(r = weighted.mean(r, stock), r_ueq = weighted.mean(r_ueq, stock), ec_ueq = weighted.mean(ec_ueq, stock), rho = weighted.mean(rho, stock)), by = .(month, prod, ccy)]
  W2 <- dcast(W, month + prod ~ ccy, value.var = c("r", "r_ueq", "ec_ueq", "rho"))
  W2[, `:=`(gap_nominal = r_fx - r_uah, gap_ueq = r_ueq_fx - r_ueq_uah, gap_sfc_ueq = ec_ueq_fx - ec_ueq_uah)]
  save_tab(W2, "S24_R4_uah_vs_fx_monthly.csv", "R4_uah_fx")
  sm <- W2[, .(months = .N, gap_nominal = 100 * mean(gap_nominal, na.rm = TRUE), gap_ueq = 100 * mean(gap_ueq, na.rm = TRUE), gap_sfc_ueq = 100 * mean(gap_sfc_ueq, na.rm = TRUE),
               share_fx_cheaper_nominal = 100 * mean(gap_nominal < 0, na.rm = TRUE), share_fx_cheaper_ueq = 100 * mean(gap_ueq < 0, na.rm = TRUE), share_fx_cheaper_shadow = 100 * mean(gap_sfc_ueq < 0, na.rm = TRUE)),
           by = .(prod, period = fifelse(month < as.Date("2022-03-01"), "до війни", "війна"))]
  save_tab(sm, "S24_R4_uah_vs_fx_summary.csv", "R4_summary")
})

# ------------------------------ R6. RR reforms as quasi-experiments ------------
fe2_bank <- function(d, yname, xnames, tol = 1e-9, maxit = 500) {
  cols <- c(yname, xnames); d <- d[complete.cases(d[, ..cols])]; if (nrow(d) < 60) return(NULL)
  Z <- as.matrix(d[, ..cols]); g1 <- d$nkb; g2 <- as.character(d$month); n1 <- table(g1); n2 <- table(g2)
  for (it in seq_len(maxit)) { Z0 <- Z
    Z <- Z - (rowsum(Z, g1) / as.vector(n1[sort(unique(g1))]))[g1, , drop = FALSE]
    Z <- Z - (rowsum(Z, g2) / as.vector(n2[sort(unique(g2))]))[g2, , drop = FALSE]
    if (max(abs(Z - Z0)) < tol) break }
  yv <- Z[, 1]; X <- Z[, -1, drop = FALSE]; keep <- apply(X, 2, function(c) sd(c) > 1e-10); X <- X[, keep, drop = FALSE]
  XtX <- crossprod(X); if (rcond(XtX) < 1e-12) return(NULL); Bi <- solve(XtX); b <- Bi %*% crossprod(X, yv); u <- as.vector(yv - X %*% b)
  hb <- rowsum(X * u, g1); G <- nrow(hb); Sm <- crossprod(hb) * G / (G - 1) * (nrow(X) - 1) / (nrow(X) - ncol(X))
  V <- Bi %*% Sm %*% Bi; dimnames(V) <- list(colnames(X), colnames(X)); list(b = setNames(as.vector(b), colnames(X)), V = V, n = nrow(X), nb = G) }
blk("R6 реформи резервування", {
  B[, `:=`(dep_all = z0(col_or_na(B, "dep_core6_total")) + z0(col_or_na(B, "dep_budget_total")))]
  B[, `:=`(e_hh_dem = (z0(col_or_na(B, "dep_hh_demand_uah")) + z0(col_or_na(B, "dep_hh_demand_fx"))) / dep_all,
           e_hh_term = (z0(col_or_na(B, "dep_hh_term_uah")) + z0(col_or_na(B, "dep_hh_term_fx"))) / dep_all,
           e_corp_term = (z0(col_or_na(B, "dep_corp_term_uah")) + z0(col_or_na(B, "dep_nbfi_term_uah"))) / dep_all,
           e_all_res = (z0(col_or_na(B, "dep_hh_demand_uah")) + z0(col_or_na(B, "dep_corp_demand_uah")) + z0(col_or_na(B, "dep_corp_term_uah"))) / dep_all)]
  B[, `:=`(o_r_hh_dem = 100 * col_or_na(B, "r_hh_demand_uah"), o_r_hh_term = 100 * col_or_na(B, "r_hh_term_uah"), o_r_corp_term = 100 * col_or_na(B, "r_corp_term_uah"),
           o_l_cd = fifelse(z0(col_or_na(B, "cd_nbu_total")) > 1, 100 * log(z0(col_or_na(B, "cd_nbu_total"))), NA_real_),
           o_l_ovdp = fifelse(z0(col_or_na(B, "ovdp_uah")) > 1, 100 * log(z0(col_or_na(B, "ovdp_uah"))), NA_real_),
           o_l_hh_term = fifelse(z0(col_or_na(B, "dep_hh_term_uah")) > 1, 100 * log(z0(col_or_na(B, "dep_hh_term_uah"))), NA_real_),
           o_l_corp_term = fifelse(z0(col_or_na(B, "dep_corp_term_uah")) > 1, 100 * log(z0(col_or_na(B, "dep_corp_term_uah"))), NA_real_),
           o_r_hh_term_fx = 100 * col_or_na(B, "r_hh_term_fx"),
           o_l_loans_corp = fifelse(z0(col_or_na(B, "loans_corp_all_uah")) > 1, 100 * log(z0(col_or_na(B, "loans_corp_all_uah"))), NA_real_))]
  REF <- data.table(date = as.Date(c("2023-03-01", "2023-05-01", "2023-09-01", "2024-10-01")),
                    label = c("2023-03: поточні ФО +10 в.п.", "2023-05: депозити ФО ≤92 дні", "2023-09: строкові СГ 0→10%", "2024-10: усі +5 в.п., ОВДП до 60%"),
                    expo = c("e_hh_dem", "e_hh_term", "e_corp_term", "e_all_res"))
  OUTS <- c(o_r_hh_dem = "Ставка коштів ФО на вимогу, грн", o_r_hh_term = "Ставка строкових депозитів ФО, грн", o_r_corp_term = "Ставка строкових депозитів СГ, грн",
            o_l_cd = "Депсертифікати НБУ (log)", o_l_ovdp = "ОВДП у гривні (log)", o_l_hh_term = "Строкові депозити ФО, грн (log)", o_l_corp_term = "Строкові депозити СГ, грн (log)",
            o_r_hh_term_fx = "Ставка строкових депозитів ФО, валюта", o_l_loans_corp = "Кредити СГ у гривні (log)")
  pre <- 6L; post <- 9L; kn <- function(k) paste0("ek_", ifelse(k < 0, paste0("m", -k), k))
  RES <- list(); SUM <- list()
  for (ri in seq_len(nrow(REF))) for (ov in names(OUTS)) {
    R0 <- REF$date[ri]; po <- min(post, (year(max(B$month)) - year(R0)) * 12L + month(max(B$month)) - month(R0)); if (po < 2) next
    D <- B[month >= add_months(R0, -pre) & month <= add_months(R0, po), .(nkb, month, y = .SD[[1]], ex = .SD[[2]]), .SDcols = c(ov, REF$expo[ri])]
    E <- D[month < R0, .(E = mean(ex, na.rm = TRUE)), by = nkb][is.finite(E)]
    if (nrow(E) < 15 || sd(E$E) == 0) next
    E[, E := (E - mean(E)) / sd(E)]; D <- merge(D, E, by = "nkb")[is.finite(y)]
    full <- D[, .N, by = nkb][N >= .8 * (pre + po + 1), nkb]; D <- D[nkb %in% full]
    D[, k := (year(month) - year(R0)) * 12L + month(month) - month(R0)]
    ks <- setdiff(-pre:po, -1L); for (kk in ks) D[, (kn(kk)) := as.numeric(k == kk) * E]
    f <- fe2_bank(D, "y", kn(ks)); if (is.null(f)) next
    cf <- data.table(k = ks, b = f$b[kn(ks)], se = sqrt(diag(f$V)[kn(ks)]))
    prek <- intersect(kn(ks[ks < -1]), names(f$b)); postk <- intersect(kn(ks[ks >= 0]), names(f$b))
    Wd <- tryCatch({ bp <- f$b[prek]; as.numeric(t(bp) %*% solve(f$V[prek, prek]) %*% bp) }, error = function(e) NA_real_)
    a <- rep(1 / length(postk), length(postk)); pa <- sum(a * f$b[postk]); pse <- sqrt(as.numeric(t(a) %*% f$V[postk, postk] %*% a))
    RES[[length(RES) + 1]] <- cf[, `:=`(reform = REF$label[ri], outcome = ov, label = OUTS[[ov]])]
    SUM[[length(SUM) + 1]] <- data.table(reform = REF$label[ri], outcome = ov, label = OUTS[[ov]], post_avg = pa, se = pse, p = 2 * pnorm(-abs(pa / pse)),
                                         pretrend_chi2 = Wd, pretrend_p = pchisq(Wd, length(prek), lower.tail = FALSE), banks = f$nb, obs = f$n)
  }
  RES <- rbindlist(RES); SUM <- rbindlist(SUM)
  save_tab(RES, "S24_R6_rr_reforms_event_study.csv", "R6_ES"); save_tab(SUM, "S24_R6_rr_reforms_summary.csv", "R6_summary")
  if (nrow(RES)) {
    z <- RES[outcome %in% c("o_r_hh_dem", "o_r_hh_term", "o_l_cd", "o_l_ovdp")]
    z <- rbind(z, unique(z[, .(reform, outcome, label)])[, `:=`(k = -1L, b = 0, se = 0)], fill = TRUE)
    p <- ggplot(z, aes(k, b)) + geom_hline(yintercept = 0, linewidth = .3) + geom_vline(xintercept = -.5, linetype = "22", linewidth = .3) +
      geom_ribbon(aes(ymin = b - 1.645 * se, ymax = b + 1.645 * se), fill = "grey80") + geom_line(linewidth = .6) + geom_point(size = 1) +
      facet_grid(label ~ reform, scales = "free_y") +
      labs(title = "Реформи обов'язкового резервування: подієве дослідження", subtitle = "Банки з вищою (на 1 с.в.) часткою бази, що стала резервованою; FE банку і місяця; k = −1 — база",
           x = "Місяці від реформи", y = NULL, caption = CAP) + theme_thesis()
    save_fig(p, "S24_R6_rr_reforms.png", 10, 9)
  }
})

# ------------------------------ R8. dollarization of deposits -----------------------
# De Nicolò, Honohan & Ize (2005); Basso, Calvo-González & Jurgilas (2011); Broda & Levy Yeyati (2006): the currency composition of
# deposits responds to the expected-return differential for depositors; on the bank side the higher FX reserve ratio is a tax on FX funding.
# Δ12 FX share of a product (t → t+12) on (i) the depositor return gap in UAH terms r_fx·(1+E[Δs]) − r_uah, (ii) the reserve-tax gap τ_fx − τ_uah;
# bank and month FE, bank-clustered SE.
blk("R8 доларизація депозитів", {
  P <- PRODSFC[prod %in% c("hh_term", "hh_demand", "corp_term")]
  W <- dcast(P, nkb + month + prod ~ ccy, value.var = c("stock", "r", "r_ueq", "tax"))
  W <- W[is.finite(stock_uah + stock_fx) & stock_uah + stock_fx > 0]
  W[, `:=`(fxs = 100 * stock_fx / (stock_uah + stock_fx), ret_gap = 100 * (r_ueq_fx - r_uah), tax_gap = 100 * (tax_fx - tax_uah))]
  setorder(W, nkb, prod, month)
  W[, f12_dfxs := calendar_shift(fxs, month, 12, type = "lead") - fxs, by = .(nkb, prod)]
  W[, f12_dfxs := fifelse(abs(f12_dfxs) > 50, NA_real_, f12_dfxs)]
  # (a) bank panel, two-way FE: identified from the cross-bank variation of the return gap (the reserve-tax gap is common to all banks → absorbed by month FE)
  out <- rbindlist(lapply(unique(W$prod), function(pp) { D <- W[prod == pp & is.finite(ret_gap) & is.finite(f12_dfxs)]; f <- fe2_bank(D, "f12_dfxs", "ret_gap")
    if (is.null(f)) return(NULL); data.table(level = "банки, FE банку і місяця", prod = pp, term = names(f$b), b = f$b, se = sqrt(diag(f$V)), n = f$n, banks = f$nb) }))
  # (b) system time series: both gaps, Newey–West (12)
  SYSW <- W[, .(fxs = 100 * sum(stock_fx) / sum(stock_uah + stock_fx), ret_gap = weighted.mean(ret_gap, stock_uah + stock_fx, na.rm = TRUE),
                tax_gap = weighted.mean(tax_gap, stock_uah + stock_fx, na.rm = TRUE)), by = .(prod, month)][order(prod, month)]
  SYSW[, f12_dfxs := calendar_shift(fxs, month, 12, type = "lead") - fxs, by = prod]
  ts <- rbindlist(lapply(unique(SYSW$prod), function(pp) { D <- SYSW[prod == pp & is.finite(f12_dfxs) & is.finite(ret_gap) & is.finite(tax_gap)]; if (nrow(D) < 30) return(NULL)
    f <- lm(f12_dfxs ~ ret_gap + tax_gap, D); V <- sandwich::NeweyWest(f, lag = 12, prewhite = FALSE)
    data.table(level = "система, Newey–West", prod = pp, term = c("ret_gap", "tax_gap"), b = coef(f)[2:3], se = sqrt(diag(V))[2:3], n = nrow(D), banks = NA_integer_) }))
  out <- rbind(out, ts, fill = TRUE)
  if (nrow(out)) { out[, p := 2 * pnorm(-abs(b / se))]; save_tab(out, "S24_R8_dollarization_response.csv", "R8_dollarization")
    a <- out[prod == "hh_term"]
    if (nrow(a)) fact("R8_hh_term", paste(sprintf("%s: %s %+.2f (с.п. %.2f)", a$level, a$term, a$b, a$se), collapse = "; ")) }
})

# ------------------------------ R7. plausibility --------------------------------
blk("R7 правдоподібність", {
  P <- PRODSFC
  req <- P[, .(required = sum(stock * rho), required_cash = sum(stock * rho_cash)), by = .(nkb, month)]
  corr <- B[, .(nkb, month, corr = z0(col_or_na(B, "nbu_corr_reserves_uah")) + z0(col_or_na(B, "nbu_corr_reserves_fx")))]
  X <- merge(req, corr, by = c("nkb", "month"))
  sx <- X[, .(required = sum(required), required_cash = sum(required_cash), corr = sum(corr)), by = month][order(month)]
  sx[, ratio := corr / required_cash]
  save_tab(sx, "S24_R7_required_vs_correspondent.csv", "R7_plausibility")
  fact("R7_ratio", sprintf("залишки на кор. рахунках / розрахункові грошові резерви: медіана %.2f за 2023–2026", median(sx[month >= as.Date("2023-03-01"), ratio], na.rm = TRUE)))
})

# ------------------------------ exports for the Excel ----------------------------
blk("Excel-входи", {
  latest <- max(PRODSFC$month)
  save_tab(PRODSFC[month == latest, .(nkb, prod, ccy, stock, r, rho, rho_cov, dgf)], "S24_bank_products_latest.csv", "X_products")
  save_tab(MAC[month == latest], "S24_macro_latest.csv", "X_macro")
  save_tab(data.table(param = names(PAR), value = unlist(PAR)), "S24_parameters.csv", "X_par")
  save_tab(SHADOW[month >= add_months(latest, -59L), .(nkb, month, cod_nom = 100 * cod_nom, ec = 100 * ec, sfc = 100 * sfc, wedge = 100 * wedge)], "S24_bank_history_60m.csv", "X_history")
})

save_tab(STATUS, "S24_00_blocks_status.csv", "Status"); save_tab(FACTS, "S24_00_key_facts.csv", "Key_facts")
write_workbook("Section_2_4.xlsx")
print(STATUS); print(FACTS)
log_msg("SECTION 2.4 COMPLETE: ", length(TABLES), " tables, ", sprintf("%.1f хв", as.numeric(difftime(Sys.time(), t0_all, units = "mins"))))
