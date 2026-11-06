# ============================================================================
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
# utils_local_inputs_v6.R — readers for files the author puts into
#                           data_nbu/local_inputs/ (no NBU API equivalent)
# ----------------------------------------------------------------------------
#   read_uird_local()  NBU file "UIRD.xls(x)": one sheet per year ("2026 рік"),
#                      rows 4–5 = two-level header (currency × 3/6/9/12 months),
#                      daily values, % p.a.  -> long daily table + monthly table
#   read_579_local()   ФРП weekly report on "Доступні кредити 5-7-9%":
#                      cumulative signed contracts (count, mn UAH) and current
#                      outstanding debt (mn UAH)  -> month-end table + monthly flow
# Both return NULL (with a log line) if the file is absent, so callers can fall
# back to proxies.
# ============================================================================
.need(c("readxl"))

find_local <- function(pattern) {
  f <- list.files(LOCAL_DIR, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  f <- f[!grepl("^~\\$", basename(f))]                       # Excel lock files
  if (!length(f)) return(character())
  f[order(file.mtime(f), decreasing = TRUE)]                  # newest first
}

# ------------------------------ UIRD -----------------------------------------
read_uird_local <- function() {
  f <- find_local("uird.*\\.(xls|xlsx|csv)$")
  if (!length(f)) { log_msg("UIRD: local file not found"); return(NULL) }
  f <- f[1]
  if (grepl("\\.csv$", f, ignore.case = TRUE)) {               # simple format: date, value[, tenor][, ccy]
    u <- fread(f); setnames(u, tolower(names(u)))
    if (!"tenor" %in% names(u)) u[, tenor := "3m"]
    if (!"ccy" %in% names(u)) u[, ccy := "uah"]
    d <- u[, .(date = as.Date(parse_nbu_date(date)), ccy = tolower(ccy), tenor = paste0(gsub("\\D", "", tenor), "m"), value = num(value))]
  } else {
    sh <- readxl::excel_sheets(f)
    ccy_lab <- c("гривня" = "uah", "долар" = "usd", "євро" = "eur")
    d <- rbindlist(lapply(sh, function(s) {
      x <- suppressMessages(readxl::read_excel(f, sheet = s, col_names = FALSE, col_types = "list"))
      x <- as.data.table(x); if (nrow(x) < 6 || ncol(x) < 5) return(NULL)
      hdr_row <- which(vapply(seq_len(min(nrow(x), 15)), function(r) any(grepl("місяц", unlist(x[r]), ignore.case = TRUE)), logical(1)))[1]
      if (is.na(hdr_row)) return(NULL)
      top <- vapply(unlist(x[hdr_row - 1], recursive = FALSE), function(v) if (is.null(v) || all(is.na(v))) NA_character_ else as.character(v), "")
      ten <- vapply(unlist(x[hdr_row], recursive = FALSE), function(v) if (is.null(v) || all(is.na(v))) NA_character_ else as.character(v), "")
      top <- zoo_locf(top)                                     # merged currency cells
      cc <- vapply(top, function(t) { k <- names(ccy_lab)[vapply(names(ccy_lab), grepl, logical(1), x = tolower(t %||% ""))]; if (length(k)) ccy_lab[[k[1]]] else NA_character_ }, "")
      body <- x[(hdr_row + 1):.N]
      dt <- vapply(body[[1]], function(v) {
        if (is.null(v) || all(is.na(v))) return(NA_real_)
        if (inherits(v, "POSIXct") || inherits(v, "Date")) return(as.numeric(as.Date(v)))
        if (is.numeric(v)) return(as.numeric(as.Date(v, origin = "1899-12-30")))
        as.numeric(as.Date(parse_nbu_date(as.character(v))))
      }, 0)
      yr <- suppressWarnings(as.integer(sub("\\D*(\\d{4}).*", "\\1", s)))
      out <- rbindlist(lapply(which(!is.na(cc) & grepl("\\d", ten)), function(j) {
        data.table(date = as.Date(dt, origin = "1970-01-01"), ccy = cc[j], tenor = paste0(gsub("\\D", "", ten[j]), "m"),
                   value = vapply(body[[j]], function(v) if (is.null(v) || all(is.na(v))) NA_real_ else suppressWarnings(as.numeric(v)), 0))
      }))
      # guard against mistyped dates inside a year sheet (e.g. "2024-09-27" in the 2014 sheet)
      if (is.finite(yr)) out <- out[year(date) %in% c(yr, yr + 1L) & !(year(date) == yr + 1L & month(date) > 1L)]
      out[is.finite(value) & !is.na(date)]
    }))
  }
  d <- unique(d[is.finite(value) & !is.na(date)], by = c("date", "ccy", "tenor"))
  setorder(d, ccy, tenor, date)
  m <- d[, .(avg = mean(value) / 100, eom = value[.N] / 100, n_days = .N), by = .(month = month_floor(date), ccy, tenor)]
  log_msg("UIRD: ", basename(f), " — ", nrow(d), " daily obs, ", min(d$date), " … ", max(d$date))
  list(daily = d, monthly = m, file = basename(f))
}

# Wide monthly UIRD columns for the macro mart: uird_<ccy>_<tenor>_{avg,eom}
uird_wide <- function(u) {
  if (is.null(u)) return(NULL)
  w <- dcast(u$monthly, month ~ paste0("uird_", ccy, "_", tenor), value.var = c("avg", "eom"))
  setnames(w, names(w), sub("^(avg|eom)_(uird_.*)$", "\\2_\\1", names(w)))
  w
}

zoo_locf <- function(x) { for (i in seq_along(x)[-1]) if (is.na(x[i]) || x[i] == "") x[i] <- x[i - 1]; x }

# ------------------------------ 5-7-9 ----------------------------------------
# Columns are matched by keywords (the ФРП report keeps them stable):
#   "Дата інформації"                              -> date (as-of date; month = calendar month of the date)
#   "Підписані кредитні договори, шт"              -> contracts_n (cumulative)
#   "Сума кредитних договорів … млн грн"           -> contracts_mn (cumulative signed amount)
#   "Поточна заборгованість … млн грн"             -> debt_mn (outstanding)
read_579_local <- function() {
  f <- find_local("(5-?7-?9|579).*\\.(xls|xlsx|csv)$")
  if (!length(f)) { log_msg("5-7-9: local file not found"); return(NULL) }
  f <- f[1]
  x <- if (grepl("\\.csv$", f, ignore.case = TRUE)) fread(f) else as.data.table(suppressMessages(readxl::read_excel(f, sheet = 1)))
  nm <- tolower(names(x))
  pick <- function(...) { k <- which(Reduce(`&`, lapply(c(...), function(p) grepl(p, nm)))); if (length(k)) k[1] else NA_integer_ }
  j_date <- pick("дата"); j_n <- pick("підписані"); j_sum <- pick("сума", "договор"); j_debt <- pick("заборгован")
  if (anyNA(c(j_date, j_sum))) { log_msg("5-7-9: columns not recognised in ", basename(f)); return(NULL) }
  w <- data.table(date = as.Date(x[[j_date]]), contracts_n = if (!is.na(j_n)) num(x[[j_n]]) else NA_real_,
                  contracts_mn = num(x[[j_sum]]), debt_mn = if (!is.na(j_debt)) num(x[[j_debt]]) else NA_real_)
  w <- unique(w[!is.na(date) & is.finite(contracts_mn)], by = "date"); setorder(w, date)
  # The cumulative signed amount must not fall; isolated falls are reporting corrections -> treated as missing
  w[, bad_cum := contracts_mn < cummax(contracts_mn) - 1e-6]
  ww <- w[bad_cum == FALSE]
  # Month-end values by linear interpolation between weekly as-of dates (no extrapolation)
  me <- seq(month_floor(min(ww$date)), month_floor(max(ww$date)), by = "month")
  eom <- add_months(me, 1L) - 1L
  ip <- function(y, d) { ok <- is.finite(y); if (sum(ok) < 2) return(rep(NA_real_, length(eom)))
    v <- approx(as.numeric(d[ok]), y[ok], xout = as.numeric(eom), rule = 1)$y; v }
  m <- data.table(month = me, contracts_mn_cum = ip(ww$contracts_mn, ww$date), contracts_n_cum = ip(ww$contracts_n, ww$date),
                  debt_mn = ip(w$debt_mn, w$date))
  # last partial month: use the latest weekly value (flagged)
  last <- w[.N]; m[month == month_floor(last$date) & is.na(contracts_mn_cum), `:=`(contracts_mn_cum = last$contracts_mn, debt_mn = last$debt_mn, contracts_n_cum = last$contracts_n)]
  m[, partial := month == month_floor(last$date) & last$date < add_months(month, 1L) - 1L]
  setorder(m, month)
  m[, `:=`(new_contracts_mn = contracts_mn_cum - shift(contracts_mn_cum), new_contracts_n = contracts_n_cum - shift(contracts_n_cum))]
  m[new_contracts_mn < 0, new_contracts_mn := NA_real_]
  log_msg("5-7-9: ", basename(f), " — ", nrow(w), " weekly obs (", sum(w$bad_cum), " corrections dropped), ", min(w$date), " … ", max(w$date))
  list(weekly = w, monthly = m, file = basename(f))
}

# ============================================================================
# EXPECTATIONS, FULL RATE STATISTICS, BANK LENDING SURVEY, MONETARY AGGREGATES
# (added 04.11.2026). Files in data_nbu/local_inputs/ (NBU website, as published):
#   *Financial_markets*.xlsx  NBU monetary & financial statistics, section 4:
#                             interest rates on NEW and OUTSTANDING loans/deposits by sector,
#                             currency and maturity (4.1.1–4.1.5), OVDP primary yields (4.2.3)
#   Surveys_price*.xlsx       inflation expectations 12 m ahead: banks, firms, households, analysts
#   Surveys_rate*.xlsx        UAH/USD exchange-rate expectations 12 m ahead (same four groups)
#   BLS*.xlsx                 Bank Lending Survey (quarterly balances of answers)
# API tables (01): raw_monetary (M0–M3, deposits in broad money by currency/sector),
#                  raw_survey (monthly business survey, IODA), raw_q_survey (quarterly BOS).
# ============================================================================
UA_MONTHS <- c("січень", "лютий", "березень", "квітень", "травень", "червень", "липень", "серпень", "вересень", "жовтень", "листопад", "грудень")
.cell <- function(v) { v <- trimws(gsub("[\r\n]+", " ", as.character(v))); v[is.na(v) | v %in% c("", "NA")] <- NA_character_; v }
.numx <- function(v) { v <- gsub("\\s", "", .cell(v)); v[v %in% c("…", "...", "–", "-", "—", "#N/A", "x", "х")] <- NA; suppressWarnings(as.numeric(gsub(",", ".", v))) }

# -------- 4-Financial_markets.xlsx: generic parser for one statistical table ------
# header block = rows between "Період" and the row of column numbers (1, 2, 3, …);
# merged header cells are filled to the right only inside the parent's span.
parse_fm_sheet <- function(f, sheet) {
  x <- suppressMessages(readxl::read_excel(f, sheet = sheet, col_names = FALSE, col_types = "text"))
  x <- as.data.frame(lapply(x, .cell), stringsAsFactors = FALSE)
  c1 <- x[[1]]
  top <- which(grepl("^Період", c1))[1]; if (is.na(top)) return(NULL)
  num <- which(seq_len(nrow(x)) > top & c1 %in% "1" & x[[2]] %in% "2")[1]; if (is.na(num)) return(NULL)
  codes <- unlist(x[num, ]); nc <- max(which(!is.na(codes)))
  H <- as.matrix(x[top:(num - 1), seq_len(nc), drop = FALSE]); Fm <- H
  for (k in seq_len(nrow(H))) for (j in seq_len(nc)[-1]) if (is.na(Fm[k, j]) && !is.na(Fm[k, j - 1]) &&
       (k == 1 || identical(Fm[k - 1, j], Fm[k - 1, j - 1]))) Fm[k, j] <- Fm[k, j - 1]
  lab <- vapply(seq_len(nc), function(j) { p <- Fm[, j]; p <- p[!is.na(p)]; p <- p[c(TRUE, p[-1] != p[-length(p)])]; paste(p, collapse = " | ") }, "")
  body <- x[(num + 1):nrow(x), seq_len(nc), drop = FALSE]
  yr <- NA_integer_; out <- vector("list", nrow(body))
  for (i in seq_len(nrow(body))) {
    p <- tolower(body[i, 1] %||% NA); if (is.na(p)) next
    if (grepl("^\\d{4}", p)) { yr <- as.integer(substr(p, 1, 4)); next }      # annual row = average of the year (skipped)
    m <- match(sub("\\s.*$", "", p), UA_MONTHS); if (is.na(m) || is.na(yr)) next
    v <- .numx(unlist(body[i, -1]))
    out[[i]] <- data.table(month = as.Date(sprintf("%d-%02d-01", yr, m)), code = codes[2:nc], label = lab[2:nc], value = v)
  }
  r <- rbindlist(out); if (!nrow(r)) return(NULL)
  r[, sheet := sheet][is.finite(value)]
}

read_fm_local <- function() {
  f <- find_local("financial_markets.*\\.xlsx$")
  if (!length(f)) { log_msg("Financial_markets: local file not found"); return(NULL) }
  sh <- readxl::excel_sheets(f[1]); sh <- sh[grepl("^4\\.(1|2\\.3)", sh)]
  r <- rbindlist(lapply(sh, function(s) tryCatch(parse_fm_sheet(f[1], s), error = function(e) { log_msg("FM sheet ", s, ": ", conditionMessage(e)); NULL })), fill = TRUE)
  log_msg(sprintf("Financial_markets: %d sheets, %d values, %s … %s", uniqueN(r$sheet), nrow(r), min(r$month), max(r$month)))
  r
}

# selected series (sheet, column code) -> variable; rates in %, volumes in mn UAH
FM_MAP <- rbindlist(list(
  # new deposits, households: UAH total / demand / <1y / 1–2y / >2y; FX total / <1y
  list("fm_dep_hh_uah", "4.1.3.6", "7"), list("fm_dep_hh_uah_dem", "4.1.3.6", "8"), list("fm_dep_hh_uah_1y", "4.1.3.6", "9"),
  list("fm_dep_hh_uah_2y", "4.1.3.6", "10"), list("fm_dep_hh_uah_g2y", "4.1.3.6", "11"), list("fm_dep_hh_fx", "4.1.3.6", "12"), list("fm_dep_hh_fx_1y", "4.1.3.6", "14"),
  # new deposits, NFC
  list("fm_dep_nfc_uah", "4.1.3.3", "7"), list("fm_dep_nfc_uah_dem", "4.1.3.3", "8"), list("fm_dep_nfc_uah_1y", "4.1.3.3", "9"), list("fm_dep_nfc_fx", "4.1.3.3", "12"),
  # new loans: NFC UAH total / excl. overdraft / overdraft / <1y / 1–5y; FX; HH UAH
  list("fm_loan_nfc_uah", "4.1.1.3", "8"), list("fm_loan_nfc_uah_xo", "4.1.1.3", "10"), list("fm_loan_nfc_uah_od", "4.1.1.3", "9"),
  list("fm_loan_nfc_uah_1y", "4.1.1.3", "11"), list("fm_loan_nfc_uah_5y", "4.1.1.3", "12"), list("fm_loan_nfc_fx", "4.1.1.3", "14"),
  list("fm_loan_hh_uah", "4.1.1.8", "8"), list("fm_loan_hh_uah_xo", "4.1.1.8", "10"),
  # outstanding (book) rates
  list("fm_out_dep_hh_uah", "4.1.4.2", "7"), list("fm_out_dep_hh_uah_dem", "4.1.4.2", "8"), list("fm_out_dep_hh_uah_1y", "4.1.4.2", "9"),
  list("fm_out_dep_hh_fx", "4.1.4.2", "12"), list("fm_out_dep_nfc", "4.1.4.1", "2"), list("fm_out_dep_nfc_dem", "4.1.4.1", "3"),
  list("fm_out_loan_nfc", "4.1.2.1", "2"), list("fm_out_loan_hh_uah", "4.1.2.2", "6"),
  # sector totals with volumes (all currencies) and currency split of the rate
  list("fm_vol_dep_hh", "4.1.3.1", "12"), list("fm_dep_hh_all", "4.1.3.1", "13"), list("fm_vol_dep_nfc", "4.1.3.1", "10"),
  list("fm_dep_all_uah", "4.1.3.1", "4"), list("fm_dep_all_fx", "4.1.3.1", "5"),
  list("fm_vol_loan_nfc", "4.1.1.1", "14"), list("fm_vol_loan_hh", "4.1.1.1", "16"), list("fm_loan_all_uah", "4.1.1.1", "4"),
  # interbank (UAH, all maturities; overnight UAH) and OVDP primary market (UAH, all; <1y)
  list("fm_ib_uah", "4.1.5.1", "4"), list("fm_ib_on_uah", "4.1.5.1", "9.1"),
  list("fm_ovdp_prim", "4.2.3", "3"), list("fm_ovdp_prim_1y", "4.2.3", "7")))
setnames(FM_MAP, c("var", "sheet", "code"))

fm_wide <- function(r) {
  if (is.null(r) || !nrow(r)) return(NULL)
  z <- merge(r, FM_MAP, by = c("sheet", "code"))
  z[, value := fifelse(grepl("^fm_vol_", var), value, value / 100)]          # rates -> shares (as all other macro rates)
  z <- z[!(value == 0 & !grepl("^fm_vol_", var))]                            # NBU prints 0 for "no transactions"
  dcast(z, month ~ var, value.var = "value", fun.aggregate = function(v) v[1])
}

# -------- Surveys_price / Surveys_rate: four groups × monthly dates ---------------
read_survey_expect <- function(pattern, prefix) {
  f <- find_local(pattern); if (!length(f)) { log_msg(prefix, ": local file not found"); return(NULL) }
  sh <- readxl::excel_sheets(f[1]); s <- if ("ENG" %in% sh) "ENG" else sh[1]
  x <- suppressMessages(readxl::read_excel(f[1], sheet = s, col_names = FALSE, col_types = "list"))
  hdr <- which(vapply(seq_len(min(nrow(x), 10)), function(r) any(grepl("^(banks|банків)", tolower(trimws(unlist(lapply(x[r, ], as.character)))))), logical(1)))[1]
  if (is.na(hdr)) return(NULL)
  grp <- tolower(trimws(vapply(x[hdr, ], function(v) as.character(v[[1]] %||% NA), "")))
  key <- c(banks = "banks", "банків" = "banks", businesses = "firms", "підприємств" = "firms", households = "hh", "домогосподарств" = "hh",
           "financial analysts" = "analysts", "фінансових аналітиків" = "analysts")
  dts <- vapply(x[[1]], function(v) { v <- v[[1]]; if (inherits(v, "POSIXt") || inherits(v, "Date")) format(as.Date(v)) else NA_character_ }, "")
  rows <- which(!is.na(dts) & seq_along(dts) > hdr)
  out <- rbindlist(lapply(which(grp %in% names(key)), function(j) data.table(month = month_floor(as.Date(dts[rows])), group = key[[grp[j]]],
                    value = .numx(vapply(x[[j]][rows], function(v) as.character(v[[1]] %||% NA), "")))))
  out <- out[is.finite(value)]
  w <- dcast(out, month ~ group, value.var = "value", fun.aggregate = function(v) v[1])
  setnames(w, setdiff(names(w), "month"), paste0(prefix, "_", setdiff(names(w), "month")))
  log_msg(sprintf("%s: %d months, %s … %s", prefix, nrow(w), min(w$month), max(w$month))); w
}

# -------- BLS: blocks headed by "Показник" (years) + quarter row; questions "N. …" --
read_bls_local <- function() {
  f <- find_local("^bls.*\\.xlsx$|bank.?lending.*\\.xlsx$"); if (!length(f)) { log_msg("BLS: local file not found"); return(NULL) }
  sh <- setdiff(readxl::excel_sheets(f[1]), c("Зміст", "Contents"))
  qn <- c(I = 1, II = 2, III = 3, IV = 4)
  rbindlist(lapply(sh, function(s) {
    x <- suppressMessages(readxl::read_excel(f[1], sheet = s, col_names = FALSE, col_types = "text"))
    x <- as.data.frame(lapply(x, .cell), stringsAsFactors = FALSE)
    lc <- which(vapply(x, function(v) any(v %in% c("Показник", "Indicator")), logical(1)))[1]; if (is.na(lc)) return(NULL)
    x <- x[, lc:ncol(x), drop = FALSE]; c1 <- x[[1]]                          # label column (first may be a "<<<<<" link column)
    hr <- which(c1 %in% c("Показник", "Indicator")); if (!length(hr)) return(NULL)
    res <- list(); q <- NA_character_; grp <- NA_character_
    for (b in seq_along(hr)) {
      r0 <- hr[b]; r1 <- if (b < length(hr)) hr[b + 1] - 1 else nrow(x)
      yrs <- .numx(unlist(x[r0, -1])); for (j in seq_along(yrs)[-1]) if (is.na(yrs[j])) yrs[j] <- yrs[j - 1]
      qq <- qn[unlist(x[r0 + 1, -1])]; dates <- as.Date(ifelse(is.finite(yrs) & !is.na(qq), sprintf("%d-%02d-01", as.integer(yrs), 3L * as.integer(qq) - 2L), NA))
      for (i in (r0 + 2):r1) {
        lab <- c1[i]; if (is.na(lab)) next
        v <- .numx(unlist(x[i, -1]))
        if (grepl("^\\d+\\.", lab)) { q <- sub("^(\\d+)\\..*$", "\\1", lab); grp <- NA_character_; next }
        if (!any(is.finite(v))) { grp <- lab; next }
        ok <- is.finite(v) & !is.na(dates)
        if (any(ok)) res[[length(res) + 1]] <- data.table(sheet = s, q = q, group = grp, label = lab, quarter = dates[ok], value = v[ok])
      }
    }
    rbindlist(res)
  }), fill = TRUE)
}

# BLS series used in the models (balances, % of weighted answers; + = tighter standards / higher demand / growth)
BLS_MAP <- rbindlist(list(
  list("bls_exp_dep",      "1",  NA,          "^Депозити"),               # banks' expected deposit growth, next 12 m
  list("bls_exp_loans",    "1",  NA,          "^Кредитний портфель"),
  list("bls_std_nfc",      "5",  NA,          "^У цілому"),               # NFC credit standards, current quarter (+ = tightening)
  list("bls_std_nfc_exp",  "7",  NA,          "^У цілому"),
  list("bls_dem_nfc",      "10", NA,          "^У цілому"),               # NFC loan demand (+ = higher)
  list("bls_dem_nfc_exp",  "12", NA,          "^У цілому"),
  list("bls_std_hh",       "14", NA,          "^У цілому|^Іпотечні|^Споживчі"),
  list("bls_dem_hh",       "19", NA,          "^У цілому|^Іпотечні|^Споживчі"),
  list("bls_dem_hh_exp",   "21", NA,          "^У цілому|^Іпотечні|^Споживчі"),
  list("bls_std_nfc_inflexp", "6", "^У цілому", "^Інфляційні очікування"),   # contribution of inflation expectations to NFC standards
  list("bls_std_nfc_fxexp",   "6", "^У цілому", "^Курсові очікування")))
setnames(BLS_MAP, c("var", "q", "group", "label"))

bls_wide <- function(b) {
  if (is.null(b) || !nrow(b)) return(NULL)
  sel <- rbindlist(lapply(seq_len(nrow(BLS_MAP)), function(k) { m <- BLS_MAP[k]
    z <- b[q == m$q & grepl(m$label, label)]
    if (!is.na(m$group)) z <- z[!is.na(group) & grepl(m$group, group)]
    if (!nrow(z)) return(NULL)
    first_lab <- z$label[1]; z <- z[label == first_lab]                       # first matching row (e.g. "У цілому")
    if (m$q == "1") z <- z[sheet == sheet[1]]
    z[, .(quarter, value = value[1]), by = quarter][, .(quarter, value, var = m$var)] }))
  if (!nrow(sel)) return(NULL)
  w <- dcast(sel, quarter ~ var, value.var = "value", fun.aggregate = function(v) v[1])
  # quarterly survey -> the three months of the quarter
  out <- w[, .(month = seq(quarter, by = "month", length.out = 3)), by = quarter]
  merge(out, w, by = "quarter")[, quarter := NULL][]
}

# -------- monetary aggregates (NBU API "monetary", t070_7 = stock, mn UAH) --------------
#   M1 − M0 = TransDepNC (transferable deposits in UAH);  M2 − M1 = TransDepFC + OtherDep (NC + FC);
#   M3 − M2 = SecOthShar. (The text labels of TransDepNC/TransDepFC in the API are swapped;
#   identifiers follow the identities above, checked on the data.) k076: 7 = NFC, 8 = households.
monetary_wide <- function(m) {
  if (is.null(m) || !nrow(m)) return(NULL)
  if ("tzep" %in% names(m)) m <- m[is.na(tzep) | tzep %in% c("t070_7", "T070_7", "")]
  m <- m[, .(month = add_months(month_floor(parse_nbu_date(dt)), -1L), id = id_api, k = as.character(k076), value = num(value))][is.finite(value)]
  g <- function(i0, k0 = "Total") { z <- m[m$id == i0 & m$k == k0, .(month, value)]; z[, .(value = value[1]), by = month] }
  w <- Reduce(function(a, b) merge(a, b, by = "month", all = TRUE), list(
    setnames(g("M0"), "value", "mon_m0"), setnames(g("M1"), "value", "mon_m1"), setnames(g("M2"), "value", "mon_m2"), setnames(g("M3"), "value", "mon_m3"),
    setnames(g("TransDepNC"), "value", "mon_dep_trans_uah"), setnames(g("TransDepFC"), "value", "mon_dep_trans_fx"),
    setnames(g("OtherDepNC"), "value", "mon_dep_oth_uah"), setnames(g("OtherDepFC"), "value", "mon_dep_oth_fx"),
    setnames(g("TransDepNC", "8"), "value", "mon_hh_trans_uah"), setnames(g("OtherDepNC", "8"), "value", "mon_hh_oth_uah"),
    setnames(g("TransDepFC", "8"), "value", "mon_hh_trans_fx"), setnames(g("OtherDepFC", "8"), "value", "mon_hh_oth_fx"),
    setnames(g("TransDepNC", "7"), "value", "mon_nfc_trans_uah"), setnames(g("OtherDepNC", "7"), "value", "mon_nfc_oth_uah"),
    setnames(g("TransDepFC", "7"), "value", "mon_nfc_trans_fx"), setnames(g("OtherDepFC", "7"), "value", "mon_nfc_oth_fx")))
  # identity check: M1 − M0 should equal UAH transferable deposits; if the NBU ever fixes the swapped ids, swap back
  chk <- w[is.finite(mon_m1) & is.finite(mon_dep_trans_uah), median(abs(mon_m1 - mon_m0 - mon_dep_trans_uah) / mon_m1)]
  chk2 <- w[is.finite(mon_m1) & is.finite(mon_dep_trans_fx), median(abs(mon_m1 - mon_m0 - mon_dep_trans_fx) / mon_m1)]
  if (is.finite(chk) && is.finite(chk2) && chk2 < chk) {
    for (s in c("dep_trans", "hh_trans", "nfc_trans")) { a <- paste0("mon_", s, "_uah"); b <- paste0("mon_", s, "_fx"); tmp <- w[[a]]; w[, (a) := get(b)]; w[, (b) := tmp] }
    log_msg("monetary: TransDepNC/FC identifiers swapped back by the M1 − M0 identity")
  }
  w[, `:=`(mon_dep_uah = mon_dep_trans_uah + mon_dep_oth_uah, mon_dep_fx = mon_dep_trans_fx + mon_dep_oth_fx)]
  w[, `:=`(mon_dollar = mon_dep_fx / (mon_dep_uah + mon_dep_fx), mon_mult = mon_m3 / mon_m0, mon_cash_share = mon_m0 / mon_m3,
           mon_hh_dep_uah = mon_hh_trans_uah + mon_hh_oth_uah, mon_hh_dep_fx = mon_hh_trans_fx + mon_hh_oth_fx,
           mon_hh_term_share_uah = mon_hh_oth_uah / (mon_hh_trans_uah + mon_hh_oth_uah))]
  w[, mon_hh_dollar := mon_hh_dep_fx / (mon_hh_dep_uah + mon_hh_dep_fx)]
  w[]
}

# fallback for monetary data: the author's own workbook "*Monetary_FX*.xlsx" (sheet monetary_raw = API dump)
read_monetary_fallback <- function() {
  f <- find_local("monetary.*\\.xlsx$"); if (!length(f)) return(NULL)
  sh <- readxl::excel_sheets(f[1]); if (!"monetary_raw" %in% sh) return(NULL)
  as.data.table(suppressMessages(readxl::read_excel(f[1], sheet = "monetary_raw", col_types = "text")))
}

# -------- business surveys (API): IODA monthly, BOS quarterly --------------------
survey_wide <- function(s, q) {
  out <- NULL
  if (!is.null(s) && nrow(s) && any(s$id_api == "m_survey_IODA"))
    out <- s[id_api == "m_survey_IODA", .(ioda = num(value)[1]), by = .(month = month_floor(parse_nbu_date(dt)))]
  if (!is.null(q) && nrow(q)) {
    kk <- if ("kod_esrv" %in% names(q)) q[kod_esrv %in% c("TOTAL", "Total")] else q
    if ("odku" %in% names(kk)) kk <- kk[is.na(odku) | odku %in% c("Усього", "Total", "")]
    map <- c(q_survey_BOI_total = "bos_boi", q_survey_change_rate_total = "bos_fx_exp", q_survey_sell_prices_total = "bos_sell_prices",
             q_survey_purch_prices_total = "bos_purch_prices", q_survey_lending_cond_total = "bos_lending_cond", q_survey_borrow_needs_total = "bos_borrow_needs")
    z <- kk[id_api %in% names(map), .(value = num(value)[1]), by = .(quarter = month_floor(parse_nbu_date(dt)), v = map[id_api])]
    if (nrow(z)) {
      zw <- dcast(z, quarter ~ v, value.var = "value")
      zw <- merge(zw[, .(month = seq(quarter, by = "month", length.out = 3)), by = quarter], zw, by = "quarter")[, quarter := NULL]
      out <- if (is.null(out)) zw else merge(out, zw, by = "month", all = TRUE)
    }
  }
  out
}
read_survey_fallback <- function(sheet) {
  f <- find_local("surveys?_expectations.*\\.xlsx$"); if (!length(f)) return(NULL)
  if (!sheet %in% readxl::excel_sheets(f[1])) return(NULL)
  as.data.table(suppressMessages(readxl::read_excel(f[1], sheet = sheet, col_types = "text")))
}

# -------- UONIA (NBU xlsx "Money market: the dynamics of UONIA") -----------------------
# columns: Date (dd.mm.yyyy) | Pre-UONIA, % p.a. (2019–2020) | UONIA, % p.a. | note.
# Returns daily data.table(date, value, id_api): UONIA where published, Pre-UONIA before it.
read_uonia_local <- function() {
  f <- find_local("uonia.*\\.xlsx$"); if (!length(f)) return(NULL)
  x <- suppressMessages(readxl::read_excel(f[1], sheet = 1, col_types = "text")); x <- as.data.table(x)
  nm <- tolower(names(x)); dcol <- which(grepl("^date|дата", nm))[1]
  pre <- which(grepl("pre", nm))[1]; uon <- setdiff(which(grepl("uonia|overnight", nm)), pre)[1]
  if (is.na(dcol) || is.na(uon)) return(NULL)
  d <- x[[dcol]]; dt <- suppressWarnings(as.Date(d, format = "%d.%m.%Y"))
  bad <- is.na(dt); if (any(bad)) dt[bad] <- suppressWarnings(as.Date(as.numeric(d[bad]), origin = "1899-12-30"))
  u <- .numx(x[[uon]]); p0 <- if (!is.na(pre)) .numx(x[[pre]]) else rep(NA_real_, nrow(x))
  out <- data.table(date = dt, value = fifelse(is.finite(u), u, p0), id_api = fifelse(is.finite(u), "UONIA_local_xlsx", "PreUONIA_local_xlsx"))
  out <- out[!is.na(date) & is.finite(value)]
  log_msg(sprintf("UONIA (local %s): %d days, %s … %s (Pre-UONIA до %s)", basename(f[1]), nrow(out), min(out$date), max(out$date),
                  if (any(out$id_api == "PreUONIA_local_xlsx")) max(out[id_api == "PreUONIA_local_xlsx", date]) else "—"))
  out
}
