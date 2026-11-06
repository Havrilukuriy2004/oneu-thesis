# ============================================================================
# 01_nbu_download_v6.R — INCREMENTAL download of NBU OpenData (network layer)
# ----------------------------------------------------------------------------
# What this script does
#   1. Re-uses everything already stored in data_nbu/parquet (no full re-pull).
#   2. For every dataset finds the last stored report date and downloads only
#      [last - REVISION_WINDOW_MONTHS, today] (NBU revises recent months).
#   3. OSB: downloads the extended account list of account_map_v6.R. Accounts
#      not yet stored are pulled for the full 2018+ sample.
#   4. Adds datasets missing in v5: official FX rates, UONIA (fixed),
#      OVDP primary auctions (fixed, with per-date fallback), and keeps MIR,
#      deposit, loan, banksfinrep, key, liquidity, inflation, klk, etc.
#   5. Writes a data-availability report: output_nbu/logs/data_availability_v6.csv
#
# Run in RStudio on the computer that has internet access to bank.gov.ua.
# Typical runtime: 15–40 min on the first v6 run (new OSB accounts), 2–5 min
# for later monthly updates.
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "account_map_v6.R"), encoding = "UTF-8")

source(file.path(PROJECT_DIR, "utils_nbu_api_v6.R"), encoding = "UTF-8")   # API helpers + gap filling

# ------------------------------ 1. CATALOG -----------------------------------
catalog <- as.data.table(http_json(NBU_BASE, list(json = ""), cache = FALSE) %||% data.frame())
if (nrow(catalog)) fwrite(catalog, file.path(OUTPUT_DIR, "nbu_api_catalog.csv"))

# ------------------------------ 2. STATDIRECTORY BLOCKS ----------------------
# STAT_BLOCKS (name, apikod, period, key columns) is defined in utils_nbu_api_v6.R
for (b in STAT_BLOCKS) update_stat_block(b[[1]], b[[2]], b[[3]], b[[4]])
# 2b. Gap filling: the incremental update re-pulls only the last months, so a hole in the
#     MIDDLE of a series (e.g. MIR 2023-03…2024-05 lost in an earlier run) is never repaired.
#     Every monthly block is scanned for missing report months and those windows are
#     re-downloaded without cache. Result: output_nbu/logs/db_gaps_v6.csv
fill_all_gaps(Filter(function(b) b[[3]] == "m", STAT_BLOCKS))

# ------------------------------ 3. OSB (account level) -----------------------
log_msg("OSB: ", length(OSB_ACCOUNTS_V6), " accounts in account_map_v6")
osb_old <- store_read("raw_osb")
if (nrow(osb_old)) osb_old[, r020 := sprintf("%04d", as.integer(r020))]
osb_last <- if (nrow(osb_old)) osb_old[, .(last = max(parse_nbu_date(dt))), by = r020] else data.table(r020 = character(), last = as.Date(character()))

osb_new <- vector("list", length(OSB_ACCOUNTS_V6))
for (i in seq_along(OSB_ACCOUNTS_V6)) {
  a <- OSB_ACCOUNTS_V6[i]
  l <- osb_last[r020 == a, last]
  start <- if (length(l) && is.finite(l)) max(START_REPORT_DATE, add_months(month_floor(l), -REVISION_WINDOW_MONTHS)) else START_REPORT_DATE
  if (i %% 20 == 1) log_msg(sprintf("OSB %d/%d account %s from %s", i, length(OSB_ACCOUNTS_V6), a, start))
  x <- tryCatch(nbu_stat("osb", start, END_REPORT_DATE, "m", list(id_api = "OSB_account", r020 = a)),
                error = function(e) data.table())
  if (nrow(x)) { x[, r020 := sprintf("%04d", as.integer(r020))]; x[, window_start := start] }
  osb_new[[i]] <- x
}
osb_new <- rbindlist(osb_new, fill = TRUE, use.names = TRUE)

if (nrow(osb_new)) {
  ws <- unique(osb_new[, .(r020, window_start)])
  keep_old <- if (nrow(osb_old)) {
    o <- merge(osb_old, ws, by = "r020", all.x = TRUE)
    o[is.na(window_start) | parse_nbu_date(dt) < window_start][, window_start := NULL]
  } else data.table()
  osb_new[, window_start := NULL]
  keep_cols <- intersect(c("dt","nkb","r020","r034","t025","value","txt","id_api","freq","tzep"), union(names(keep_old), names(osb_new)))
  raw_osb <- rbindlist(list(keep_old[, intersect(keep_cols, names(keep_old)), with = FALSE],
                            osb_new[, intersect(keep_cols, names(osb_new)), with = FALSE]),
                       use.names = TRUE, fill = TRUE)
  raw_osb[, `:=`(dt = as.character(dt), nkb = as.character(nkb), r034 = as.integer(r034),
                 t025 = as.integer(t025), value = num(value))]
  raw_osb <- unique(raw_osb, by = c("dt","nkb","r020","r034","t025"), fromLast = TRUE)
  store_write(raw_osb, "raw_osb")
  log_msg("OSB stored: ", nrow(raw_osb), " rows, ", uniqueN(raw_osb$r020), " accounts, last ", last_date_of(raw_osb))
}

# ------------------------------ 4. OFFICIAL FX RATES -------------------------
# https://bank.gov.ua/NBU_Exchange/exchange_site?start=YYYYMMDD&end=YYYYMMDD&valcode=usd&sort=exchangedate&order=asc&json
pull_fx <- function(valcode) {
  yrs <- seq(year(START_MONTH) - 1L, year(Sys.Date()))
  rbindlist(lapply(yrs, function(y) {
    s <- as.Date(sprintf("%d-01-01", y)); e <- min(as.Date(sprintf("%d-12-31", y)), Sys.Date())
    x <- http_json("https://bank.gov.ua/NBU_Exchange/exchange_site",
                   list(start = fmt_api(s), end = fmt_api(e), valcode = tolower(valcode),
                        sort = "exchangedate", order = "asc", json = ""),
                   cache = !is_hot(e))
    as.data.table(x %||% data.frame())
  }), fill = TRUE)
}
raw_fx <- rbindlist(lapply(c("USD", "EUR", "GBP", "PLN", "CHF"), pull_fx), fill = TRUE)
if (nrow(raw_fx)) {
  dcol <- intersect(c("exchangedate", "calcdate"), names(raw_fx))[1]
  raw_fx[, date := parse_nbu_date(get(dcol))]
  rcol <- intersect(c("rate_per_unit", "rate"), names(raw_fx))[1]
  raw_fx[, rate := num(get(rcol))]
  raw_fx <- unique(raw_fx[, .(date, cc = toupper(cc), rate)], by = c("date", "cc"))
  store_write(raw_fx, "raw_fx")
  log_msg("FX: ", nrow(raw_fx), " rows, ", min(raw_fx$date), " -> ", max(raw_fx$date))
}

# ------------------------------ 5. UONIA -------------------------------------
pull_uonia <- function() {
  yrs <- seq(2020L, year(Sys.Date()))
  variants <- list(list(id_api = "UONIA_UnsecLoansDepo"), list())
  for (v in variants) {
    x <- rbindlist(lapply(yrs, function(y) {
      s <- max(as.Date(sprintf("%d-01-01", y)), as.Date("2020-06-22")); e <- min(as.Date(sprintf("%d-12-31", y)), Sys.Date())
      as.data.table(http_json("https://bank.gov.ua/NBU_uonia",
                              c(v, list(start = fmt_api(s), end = fmt_api(e), sort = "dt", order = "asc", json = "")),
                              cache = !is_hot(e)) %||% data.frame())
    }), fill = TRUE)
    if (nrow(x)) return(x)
  }
  data.table()
}
raw_uonia <- pull_uonia()
# Local NBU xlsx "Money market: the dynamics of UONIA" (data_nbu/local_inputs): used when the API fails
# (HTTP 500 seen in 11.2026) and to fill days the API does not return.
source(file.path(PROJECT_DIR, "utils_local_inputs_v6.R"), encoding = "UTF-8")
ul <- tryCatch(read_uonia_local(), error = function(e) NULL)
if (!is.null(ul) && nrow(ul)) {
  if (nrow(raw_uonia)) {
    dcol <- intersect(c("date", "dt"), names(raw_uonia))[1]
    have <- parse_nbu_date(raw_uonia[[dcol]])
    raw_uonia <- rbind(raw_uonia[, .(date = as.character(get(dcol)), value, id_api = if ("id_api" %in% names(raw_uonia)) id_api else "UONIA_api")],
                       ul[!date %in% have, .(date = as.character(date), value, id_api)], fill = TRUE)
  } else raw_uonia <- ul[, .(date = as.character(date), value, id_api)]
}
if (nrow(raw_uonia)) {
  dcol <- intersect(c("date", "dt"), names(raw_uonia))[1]
  raw_uonia[, date := parse_nbu_date(get(dcol))]
  raw_uonia[, value := num(value)]
  raw_uonia <- raw_uonia[is.finite(value) & !is.na(date)]
  store_write(raw_uonia, "raw_uonia")
  log_msg("UONIA: ", nrow(raw_uonia), " rows, ", min(raw_uonia$date), " -> ", max(raw_uonia$date))
} else warning("UONIA not available from API or local xlsx.")

# ------------------------------ 6. OVDP PRIMARY AUCTIONS ---------------------
# Documented parameters: date (dd.mm.yyyy), val_code (UAH/USD/EUR), mode=AuctionDate, ord.
ovdp_one <- function(extra, cache = TRUE) {
  url <- paste0("https://bank.gov.ua/NBU_ovdp?json&", paste(names(extra), unlist(extra), sep = "=", collapse = "&"))
  as.data.table(http_json(url, cache = cache) %||% data.frame())
}
raw_ovdp_old <- store_read("raw_ovdp")
raw_ovdp <- data.table()
for (vc in c("UAH", "USD", "EUR")) {
  x <- ovdp_one(list(val_code = vc, mode = "AuctionDate", ord = "asc"), cache = FALSE)
  if (nrow(x)) raw_ovdp <- rbind(raw_ovdp, x, fill = TRUE)
}
OVDP_PER_DATE_FALLBACK <- FALSE   # querying every business day took >12 min and returned nothing in 11.2026;
                                  # OVDP primary yields now come from the NBU statistics (4-Financial_markets.xlsx, table 4.2.3)
if (!nrow(raw_ovdp) && OVDP_PER_DATE_FALLBACK) {
  # Fallback: query every business day (auctions are mostly on Tuesdays);
  # cold days are cached, so this is slow only on the first run.
  days <- seq(as.Date("2018-01-01"), Sys.Date(), by = "day")
  days <- days[!as.POSIXlt(days)$wday %in% c(0L, 6L)]
  done <- if (nrow(raw_ovdp_old) && "AuctionDate" %in% names(raw_ovdp_old)) unique(parse_nbu_date(raw_ovdp_old$AuctionDate)) else as.Date(character())
  days <- days[!(days %in% done) | days >= Sys.Date() - 60]
  log_msg("OVDP: per-date fallback over ", length(days), " business days")
  raw_ovdp <- rbindlist(lapply(days, function(d) ovdp_one(list(date = format(d, "%d.%m.%Y"), mode = "AuctionDate"),
                                                         cache = d < Sys.Date() - 30)), fill = TRUE)
  if (nrow(raw_ovdp_old)) raw_ovdp <- rbind(raw_ovdp_old, raw_ovdp, fill = TRUE)
}
if (nrow(raw_ovdp)) {
  for (nm in names(raw_ovdp)) if (is.list(raw_ovdp[[nm]])) raw_ovdp[, (nm) := vapply(get(nm), function(z) paste(z, collapse = ";"), "")]
  raw_ovdp <- unique(raw_ovdp)
  store_write(raw_ovdp, "raw_ovdp")
  log_msg("OVDP auctions: ", nrow(raw_ovdp), " rows")
} else warning("OVDP auctions unavailable — put Minfin 'OVDP_mis.xlsx' into data_nbu/local_inputs.")

# ------------------------------ 7. LOCAL CURVE FILES -------------------------
# NSS (UAH) and NS (FX) zero-coupon parameters are published by the NBU as xlsx
# (Ринки -> ОВДП -> Криві дохідності). Put the latest files into
# data_nbu/local_inputs; 02_build_marts_v6.R parses them.
for (f in c("NSS_parameters_LC.xlsx", "N_S_parameters_FC.xlsx", "OVDP_mis.xlsx")) {
  hit <- list.files(c(LOCAL_DIR, PROJECT_DIR, file.path(Sys.getenv("USERPROFILE"), "Downloads")),
                    pattern = paste0("^", gsub(".", "\\.", f, fixed = TRUE), "$"), full.names = TRUE)
  log_msg(sprintf("local input %-24s : %s", f, if (length(hit)) paste(hit[1], format(file.mtime(hit[1]), "(%Y-%m-%d)")) else "NOT FOUND"))
}

# ------------------------------ 8. AVAILABILITY REPORT -----------------------
avail <- rbindlist(lapply(c(vapply(STAT_BLOCKS, `[[`, "", 1), "raw_osb"), function(nm) {
  x <- store_read(nm)
  data.table(dataset = nm, rows = nrow(x),
             first_report_date = if (nrow(x)) min(parse_nbu_date(x$dt), na.rm = TRUE) else as.Date(NA),
             last_report_date  = if (nrow(x)) max(parse_nbu_date(x$dt), na.rm = TRUE) else as.Date(NA))
}))
avail <- rbind(avail, data.table(
  dataset = c("raw_fx", "raw_uonia", "raw_ovdp"),
  rows = c(nrow(raw_fx), nrow(raw_uonia), nrow(raw_ovdp)),
  first_report_date = c(if (nrow(raw_fx)) min(raw_fx$date) else NA, if (nrow(raw_uonia)) min(raw_uonia$date) else NA, as.Date(NA)),
  last_report_date  = c(if (nrow(raw_fx)) max(raw_fx$date) else NA, if (nrow(raw_uonia)) max(raw_uonia$date) else NA, as.Date(NA))),
  fill = TRUE)
avail[, months_behind_today := (year(Sys.Date()) - year(last_report_date)) * 12 + month(Sys.Date()) - month(last_report_date)]
fwrite(avail, file.path(LOG_DIR, "data_availability_v6.csv"))
print(avail)
log_msg("DOWNLOAD v6 COMPLETE. Next: source('02_build_marts_v6.R')")
