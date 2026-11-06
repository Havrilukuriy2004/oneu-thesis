# ============================================================================
# utils_nbu_api_v6.R — NBU OpenData API helpers (used by 01 and 01c)
#   http_json / nbu_stat / upsert / update_stat_block   — incremental download
#   find_gaps / fill_gaps_block / fill_all_gaps          — repair missing months
# Needs 00_config_v6.R (paths, store_read/write, parse_nbu_date, add_months, log_msg).
# ============================================================================
.need(c("httr2"))
library(httr2)

NBU_BASE   <- "https://bank.gov.ua/NBUStatService/v1/statdirectory"
PAGE_LIMIT <- 50000L
SLEEP_SEC  <- 0.15
HOT_MONTHS <- 4L   # responses whose window ends within the last 4 months are never cached

fmt_api <- function(d) format(as.Date(d), "%Y%m%d")

# ------------------------------ HTTP -----------------------------------------
http_json <- function(url, query = list(), cache = TRUE) {
  key <- digest::digest(list(url, query), algo = "xxhash64")
  cp  <- file.path(RAW_DIR, paste0("v6_", key, ".rds"))
  if (cache && file.exists(cp)) return(readRDS(cp))
  req <- request(url) |>
    req_user_agent("ONEU-master-thesis/6.0 (research)") |>
    req_timeout(180) |>
    req_retry(max_tries = 5, backoff = ~ min(30, 2^.x),
              is_transient = function(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504))
  if (length(query)) req <- req_url_query(req, !!!query)
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) { warning("HTTP failed: ", url, " | ", conditionMessage(resp)); return(NULL) }
  txt <- resp_body_string(resp, encoding = "UTF-8")
  if (!nzchar(trimws(txt))) obj <- data.frame() else
    obj <- tryCatch(jsonlite::fromJSON(txt, flatten = TRUE), error = function(e) NULL)
  if (is.null(obj)) { warning("Non-JSON response: ", url); return(NULL) }
  if (cache) saveRDS(obj, cp)
  Sys.sleep(SLEEP_SEC)
  obj
}

is_hot <- function(end) as.Date(end) >= add_months(month_floor(Sys.Date()), -HOT_MONTHS)

# Generic paginated statdirectory pull.
nbu_stat <- function(apikod, start, end, period = NULL, filters = list(), cache = NULL) {
  if (is.null(cache)) cache <- !is_hot(end)
  out <- list(); offset <- 0L
  repeat {
    q <- c(list(start = fmt_api(start), end = fmt_api(end), limit = PAGE_LIMIT,
                offset = offset, json = ""),
           if (!is.null(period)) list(period = period), filters)
    dat <- http_json(file.path(NBU_BASE, apikod), q, cache = cache)
    if (is.null(dat)) break
    dat <- as.data.table(dat)
    if (!nrow(dat)) break
    out[[length(out) + 1L]] <- dat
    if (nrow(dat) < PAGE_LIMIT) break
    offset <- offset + nrow(dat)
  }
  x <- rbindlist(out, fill = TRUE, use.names = TRUE)
  if (nrow(x) && "dt" %in% names(x)) x[, dt := as.character(dt)]
  x
}

last_date_of <- function(x, col = "dt") {
  if (!nrow(x) || !col %in% names(x)) return(as.Date(NA))
  suppressWarnings(max(parse_nbu_date(x[[col]]), na.rm = TRUE))
}

# Upsert: keep old rows before window_start, append freshly downloaded rows.
upsert <- function(old, new, window_start, key_cols, date_col = "dt") {
  if (!nrow(new)) return(old)
  if (!nrow(old)) return(unique(new, by = intersect(key_cols, names(new))))
  old <- old[parse_nbu_date(get(date_col)) < window_start]
  common <- union(names(old), names(new))
  for (nm in setdiff(common, names(old))) old[, (nm) := NA]
  for (nm in setdiff(common, names(new))) new[, (nm) := NA]
  for (nm in common) if (!identical(class(old[[nm]]), class(new[[nm]]))) {
    old[, (nm) := as.character(get(nm))]; new[, (nm) := as.character(get(nm))]
  }
  x <- rbindlist(list(old, new), use.names = TRUE, fill = TRUE)
  unique(x, by = intersect(key_cols, names(x)), fromLast = TRUE)
}

update_stat_block <- function(name, apikod, period, key_cols, filters = list()) {
  old <- store_read(name)
  last <- last_date_of(old)
  start <- if (is.na(last)) START_REPORT_DATE else max(START_REPORT_DATE, add_months(month_floor(last), -REVISION_WINDOW_MONTHS))
  log_msg(sprintf("%-16s stored to %s | pulling %s -> %s", name, last, start, END_REPORT_DATE))
  new <- tryCatch(nbu_stat(apikod, start, END_REPORT_DATE, period, filters),
                  error = function(e) { warning(name, ": ", conditionMessage(e)); data.table() })
  if (!nrow(new) && !is.null(period) && !nrow(old))          # some sets (e.g. q_survey) reject the period parameter
    new <- tryCatch(nbu_stat(apikod, start, END_REPORT_DATE, NULL, filters), error = function(e) data.table())
  x <- upsert(old, new, start, key_cols)
  if (nrow(x)) store_write(x, name)
  log_msg(sprintf("%-16s +%s new rows | now %s rows | last %s", name, nrow(new), nrow(x), last_date_of(x)))
  invisible(x)
}


# ------------------------------ DATASETS -------------------------------------
STAT_BLOCKS <- list(
  # name,               apikod,       period, key columns (besides value)
  list("raw_banksfinrep", "banksfinrep", "m", c("dt","id_api","nkb","r034")),
  list("raw_banksincexp", "banksincexp", "m", c("dt","id_api")),
  list("raw_basindbank",  "basindbank",  "m", c("dt","id_api")),
  list("raw_mir",         "mir",         "m", c("dt","id_api","odkodter","odr030","ods180","odk111","odf074","k140","repository","tzep")),
  list("raw_deposit",     "deposit",     "m", c("dt","id_api","odkodter","odr030","ods183dd","odk111","odk051","odf074","k140","tzep")),
  list("raw_loan",        "loan",        "m", c("dt","id_api","odkodter","odr030","ods183ld","odk111","odk051","odf074","k140","tzep")),
  list("raw_securities",  "securities",  "m", c("dt","id_api","odr030","ods183sd","odk070","tzep")),
  list("raw_key",         "key",         "d", c("dt","id_api")),
  list("raw_liquidity",   "liquidity",   "d", c("dt","id_api")),
  list("raw_inflation",   "inflation",   "m", c("dt","id_api","mcrd081","ku","mcrk110","tzep")),
  list("raw_klk",         "klk",         "m", c("dt","id_api","nkb","s080","r034")),
  list("raw_fisin",       "fisin",       "m", c("dt","id_api","ekpok")),
  list("raw_monetary",    "monetary",    "m", c("dt","id_api","k076","tzep")),
  list("raw_survey",      "survey",      "m", c("dt","id_api")),
  list("raw_q_survey",    "qsurvey",     "q", c("dt","id_api","kod_esrv","odku","tzep"))   # NBU set name is "qsurvey" (q_survey -> HTTP 404)
)

# ------------------------------ GAP FILLING ----------------------------------
# Report months that are absent INSIDE the stored range [first stored month, last stored month]
# (and before it, back to START_REPORT_DATE). Consecutive months are merged into windows.
find_gaps <- function(x, from = START_REPORT_DATE, col = "dt") {
  if (!nrow(x) || !col %in% names(x)) return(data.table(start = as.Date(character()), end = as.Date(character()), n = integer()))
  pr <- unique(month_floor(parse_nbu_date(x[[col]]))); pr <- pr[is.finite(pr)]
  exp <- seq(month_floor(from), max(pr), by = "month"); miss <- sort(exp[!exp %in% pr])
  if (!length(miss)) return(data.table(start = as.Date(character()), end = as.Date(character()), n = integer()))
  mi <- as.integer(format(miss, "%Y")) * 12L + as.integer(format(miss, "%m"))
  g <- cumsum(c(1L, diff(mi) != 1L))
  data.table(m = miss, g = g)[, .(start = min(m), end = max(m), n = .N), by = g][, g := NULL][]
}

fill_gaps_block <- function(name, apikod, period, key_cols, filters = list()) {
  old <- store_read(name)
  if (!nrow(old)) { log_msg(sprintf("%-16s gaps: table is empty — run the full download", name)); return(NULL) }
  gp <- find_gaps(old)
  if (!nrow(gp)) { log_msg(sprintf("%-16s gaps: none (%s … %s)", name, min(parse_nbu_date(old$dt)), last_date_of(old)))
                   return(data.table(table = name, start = NA, end = NA, months_missing = 0L, rows_found = 0L, months_filled = 0L, still_missing = 0L)) }
  rep <- vector("list", nrow(gp)); got <- vector("list", nrow(gp))
  for (i in seq_len(nrow(gp))) {
    w_end <- add_months(gp$end[i], 1L) - 1L
    new <- tryCatch(nbu_stat(apikod, gp$start[i], w_end, period, filters, cache = FALSE),
                    error = function(e) { warning(name, " gap ", gp$start[i], ": ", conditionMessage(e)); data.table() })
    filled <- if (nrow(new) && "dt" %in% names(new)) uniqueN(month_floor(parse_nbu_date(new$dt))) else 0L
    log_msg(sprintf("%-16s gap %s … %s (%d міс.): +%d rows, %d months returned by NBU",
                    name, format(gp$start[i], "%Y-%m"), format(gp$end[i], "%Y-%m"), gp$n[i], nrow(new), filled))
    got[[i]] <- new
    rep[[i]] <- data.table(table = name, start = gp$start[i], end = gp$end[i], months_missing = gp$n[i], rows_found = nrow(new), months_filled = filled)
  }
  new <- rbindlist(got, fill = TRUE, use.names = TRUE)
  if (nrow(new)) {
    common <- union(names(old), names(new))
    for (nm in setdiff(common, names(old))) old[, (nm) := NA]
    for (nm in setdiff(common, names(new))) new[, (nm) := NA]
    for (nm in common) if (!identical(class(old[[nm]]), class(new[[nm]]))) {
      old[, (nm) := as.character(get(nm))]; new[, (nm) := as.character(get(nm))] }
    if ("value" %in% common) { old[, value := num(value)]; new[, value := num(value)] }
    x <- unique(rbindlist(list(old, new), use.names = TRUE), by = intersect(key_cols, common), fromLast = TRUE)
    store_write(x, name); old <- x
  }
  left <- find_gaps(old)
  out <- rbindlist(rep); out[, still_missing := sum(left$n)]
  if (nrow(left)) log_msg(sprintf("%-16s STILL missing %d months: %s", name, sum(left$n),
                                  paste(sprintf("%s…%s", format(left$start, "%Y-%m"), format(left$end, "%Y-%m")), collapse = ", ")))
  out
}

fill_all_gaps <- function(blocks) {
  res <- rbindlist(lapply(blocks, function(b) tryCatch(fill_gaps_block(b[[1]], b[[2]], b[[3]], b[[4]]),
                                                       error = function(e) { warning(b[[1]], ": ", conditionMessage(e)); NULL })), fill = TRUE)
  if (nrow(res)) fwrite(res, file.path(LOG_DIR, "db_gaps_v6.csv"), bom = TRUE)
  invisible(res)
}
