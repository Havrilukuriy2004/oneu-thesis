# Exact-date prudential inputs. The Python reader handles all historical layouts.
ensure_capital_inputs <- function() {
  inputs <- file.path(LOCAL_DIR, c("Skladovi_RK_2026-10-01.xlsx", "Ratios_by_Banks_2026-10-01.xlsx"))
  if (!all(file.exists(inputs))) stop("Place both supplied capital workbooks in ", LOCAL_DIR)
  target <- file.path(PARQUET_DIR, "mart_bank_prudential_v7.csv")
  parser <- file.path(PROJECT_DIR, "capital_inputs.py")
  fresh <- file.exists(target) && file.info(target)$mtime >= max(file.info(c(inputs, parser))$mtime)
  if (!fresh) {
    py <- Sys.getenv("THESIS_PYTHON", "")
    if (!nzchar(py)) { py <- Sys.which(c("python3", "python")); py <- unname(py[nzchar(py)])[1] }
    if (!length(py) || is.na(py)) stop("Python is required for capital_inputs.py")
    result <- system2(shQuote(py), c(shQuote(parser), shQuote(PROJECT_DIR)))
    if (result != 0) stop("Capital workbook import failed; existing capital outputs are stale")
  }
  invisible(target)
}

attach_capital_inputs <- function(x) {
  target <- ensure_capital_inputs()
  cap <- data.table::fread(target, colClasses = list(character = "nkb"))
  cap[, `:=`(month = as.Date(month), report_date = as.Date(report_date))]
  if (anyDuplicated(cap[, .(nkb, month)])) stop("Duplicate prudential bank-month keys")
  x <- data.table::copy(x)
  x[, `:=`(nkb = as.character(nkb), month = as.Date(month))]
  keep <- c("nkb", "month", setdiff(names(cap), names(x)))
  merge(x, cap[, ..keep], by = c("nkb", "month"), all.x = TRUE, sort = FALSE)
}
