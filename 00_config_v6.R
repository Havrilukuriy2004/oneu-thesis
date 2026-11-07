# ============================================================================
# 00_config_v6.R — shared configuration and helpers (pipeline v6)
# Master thesis: "Вартість фондування, процентне переоцінювання та розподіл
#                 активів банків України"
# ----------------------------------------------------------------------------
# Sourced by:
#   01_nbu_download_v6.R      (network layer; NBU OpenData API)
#   02_build_marts_v6.R       (offline build: raw -> staging -> marts)
#   03_section_2_1_analysis_v2.R (Section 2.1 empirical package)
#
# The ONLY thing you normally edit is PROJECT_DIR (or set the environment
# variable THESIS_CODE_DIR before sourcing).
# ============================================================================

PIPELINE_VERSION <- "7.1.0"

# ------------------------------ PATHS ----------------------------------------
.default_dir <- getwd()
PROJECT_DIR <- Sys.getenv("THESIS_CODE_DIR", unset = .default_dir)
if (!dir.exists(PROJECT_DIR)) PROJECT_DIR <- getwd()
PROJECT_DIR <- normalizePath(PROJECT_DIR, winslash = "/", mustWork = FALSE)

DATA_DIR    <- file.path(PROJECT_DIR, "data_nbu")
RAW_DIR     <- file.path(DATA_DIR, "raw")          # JSON->RDS API cache
PARQUET_DIR <- file.path(DATA_DIR, "parquet")      # raw + mart tables
LOCAL_DIR   <- file.path(DATA_DIR, "local_inputs") # xlsx supplied by hand
OUTPUT_DIR  <- file.path(PROJECT_DIR, "output_nbu")
LOG_DIR     <- file.path(OUTPUT_DIR, "logs")

for (d in c(DATA_DIR, RAW_DIR, PARQUET_DIR, LOCAL_DIR, OUTPUT_DIR, LOG_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ------------------------------ SAMPLE ---------------------------------------
START_REPORT_DATE <- as.Date("2018-02-01")   # first OSB report date (Jan-2018 data)
END_REPORT_DATE   <- as.Date(format(Sys.Date(), "%Y-%m-01"))
START_MONTH       <- as.Date("2018-01-01")   # economic month = report date - 1m
WAR_START         <- as.Date("2022-02-01")

# Rows re-downloaded before the last stored date of each series (captures
# NBU revisions of the latest months).
REVISION_WINDOW_MONTHS <- 3L

# ------------------------------ PACKAGES -------------------------------------
AUTO_INSTALL <- identical(Sys.getenv("THESIS_AUTO_INSTALL", "false"), "true")
.need <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) && AUTO_INSTALL)
    try(install.packages(miss, repos = "https://cloud.r-project.org"), silent = TRUE)
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "))
  invisible(TRUE)
}
.need(c("data.table", "lubridate", "jsonlite", "digest", "openxlsx"))
suppressPackageStartupMessages({
  library(data.table); library(lubridate)
})
HAS_ARROW <- requireNamespace("arrow", quietly = TRUE)

# ------------------------------ I/O ------------------------------------------
# Parquet if {arrow} is installed, otherwise RDS. Reading with mmap = FALSE
# avoids the Windows "user-mapped section" lock (error 1224) seen in v5.x.
store_path <- function(name, ext = if (HAS_ARROW) "parquet" else "rds")
  file.path(PARQUET_DIR, paste0(name, ".", ext))

store_exists <- function(name)
  file.exists(store_path(name, "parquet")) || file.exists(store_path(name, "rds"))

store_read <- function(name) {
  p <- store_path(name, "parquet"); r <- store_path(name, "rds")
  if (HAS_ARROW && file.exists(p)) {
    x <- arrow::read_parquet(p, as_data_frame = TRUE, mmap = FALSE)
    return(data.table::as.data.table(x))
  }
  if (file.exists(r)) return(data.table::as.data.table(readRDS(r)))
  if (file.exists(p)) stop("{arrow} is required to read ", p)
  data.table::data.table()
}

store_write <- function(x, name) {
  if (is.null(x) || !NROW(x)) return(invisible(FALSE))
  x <- as.data.frame(x)
  if (HAS_ARROW) {
    final <- store_path(name, "parquet")
    tmp <- paste0(final, ".tmp")
    arrow::write_parquet(x, tmp)
    if (file.exists(final)) {
      ok <- suppressWarnings(file.remove(final))
      if (!ok) stop("Cannot replace ", final, " — close any program using it (Excel/Power BI/Python) and rerun.")
    }
    file.rename(tmp, final)
  } else {
    saveRDS(x, store_path(name, "rds"), compress = "xz")
  }
  invisible(TRUE)
}

# ------------------------------ HELPERS --------------------------------------
num  <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
chr  <- function(x) as.character(x)
safe_div <- function(a, b) ifelse(is.finite(a) & is.finite(b) & abs(b) > 1e-12, a / b, NA_real_)

parse_nbu_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  out <- suppressWarnings(as.Date(x, format = "%Y%m%d"))
  bad <- is.na(out); if (any(bad)) out[bad] <- suppressWarnings(as.Date(x[bad], format = "%d.%m.%Y"))
  bad <- is.na(out); if (any(bad)) out[bad] <- suppressWarnings(as.Date(x[bad], format = "%Y-%m-%d"))
  bad <- is.na(out); if (any(bad)) out[bad] <- suppressWarnings(as.Date(as.numeric(x[bad]), origin = "1970-01-01"))
  out
}
month_floor <- function(d) as.Date(format(as.Date(d), "%Y-%m-01"))
add_months  <- function(d, k) as.Date(lubridate::`%m+%`(as.Date(d), months(k)))

# Day-count annualisation of a MONTHLY accounting flow:
#   r_annual = (days_in_year / days_in_month) * flow / average_stock
# (12 * flow / stock under-states February by ~8% and over-states 31-day
#  months by ~2%; banks accrue on an actual-day basis.)
ann_factor <- function(month) {
  m <- as.Date(month)
  dim <- as.numeric(lubridate::days_in_month(m))
  diy <- ifelse(lubridate::leap_year(m), 366, 365)
  diy / dim
}

wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

winsor <- function(x, p = c(.01, .99)) {
  if (sum(is.finite(x)) < 10) return(x)
  q <- stats::quantile(x, p, na.rm = TRUE, names = FALSE)
  pmin(pmax(x, q[1]), q[2])
}

# Percent -> decimal only when the series is in percent units (robust to NA).
pct_to_dec <- function(x) {
  x <- as.numeric(x)
  if (stats::median(abs(x), na.rm = TRUE) > 1) x / 100 else x
}

log_msg <- function(...) {
  m <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", paste0(...))
  message(m)
  cat(m, "\n", file = file.path(LOG_DIR, "pipeline_v6.log"), append = TRUE)
}

message("Pipeline v", PIPELINE_VERSION, " | PROJECT_DIR = ", PROJECT_DIR,
        " | storage = ", if (HAS_ARROW) "parquet" else "rds")

source(file.path(PROJECT_DIR, "utils_review_v7.R"), encoding = "UTF-8")
.store_read_original <- store_read
source(file.path(PROJECT_DIR, "utils_capital_inputs_v7.R"), encoding = "UTF-8")
store_read <- function(name) {
  x <- clean_bank_columns(.store_read_original(name))
  if (name == "mart_bank_month_v6" && nrow(x)) x <- attach_capital_inputs(x)
  x
}
