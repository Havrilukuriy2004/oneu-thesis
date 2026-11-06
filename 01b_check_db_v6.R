# ============================================================================
# 01b_check_db_v6.R — health check of the local NBU database (no network)
# ----------------------------------------------------------------------------
# Run after 01 (or any time). Writes output_nbu/logs/db_check_v6.csv and prints
# a short verdict per table: last date, rows, duplicates, known pitfalls.
# Nothing is modified.
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_local_inputs_v6.R"), encoding = "UTF-8")

# key columns = one row per observation (same keys as in 01_nbu_download_v6.R)
KEYS <- list(
  raw_banksfinrep = c("dt", "id_api", "nkb", "r034"),
  raw_mir         = c("dt", "id_api", "odkodter", "odr030", "ods180", "odk111", "odf074", "k140", "repository", "tzep"),
  raw_deposit     = c("dt", "id_api", "odkodter", "odr030", "ods183dd", "odk111", "odk051", "odf074", "k140", "tzep"),
  raw_loan        = c("dt", "id_api", "odkodter", "odr030", "ods183ld", "odk111", "odk051", "odf074", "k140", "tzep"),
  raw_key         = c("dt", "id_api"),
  raw_liquidity   = c("dt", "id_api"),
  raw_inflation   = c("dt", "id_api", "mcrd081", "ku", "mcrk110", "tzep"),
  raw_klk         = c("dt", "id_api", "nkb", "s080", "r034"),
  raw_osb         = c("dt", "nkb", "r020", "r034", "t025"),
  raw_monetary    = c("dt", "id_api", "k076", "tzep"),
  raw_survey      = c("dt", "id_api"),
  raw_q_survey    = c("dt", "id_api", "kod_esrv", "odku", "tzep"),
  raw_uonia       = NULL, raw_ovdp = NULL, raw_fx = NULL)

last_dt <- function(x) {
  for (c in c("dt", "date", "month")) if (c %in% names(x)) return(suppressWarnings(max(as.Date(parse_nbu_date(x[[c]])), na.rm = TRUE)))
  as.Date(NA)
}
chk <- rbindlist(lapply(names(KEYS), function(nm) {
  x <- store_read(nm)
  if (!nrow(x)) return(data.table(table = nm, rows = 0L, last_date = as.Date(NA), exact_dups = NA_integer_, key_dups = NA_integer_, note = "НЕМАЄ — запустіть 01"))
  if ("id_api" %in% names(x)) x <- x[!is.na(id_api) & id_api != ""]      # technical rows without an indicator code
  k <- intersect(KEYS[[nm]] %||% character(), names(x))
  data.table(table = nm, rows = nrow(x), last_date = last_dt(x), exact_dups = sum(duplicated(x)),
             key_dups = if (length(k)) nrow(x) - uniqueN(x, by = k) else NA_integer_, note = "")
}))

# --- known pitfalls -----------------------------------------------------------
mir <- store_read("raw_mir")
if (nrow(mir)) {
  m <- unique(mir)
  ln <- m[grepl("^New_Loans_(NFinCorp|Households)$", id_api) & odkodter == "total" & odk111 == "total" & odf074 == "total" & odr030 == "01" & ods180 == "total"]
  cov <- ln[, .(has_total = any(k140 == "#"), n_size_classes = uniqueN(k140)), by = .(dt, id_api)]
  chk[table == "raw_mir", note := sprintf("k140: підсумок '#' є в %d з %d місяців×секторів кредитів (у моделях береться лише '#'); репозиторії: %s",
                                          sum(cov$has_total), nrow(cov), paste(sort(unique(m$repository)), collapse = "/"))]
  gap <- setdiff(format(seq(as.Date("2018-02-01"), max(as.Date(parse_nbu_date(m$dt))), by = "month"), "%Y%m01"), unique(as.character(m$dt)))
  if (length(gap)) chk[table == "raw_mir", note := paste0(note, sprintf("; немає звітних дат: %s … %s (%d міс.)", min(gap), max(gap), length(gap)))]
}
bf <- store_read("raw_banksfinrep")
if (nrow(bf)) chk[table == "raw_banksfinrep", note := sprintf("банків в останньому місяці: %d; BS1_InterestIncomeSec з %s",
  uniqueN(bf[dt == max(dt), nkb]), min(bf[id_api == "BS1_InterestIncomeSec", dt]))]

# --- local inputs ---------------------------------------------------------------
`%|%` <- function(a, b) ifelse(is.na(a), b, a)
u <- read_uird_local(); g <- read_579_local()
nss <- store_read("local_nss_lc_params"); nsf <- store_read("local_ns_fc_params")
ld <- function(x) if (nrow(x)) last_dt(x) else as.Date(NA)
loc <- data.table(table = c("local: UIRD", "local: 5-7-9 (ФРП)", "local: криві NSS UAH (у базі)", "local: криві NS FX (у базі)"),
                  rows = c(if (is.null(u)) 0L else nrow(u$daily), if (is.null(g)) 0L else nrow(g$weekly), nrow(nss), nrow(nsf)),
                  last_date = c(if (is.null(u)) as.Date(NA) else max(u$daily$date), if (is.null(g)) as.Date(NA) else max(g$weekly$date), ld(nss), ld(nsf)),
                  exact_dups = NA_integer_, key_dups = NA_integer_,
                  note = c(u$file %||% "немає UIRD.xls у local_inputs", g$file %||% "немає 5-7-9.xlsx у local_inputs",
                           "оновлюються з NSS_parameters_LC.xlsx у local_inputs при запуску 02", "оновлюються з N_S_parameters_FC.xlsx у local_inputs при запуску 02"))
chk <- rbind(chk, loc)
# new local inputs (04.11.2026): full NBU rate statistics, expectation surveys, BLS
fl <- function(pat) { f <- find_local(pat); if (length(f)) basename(f[1]) else NA_character_ }
fm <- tryCatch(read_fm_local(), error = function(e) NULL)
pe <- tryCatch(read_survey_expect("surveys?_price.*\\.xlsx$", "pie"), error = function(e) NULL)
fe <- tryCatch(read_survey_expect("surveys?_rate.*\\.xlsx$", "fxe"), error = function(e) NULL)
bl <- tryCatch(read_bls_local(), error = function(e) NULL)
nr <- function(x) if (is.null(x)) 0L else nrow(x)
loc2 <- data.table(table = c("local: ставки НБУ (4-Financial_markets)", "local: інфляційні очікування (Surveys_price)", "local: курсові очікування (Surveys_rate)", "local: BLS"),
                   rows = c(nr(fm), nr(pe), nr(fe), nr(bl)),
                   last_date = c(if (nr(fm)) max(fm$month) else as.Date(NA), if (nr(pe)) max(pe$month) else as.Date(NA),
                                 if (nr(fe)) max(fe$month) else as.Date(NA), if (nr(bl)) add_months(max(bl$quarter), 3L) - 1L else as.Date(NA)),   # quarterly survey: end of the quarter
                   exact_dups = NA_integer_, key_dups = NA_integer_,
                   note = c(fl("financial_markets.*\\.xlsx$") %|% "немає 4-Financial_markets.xlsx", fl("surveys?_price.*\\.xlsx$") %|% "немає Surveys_price.xlsx",
                            fl("surveys?_rate.*\\.xlsx$") %|% "немає Surveys_rate.xlsx", fl("^bls.*\\.xlsx$") %|% "немає BLS_*.xlsx"))
chk <- rbind(chk, loc2)

# --- verdict --------------------------------------------------------------------
today <- Sys.Date()
chk[, status := fcase(rows == 0, "ВІДСУТНЄ",
                      !is.na(key_dups) & key_dups > 0, "ДУБЛІ (01 прибере при оновленні)",
                      !is.na(exact_dups) & exact_dups > 0, "ДУБЛІ",
                      !is.na(last_date) & as.numeric(today - last_date) > 75, "ЗАСТАРІЛЕ (>75 днів)",
                      default = "ok")]
fwrite(chk, file.path(LOG_DIR, "db_check_v6.csv"))
print(chk[, .(table, rows, last_date, key_dups, status, note)])
log_msg("DB CHECK: ", sum(chk$status == "ok"), " ok / ", nrow(chk), " — див. output_nbu/logs/db_check_v6.csv")
