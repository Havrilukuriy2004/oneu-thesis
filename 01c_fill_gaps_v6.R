# ============================================================================
# 01c_fill_gaps_v6.R — ЛИШЕ дозаповнення пропусків у базі (без повного оновлення)
# ----------------------------------------------------------------------------
# Сканує кожну місячну таблицю НБУ (raw_mir, raw_deposit, raw_banksfinrep, raw_klk, …)
# на пропущені звітні місяці всередині ряду (напр. MIR 2023-03…2024-05) і
# перезавантажує саме ці вікна з API НБУ (без кешу). Потрібен інтернет.
# Те саме робить 01_nbu_download_v6.R наприкінці кроку 2, тож при UPDATE_DB = TRUE
# окремо запускати не треба. Звіт: output_nbu/logs/db_gaps_v6.csv
# Після цього: BUILD (02) і моделі — тобто run_all_v6.R з UPDATE_DB <- FALSE.
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
source(file.path(PROJECT_DIR, "utils_nbu_api_v6.R"), encoding = "UTF-8")

ONLY <- NULL            # напр. c("raw_mir") — лише ці таблиці; NULL = усі місячні
blocks <- Filter(function(b) b[[3]] == "m" && (is.null(ONLY) || b[[1]] %in% ONLY), STAT_BLOCKS)
res <- fill_all_gaps(blocks)
print(res)
mir <- store_read("raw_mir")
if (nrow(mir)) {
  m <- mir[, .(rows = .N), keyby = .(month = format(parse_nbu_date(dt), "%Y-%m"))]
  message("MIR: місяців у базі ", nrow(m), ", перший ", m$month[1], ", останній ", m$month[nrow(m)])
  print(m[month >= "2022-10" & month <= "2024-12"])
}
