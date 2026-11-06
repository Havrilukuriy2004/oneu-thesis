# ============================================================================
# run_all_v6.R — ONE entry point: update the database, then rebuild, then models
# ----------------------------------------------------------------------------
# Layers (models never download anything):
#   1) DATABASE   01_nbu_download_v6.R   NBU API -> data_nbu/parquet/raw_*   (incremental, needs internet)
#                 01b_check_db_v6.R       health check of raw_* + local_inputs (offline)
#   2) MARTS      02_build_marts_v6.R    raw_* + local_inputs -> mart_* , macro_monthly_v6 (offline)
#   3) MODELS     03–07                  read ONLY mart_* / macro_monthly_v6 (offline); 06 = показники динаміки; 07 = підрозділ 2.2; 09 = очікування агентів, повна статистика ставок, грошова статистика; 10 = 2.3 виявлений розрив переоцінки; 11 = 2.4 тіньова вартість фондування; 08 = таблиці для тексту (Word); 12 = динамічний Excel по банках
#   4) ORIGIN     every figure also goes to output_nbu/origin/ -> origin_build_figures.py (inside Origin)
#
# Monthly routine: replace UIRD.xls and 5-7-9.xlsx in data_nbu/local_inputs, then
#   source("run_all_v6.R", encoding = "UTF-8")
# After a code fix in 02 (like the MIR k140 fix) no download is needed:
#   set UPDATE_DB <- FALSE and run.
# ============================================================================
UPDATE_DB  <- identical(Sys.getenv("THESIS_UPDATE_DB", "false"), "true")    # 01: pull new data from NBU (last 3 months + new months) AND fill holes inside series (e.g. MIR 2023-03…2024-05)
FILL_GAPS  <- FALSE   # 01c: ONLY fill holes inside series (quick; use when UPDATE_DB = FALSE)
CHECK_DB   <- TRUE    # 01b: database health check -> output_nbu/logs/db_check_v6.csv
BUILD      <- TRUE    # 02: rebuild marts (always after UPDATE_DB or any change in local_inputs)
MODELS     <- c("03", "04", "05", "06", "07", "09", "10", "14", "11", "08", "12", "13")   # character(0) = none; 09 = очікування і гроші; 10 = 2.3; 11 = 2.4; 08 = таблиці Word; 12 = динамічний Excel

`%||%` <- function(a, b) if (is.null(a)) b else a
.root <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
if (!length(.root) || !nzchar(.root[1])) .root <- Sys.getenv("THESIS_CODE_DIR", unset = getwd())
Sys.setenv(THESIS_CODE_DIR = .root)
SCRIPTS <- c("01" = "01_nbu_download_v6.R", "01c" = "01c_fill_gaps_v6.R", "01b" = "01b_check_db_v6.R", "02" = "02_build_marts_v6.R",
             "03" = "03_section_2_1_analysis_v2.R", "04" = "04_section_2_1_deposit_dynamics.R", "05" = "05_section_2_1_assets_joint.R",
             "06" = "06_dynamics_v6.R", "07" = "07_section_2_2_transmission.R", "09" = "09_expectations_money_v6.R",
             "10" = "10_section_2_3_repricing_gap_v6.R", "11" = "11_section_2_4_shadow_cost_v6.R", "08" = "08_thesis_tables_v6.R", "12" = "12_bank_excel_v6.R", "13" = "13_irrbb_stress_test_v6.R", "14" = "14_ou_deposit_dynamics_v6.R")
run_step <- function(id) {
  f <- file.path(.root, SCRIPTS[[id]]); t0 <- Sys.time()
  message(sprintf("\n========== %s  %s ==========", id, SCRIPTS[[id]]))
  ok <- tryCatch({ source(f, encoding = "UTF-8", local = new.env());
    if (exists("STATUS", envir = .GlobalEnv) && any(get("STATUS", envir = .GlobalEnv)$status == "error")) stop("Model block failed; see status tables")
    TRUE },
                 error = function(e) { message("ПОМИЛКА у ", SCRIPTS[[id]], ": ", conditionMessage(e)); FALSE })
  message(sprintf("---------- %s: %s за %.1f хв", id, if (ok) "готово" else "ЗУПИНЕНО", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  ok
}
plan <- c(if (UPDATE_DB) "01", if (!UPDATE_DB && FILL_GAPS) "01c", if (CHECK_DB) "01b", if (BUILD) "02", MODELS)
done <- character()
for (id in plan) {
  ok <- run_step(id); done <- c(done, setNames(if (ok) "ok" else "error", id))
  if (!ok) { message("Наступні кроки пропущено: вони залежать від ", id); break }
}
print(done)

if (any(done == "error")) stop("Pipeline failed; inspect the step log. Existing outputs may be stale.")
if (identical(Sys.getenv("THESIS_REVIEW", "true"), "true")) {
  py <- Sys.getenv("THESIS_PYTHON", "python3")
  code <- system2(py, c(shQuote(file.path(.root, "15_robustness_review.py")), shQuote(.root)))
  if (code != 0) stop("Robustness supplement failed")
}
