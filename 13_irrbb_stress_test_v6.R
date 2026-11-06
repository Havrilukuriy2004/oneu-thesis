# ============================================================================
# 13_irrbb_stress_test_v6.R — стрес-тест IRRBB по кожному банку (Excel): викликає 13_irrbb_stress_test.py
# ----------------------------------------------------------------------------
# Потрібні результати 10_ (output_nbu/section_2_3: S23_*, у т.ч. S23_item_rates_latest.csv і S23_curves_history.csv).
# Результат: output_nbu/bank_excel/IRRBB_Stress_Test.xlsx (формули перераховуються при відкритті).
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
py <- Sys.which(c("python3", "python")); py <- py[nzchar(py)][1]
if (is.na(py)) stop("Не знайдено python3")
if (system2(py, c("-c", shQuote("import openpyxl")), stdout = FALSE, stderr = FALSE) != 0)
  system2(py, c("-m", "pip", "install", "--user", "--quiet", "openpyxl"))
res <- system2(py, c(shQuote(file.path(PROJECT_DIR, "13_irrbb_stress_test.py")), shQuote(OUTPUT_DIR)), stdout = TRUE, stderr = TRUE)
cat(res, sep = "\n")
if (!any(grepl("^OK:", res))) stop("13_irrbb_stress_test.py завершився з помилкою")
