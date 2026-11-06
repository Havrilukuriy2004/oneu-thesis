# ============================================================================
# 12_bank_excel_v6.R — динамічний Excel по банках (2.3 + 2.4): викликає 12_bank_excel.py
# ----------------------------------------------------------------------------
# Потрібні результати 10_ і 11_ (output_nbu/section_2_3, section_2_4) і Python 3 з openpyxl.
# Результат: output_nbu/bank_excel/Bank_Repricing_Funding_Tool.xlsx (формули перераховуються при відкритті).
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
py <- Sys.which(c("python3", "python")); py <- py[nzchar(py)][1]
if (is.na(py)) stop("Не знайдено python3. macOS: xcode-select --install або brew install python")
has_opx <- system2(py, c("-c", shQuote("import openpyxl")), stdout = FALSE, stderr = FALSE) == 0
if (!has_opx) {
  message("Встановлюю openpyxl (pip3 install --user openpyxl) ...")
  system2(py, c("-m", "pip", "install", "--user", "--quiet", "openpyxl"))
}
script <- file.path(PROJECT_DIR, "12_bank_excel.py")
res <- system2(py, c(shQuote(script), shQuote(OUTPUT_DIR)), stdout = TRUE, stderr = TRUE)
cat(res, sep = "\n")
if (!any(grepl("^OK:", res))) stop("12_bank_excel.py завершився з помилкою")
