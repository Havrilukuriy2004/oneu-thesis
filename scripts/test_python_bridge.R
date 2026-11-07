# Real subprocess regressions, including Windows paths with spaces and Unicode.
local({
  source("utils_capital_inputs_v7.R", local = TRUE, encoding = "UTF-8")
  old_python <- Sys.getenv("THESIS_PYTHON", unset = NA_character_)
  on.exit(if (is.na(old_python)) Sys.unsetenv("THESIS_PYTHON") else Sys.setenv(THESIS_PYTHON = old_python), add = TRUE)
  known_python <- Sys.getenv("TEST_PYTHON", "")
  if (nzchar(known_python)) Sys.setenv(THESIS_PYTHON = known_python)
  py <- resolve_thesis_python(check_packages = FALSE)
  root <- file.path(tempdir(), "thesis bridge Київ with spaces")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  PROJECT_DIR <- root
  LOCAL_DIR <- file.path(root, "data_nbu", "local_inputs")
  PARQUET_DIR <- file.path(root, "data_nbu", "parquet")
  dir.create(LOCAL_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(PARQUET_DIR, recursive = TRUE, showWarnings = FALSE)
  expect_error <- function(expr, pattern) {
    err <- tryCatch({force(expr); NULL}, error = identity)
    stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err), fixed = TRUE))
    invisible(err)
  }
  fake_lookup <- function(n) c(python = "C:/Program Files/Python/python.exe",
    py = "C:/Windows/py.exe", python3 = "C:/Users/User/AppData/Local/Microsoft/WindowsApps/python3.exe")[n]
  candidates <- .thesis_python_candidates("windows", fake_lookup, explicit = "")
  stopifnot(identical(candidates, c("C:/Program Files/Python/python.exe", "C:/Windows/py.exe")))

  script <- file.path(root, "echo аргументи.py")
  writeLines(c("import sys", "print(sys.argv[1])"), script, useBytes = TRUE)
  literal <- "Київ with spaces & dollar$ (literal)"
  output <- run_thesis_python(script, literal, check_packages = FALSE)
  stopifnot(identical(tail(output, 1), literal))

  # An OK-looking stdout must not mask a failing subprocess.
  writeLines(c("import sys", "print('OK: misleading output')", "print('fixture failure', file=sys.stderr)", "sys.exit(41)"), script)
  expect_error(run_thesis_python(script, check_packages = FALSE), "exit status 41")
  log <- file.path(root, "output_nbu", "logs", "echo аргументи_python.log")
  stopifnot(file.exists(log), any(grepl("fixture failure", readLines(log, encoding = "UTF-8"))))

  # A real clean virtual environment exposes missing modules in the error.
  venv <- file.path(root, "clean Python середовище")
  result <- .thesis_python_capture(py, c("-m", "venv", "--without-pip", venv))
  stopifnot(result$status == 0L)
  vpy <- if (.Platform$OS.type == "windows") file.path(venv, "Scripts", "python.exe") else file.path(venv, "bin", "python")
  Sys.setenv(THESIS_PYTHON = vpy)
  expect_error(resolve_thesis_python(), "install_thesis_python_packages()")
  Sys.setenv(THESIS_PYTHON = py)

  # A failed partial import must be retried even if old outputs look recent.
  file.create(file.path(LOCAL_DIR, c("Skladovi_RK_2026-10-01.xlsx", "Ratios_by_Banks_2026-10-01.xlsx")))
  file.create(file.path(root, "utils_capital_inputs_v7.R"))
  parser <- file.path(root, "capital_inputs.py")
  writeLines(c("import sys", "from pathlib import Path", "p=Path(sys.argv[1])/'data_nbu/parquet/mart_bank_prudential_v7.csv'",
               "p.write_text('partial fixture', encoding='utf-8')", "sys.exit(41)"), parser)
  original_run <- run_thesis_python
  run_thesis_python <- function(script, args = character(), ...) original_run(script, args, check_packages = FALSE)
  expect_error(ensure_capital_inputs(), "exit status 41")
  stopifnot(file.exists(file.path(PARQUET_DIR, ".capital_import_incomplete")))
  manifest <- file.path(root, "output_nbu", "capital", "tables", "capital_manifest.json")
  dir.create(dirname(manifest), recursive = TRUE, showWarnings = FALSE)
  file.create(manifest, file.path(PARQUET_DIR, "mart_bank_prudential_v7.parquet"))
  expect_error(ensure_capital_inputs(), "exit status 41")

  writeLines(c("import sys", "from pathlib import Path", "r=Path(sys.argv[1])",
    "(r/'data_nbu/parquet/mart_bank_prudential_v7.csv').write_text('complete fixture', encoding='utf-8')",
    "(r/'data_nbu/parquet/mart_bank_prudential_v7.parquet').touch()",
    "(r/'output_nbu/capital/tables/capital_manifest.json').touch()"), parser)
  stopifnot(file.exists(ensure_capital_inputs()), !file.exists(file.path(PARQUET_DIR, ".capital_import_incomplete")))
  message("Python bridge regression checks passed")
})
