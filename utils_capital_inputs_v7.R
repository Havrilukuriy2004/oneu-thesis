# A single, checked Python interpreter for the capital import and Python tools.
# system2 quotes the executable itself; quote arguments, never the command twice.
.thesis_python_capture <- function(command, args = character()) {
  tryCatch({
    out <- suppressWarnings(system2(command, vapply(args, shQuote, character(1)),
                                    stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    list(status = if (is.null(status)) 0L else as.integer(status), output = out)
  }, error = function(e) list(status = 127L, output = conditionMessage(e)))
}

.thesis_python_candidates <- function(os = .Platform$OS.type, lookup = Sys.which,
                                      explicit = Sys.getenv("THESIS_PYTHON", "")) {
  if (nzchar(explicit)) return(sub('^"(.*)"$', '\\1', explicit))
  names <- if (os == "windows") c("python", "py", "python3") else c("python3", "python", "py")
  paths <- unique(unname(lookup(names)))
  paths <- paths[nzchar(paths)]
  # Windows App Execution Aliases can open the Store instead of running Python.
  if (os == "windows") paths <- paths[!grepl(
    "/Microsoft/WindowsApps/python[0-9.]*[.]exe$", gsub("\\\\", "/", paths), ignore.case = TRUE)]
  paths
}

resolve_thesis_python <- function(check_packages = TRUE) {
  paths <- .thesis_python_candidates()
  if (!length(paths)) stop("Python 3 was not found. Install Python 3.12, restart RStudio, or set THESIS_PYTHON to the full python.exe path.", call. = FALSE)
  notes <- character()
  for (command in paths) {
    prefix <- if (grepl("^py([.]exe)?$", basename(command), ignore.case = TRUE)) "-3" else character()
    code <- paste("import sys; print('THESIS_EXECUTABLE=' + sys.executable);",
                  "assert sys.version_info.major == 3, 'Python 3 is required'")
    if (check_packages) code <- paste(code, "; import numpy, pandas, openpyxl, pyarrow")
    result <- .thesis_python_capture(command, c(prefix, "-X", "utf8", "-c", code))
    marker <- grep("^THESIS_EXECUTABLE=", result$output, value = TRUE)
    if (result$status == 0L && length(marker)) {
      py <- sub("^THESIS_EXECUTABLE=", "", marker[1])
      Sys.setenv(THESIS_PYTHON = py)
      return(py)
    }
    notes <- c(notes, paste0("\n", command, " (exit ", result$status, ")"), result$output)
  }
  stop(paste(c("No usable Python environment was found.", notes,
    if (check_packages) "If Python runs but packages are missing, run install_thesis_python_packages() once, then rerun the pipeline."), collapse = "\n"), call. = FALSE)
}

install_thesis_python_packages <- function() {
  py <- resolve_thesis_python(check_packages = FALSE)
  req <- file.path(PROJECT_DIR, "requirements.txt")
  if (!file.exists(req)) stop("Missing requirements.txt at ", req, call. = FALSE)
  message("Installing the project requirements into ", py)
  status <- system2(py, vapply(c("-X", "utf8", "-m", "pip", "install", "-r", req), shQuote, character(1)))
  if (status != 0L) stop("Python package installation failed. See the pip output above.", call. = FALSE)
  invisible(resolve_thesis_python())
}

run_thesis_python <- function(script, args = character(), check_packages = TRUE) {
  if (!file.exists(script)) stop("Python script does not exist at ", script, call. = FALSE)
  py <- resolve_thesis_python(check_packages)
  message("Python = ", py)
  result <- .thesis_python_capture(py, c("-X", "utf8", script, args))
  logs <- file.path(PROJECT_DIR, "output_nbu", "logs")
  dir.create(logs, recursive = TRUE, showWarnings = FALSE)
  log <- file.path(logs, paste0(tools::file_path_sans_ext(basename(script)), "_python.log"))
  writeLines(enc2utf8(c(paste("Python", py), paste("Script", script),
                       paste("Exit status", result$status), result$output)), log, useBytes = TRUE)
  if (length(result$output)) cat(result$output, sep = "\n")
  if (result$status != 0L) stop(paste(c(
    paste0(basename(script), " failed with exit status ", result$status, "."),
    paste0("Full Python log at ", log), tail(result$output, 40)), collapse = "\n"), call. = FALSE)
  invisible(result$output)
}

# Exact-date prudential inputs. Missing or partial output sets are never fresh.
ensure_capital_inputs <- function() {
  inputs <- file.path(LOCAL_DIR, c("Skladovi_RK_2026-10-01.xlsx", "Ratios_by_Banks_2026-10-01.xlsx"))
  if (!all(file.exists(inputs))) stop("Place both supplied capital workbooks in ", LOCAL_DIR)
  target <- file.path(PARQUET_DIR, "mart_bank_prudential_v7.csv")
  parser <- file.path(PROJECT_DIR, "capital_inputs.py")
  helper <- file.path(PROJECT_DIR, "utils_capital_inputs_v7.R")
  outputs <- c(target, sub("[.]csv$", ".parquet", target),
               file.path(PROJECT_DIR, "output_nbu", "capital", "tables", "capital_manifest.json"))
  failed <- file.path(PARQUET_DIR, ".capital_import_incomplete")
  sources <- c(inputs, parser, helper)
  fresh <- !file.exists(failed) && all(file.exists(outputs)) &&
    min(file.info(outputs)$mtime) >= max(file.info(sources)$mtime)
  if (!isTRUE(fresh)) {
    dir.create(PARQUET_DIR, recursive = TRUE, showWarnings = FALSE)
    writeLines("Import has not completed successfully", failed)
    run_thesis_python(parser, PROJECT_DIR)
    if (!all(file.exists(outputs))) stop("Capital import did not produce its CSV, Parquet and manifest. Inspect output_nbu/logs/capital_inputs_python.log.", call. = FALSE)
    unlink(failed)
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
