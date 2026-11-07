# Windows RStudio Python bridge fix

The capital importer previously selected the first python3 on PATH without checking it, quoted the executable before passing it to system2, and replaced all nonzero exit codes with a generic stale-output message. On Windows this could select an App Execution Alias or fail at process launch. The reported message alone cannot identify which failure happened on a user's PC.

The bridge now probes a real Python 3 interpreter, supports the py launcher, skips Store aliases during automatic discovery, checks numpy/pandas/openpyxl/pyarrow, and uses the same interpreter for capital, both workbooks and the review supplement. It passes the executable unquoted and quotes each argument. Python runs in UTF-8 mode. Failed scripts print their real status and traceback and save logs under output_nbu/logs. A failed import is retried until its CSV, Parquet and manifest are produced successfully.

After Git Pull, restart R, then run from the project root.

```r
PROJECT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source("utils_capital_inputs_v7.R", encoding = "UTF-8")
resolve_thesis_python(check_packages = FALSE)
install_thesis_python_packages()
source("run_all_v6.R", encoding = "UTF-8")
```

The install helper explicitly installs requirements.txt into the selected interpreter. It does not install Python itself. If no real Python is found, install Python 3.12 or set THESIS_PYTHON to a known python.exe path, then retry. Do not put extra command arguments inside THESIS_PYTHON. No capital workbooks need to be supplied again.

Warnings that data.table/lubridate were built under a later R patch release are separate from a Python subprocess failure. A full numerical pipeline run remains a separate validation step.

The CI parses R and compiles Python, and exercises real subprocesses on Windows with Unicode paths, spaces, missing modules, nonzero exits and retry after partial output. These tests do not estimate banking models.
