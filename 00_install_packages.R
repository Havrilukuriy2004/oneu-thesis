# ============================================================================
# 00_install_packages.R — встановлення всіх пакетів R (запустити ОДИН раз на новому комп'ютері)
# macOS / Windows: бінарні пакети CRAN, компілятор не потрібен.
# ============================================================================
pk <- c("data.table", "lubridate", "jsonlite", "digest", "openxlsx", "readxl", "httr2", "rlang",
        "ggplot2", "scales", "sandwich", "strucchange", "zoo", "numDeriv", "MASS",
        "ragg", "systemfonts", "svglite", "devEMF", "officer", "flextable",
        "arrow")                       # arrow = читання/запис .parquet (без нього — .rds)
miss <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org", type = if (.Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin") "binary" else getOption("pkgType"))
still <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)]
if (length(still)) message("НЕ встановлено: ", paste(still, collapse = ", "),
                           if ("arrow" %in% still) "\n(arrow не обов'язковий: код автоматично перейде на .rds)" else "") else
  message("Усі пакети встановлено. R ", getRversion(), ", arrow: ", requireNamespace("arrow", quietly = TRUE))
if (requireNamespace("systemfonts", quietly = TRUE))
  message("Times New Roman у системі: ", any(grepl("Times New Roman", systemfonts::system_fonts()$family, fixed = TRUE)))
