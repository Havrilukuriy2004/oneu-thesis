# ============================================================================
# utils_models_v6.R — shared estimators, style and I/O helpers for 04/05
# ============================================================================
.need(c("ggplot2", "scales", "openxlsx"))
suppressPackageStartupMessages({ library(ggplot2); library(scales) })
source(file.path(PROJECT_DIR, "utils_origin_v6.R"), encoding = "UTF-8")

# ------------------------------ style ----------------------------------------
PAL <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"
theme_thesis <- function() theme_minimal(base_size = 11) + theme(
  plot.title = element_text(face = "bold", size = 12.5, colour = INK),
  plot.subtitle = element_text(size = 9.5, colour = INK2), plot.caption = element_text(size = 8, colour = INK2, hjust = 0),
  axis.text = element_text(colour = INK2), axis.title = element_text(size = 9.5, colour = INK2),
  panel.grid.major = element_line(colour = GRID, linewidth = .3), panel.grid.minor = element_blank(),
  legend.position = "bottom", legend.title = element_blank(), strip.text = element_text(face = "bold", colour = INK))
p2 <- function(x, d = 2) formatC(100 * x, format = "f", digits = d)

# ------------------------------ estimators -----------------------------------
# OLS with Newey–West (Bartlett) HAC covariance.
nw_ols <- function(y, X, lags) {
  X <- as.matrix(X); ok <- is.finite(y) & apply(is.finite(X), 1, all)
  y <- y[ok]; X <- cbind(`(Intercept)` = 1, X[ok, , drop = FALSE])
  if (length(y) <= ncol(X) + 5) stop("too few observations")
  fit <- lm.fit(X, y); u <- fit$residuals; b <- fit$coefficients
  bread <- solve(crossprod(X)); S <- crossprod(X * u); n <- length(u)
  for (l in seq_len(min(lags, n - 1))) {
    w <- 1 - l / (lags + 1)
    G <- crossprod(X[-(1:l), , drop = FALSE] * u[-(1:l)], X[1:(n - l), , drop = FALSE] * u[1:(n - l)])
    S <- S + w * (G + t(G))
  }
  V <- bread %*% S %*% bread * n / (n - ncol(X))
  list(b = b, V = V, n = n, r2 = 1 - sum(u^2) / sum((y - mean(y))^2))
}

# State-dependent long-difference beta: Δh y = a + γ1 Δh i + γ2 Δh i · i(t−h).
ld_beta <- function(y, i, h = 12L, grid = c(.06, .10, .15, .20, .25)) {
  dy <- y - data.table::shift(y, h); di <- i - data.table::shift(i, h); i0 <- data.table::shift(i, h)
  f <- nw_ols(dy, cbind(di = di, di_x_i0 = di * i0), lags = h)
  V <- f$V[c("di", "di_x_i0"), c("di", "di_x_i0")]; g1 <- f$b[["di"]]; g2 <- f$b[["di_x_i0"]]
  data.table::rbindlist(lapply(grid, function(x) {
    be <- g1 + g2 * x; se <- sqrt(V[1, 1] + x^2 * V[2, 2] + 2 * x * V[1, 2])
    data.table::data.table(i0 = x, beta = be, se = se, lo90 = be - 1.645 * se, hi90 = be + 1.645 * se,
                           gamma2 = g2, p_gamma2 = 2 * pnorm(-abs(g2 / sqrt(V[2, 2]))), n = f$n)
  }))
}

# Local projection, cumulative response of y to Δi; optional sign asymmetry.
lp_path <- function(y, i, H = 24, asym = FALSE, nlag = 3) {
  di <- i - data.table::shift(i)
  ctrl <- do.call(cbind, lapply(1:nlag, function(l) cbind(data.table::shift(y - data.table::shift(y), l), data.table::shift(di, l))))
  colnames(ctrl) <- paste0("c", seq_len(ncol(ctrl)))
  data.table::rbindlist(lapply(0:H, function(h) {
    lhs <- data.table::shift(y, h, type = "lead") - data.table::shift(y, 1)
    X <- if (asym) cbind(up = pmax(di, 0), down = pmin(di, 0), ctrl) else cbind(di = di, ctrl)
    f <- tryCatch(nw_ols(lhs, X, lags = h + 1), error = function(e) NULL); if (is.null(f)) return(NULL)
    terms <- if (asym) c("up", "down") else "di"
    data.table::data.table(h = h, term = terms, b = f$b[terms], se = sqrt(diag(f$V)[terms]), n = f$n)
  }))
}

# Within estimator (bank FE), Driscoll–Kraay or cluster-robust covariance.
fe_dk <- function(d, y, x, fe = "nkb", time = "month", vcov = c("dk", "cluster"), lags = NULL) {
  vcov <- match.arg(vcov)
  cols <- c(y, x, fe, time); d <- d[complete.cases(d[, ..cols])]
  cyx <- c(y, x); Z <- as.matrix(d[, ..cyx])
  for (it in 1:200) { Z0 <- Z; for (f in fe) { g <- as.character(d[[f]]); Z <- Z - rowsum(Z, g)[g, , drop = FALSE] / as.vector(table(g)[g]) }
    if (max(abs(Z - Z0)) < 1e-12) break }
  yv <- Z[, 1]; X <- Z[, -1, drop = FALSE]; B <- solve(crossprod(X)); b <- B %*% crossprod(X, yv); u <- as.vector(yv - X %*% b)
  if (vcov == "cluster") { cl <- d[[fe[1]]]; G <- length(unique(cl)); S <- crossprod(rowsum(X * u, cl)); adj <- G / (G - 1) } else {
    h <- rowsum(X * u, as.character(d[[time]])); h <- h[order(rownames(h)), , drop = FALSE]; Tn <- nrow(h)
    L <- lags %||% floor(4 * (Tn / 100)^(2 / 9)); S <- crossprod(h)
    for (l in seq_len(L)) { G <- crossprod(h[-(1:l), , drop = FALSE], h[1:(Tn - l), , drop = FALSE]); S <- S + (1 - l / (L + 1)) * (G + t(G)) }
    adj <- 1 }
  V <- B %*% S %*% B * adj; se <- sqrt(diag(V))
  data.table::data.table(term = colnames(X), estimate = as.vector(b), std.error = se, t = as.vector(b) / se,
                         p.value = 2 * pnorm(-abs(as.vector(b) / se)), n_obs = nrow(X), n_banks = data.table::uniqueN(d[[fe[1]]]))
}

# Exact midpoint (price / mix) decomposition of a share-weighted average.
# dt: month, cell, rate, w  ->  monthly price, mix, level, residual
midpoint_decomp <- function(dt) {
  dt <- data.table::copy(dt)[is.finite(rate) & is.finite(w) & w > 0]
  dt[, s := w / sum(w), by = month]
  data.table::setorder(dt, cell, month)
  dt[, `:=`(r_l = data.table::shift(rate), s_l = data.table::shift(s), m_l = data.table::shift(month)), by = cell]
  d <- dt[m_l == add_months(month, -1L)]
  d[, `:=`(price = (s + s_l) / 2 * (rate - r_l), mix = (rate + r_l) / 2 * (s - s_l))]
  lv <- dt[, .(level = sum(s * rate)), by = month]; data.table::setorder(lv, month)
  lv[, level_l := data.table::shift(level)]
  m <- merge(d[, .(price = sum(price), mix = sum(mix)), by = month], lv, by = "month")
  m[, `:=`(delta = level - level_l, residual = level - level_l - price - mix)]
  list(month = m[], cell = d[])
}

# Output registry helpers (call init_output() once per script).
init_output <- function(prefix, folder = "section_2_1_v2") {
  OUT <<- file.path(OUTPUT_DIR, folder); FIG <<- file.path(OUT, "figures"); TAB <<- file.path(OUT, "tables")
  for (d in c(FIG, TAB)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(FIG, pattern = paste0("^", prefix), full.names = TRUE)); unlink(list.files(TAB, pattern = paste0("^", prefix), full.names = TRUE))
  TABLES <<- list(); FACTS <<- data.table::data.table(item = character(), value = character(), note = character())
  STATUS <<- data.table::data.table(block = character(), status = character(), note = character())
}
save_fig <- function(p, f, w = 8.5, h = 4.8) {
  ggsave(file.path(FIG, f), p, width = w, height = h, dpi = 300, bg = "white")
  tryCatch(origin_export(p, f, w, h), error = function(e) log_msg("Origin export skipped for ", f, ": ", conditionMessage(e)))
}
save_tab <- function(x, f, sheet) { x <- data.table::as.data.table(x); data.table::fwrite(x, file.path(TAB, f)); TABLES[[sheet]] <<- x; invisible(x) }
fact <- function(k, v, n = "") FACTS <<- rbind(FACTS, data.table::data.table(item = k, value = as.character(v), note = n))
status <- function(b, s, n = "") STATUS <<- rbind(STATUS, data.table::data.table(block = b, status = s, note = n))
write_workbook <- function(file) {
  wb <- openxlsx::createWorkbook(); hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#1F4E78", fontColour = "white")
  for (nm in names(TABLES)) { sh <- substr(nm, 1, 31); openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, as.data.frame(TABLES[[nm]]), headerStyle = hs, withFilter = TRUE); openxlsx::freezePane(wb, sh, firstRow = TRUE) }
  openxlsx::saveWorkbook(wb, file.path(OUT, file), overwrite = TRUE)
}

# academic figure style (overrides PAL, theme_thesis(), save_fig() above)
source(file.path(PROJECT_DIR, "utils_figures_academic_v6.R"), encoding = "UTF-8")
