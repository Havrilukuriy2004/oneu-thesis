# ============================================================================
# utils_origin_v6.R — export of every ggplot figure for re-drawing in OriginLab
# ----------------------------------------------------------------------------
# save_fig() in 03–06 calls origin_export(p, file, w, h). For each figure it writes
#   output_nbu/origin/data/<fig>.csv   tidy data actually plotted:
#        panel, layer, geom, series, x, y, ymin, ymax, colour
#   output_nbu/origin/meta/<fig>.json  titles, axis titles, units, size, x type
# origin_build_figures.py (run inside Origin or with external Python + originpro)
# turns these into Origin graphs and exports PNG (600 dpi), EMF and PDF.
# Values are taken from the ggplot object BEFORE scaling, so legend labels,
# facet labels and dates are the original ones (no colour-code guessing).
# ============================================================================
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
.need(c("jsonlite", "rlang"))
ORIGIN_DIR <- file.path(OUTPUT_DIR, "origin")
for (d in file.path(ORIGIN_DIR, c("data", "meta"))) dir.create(d, recursive = TRUE, showWarnings = FALSE)

.ox_eval <- function(q, data) if (is.null(q)) NULL else tryCatch(rlang::eval_tidy(q, data), error = function(e) NULL)
.ox_lab  <- function(x) { if (is.null(x)) return(""); if (is.function(x) || inherits(x, "waiver")) return(""); paste(as.character(x), collapse = " ") }

# discrete scale with named labels (scale_colour_manual(labels = c(a = "…"))) -> relabel series
.ox_relabel <- function(p, aes_name, v) {
  sc <- tryCatch(p$scales$get_scales(aes_name), error = function(e) NULL)
  if (is.null(sc) || is.null(v)) return(v)
  lab <- sc$labels
  if (is.character(lab) && !is.null(names(lab))) { v <- as.character(v); m <- match(v, names(lab)); v[!is.na(m)] <- lab[m[!is.na(m)]] }
  v
}
.ox_percent <- function(p) {
  sc <- tryCatch(p$scales$get_scales("y"), error = function(e) NULL)
  if (is.null(sc) || !is.function(sc$labels)) return(FALSE)
  out <- tryCatch(sc$labels(c(0.1, 0.25)), error = function(e) "")
  any(grepl("%", out))
}

origin_export <- function(p, file, w = 8.5, h = 4.8) {
  if (!inherits(p, "ggplot")) return(invisible(NULL))
  fig <- sub("\\.[A-Za-z]+$", "", basename(file))
  built <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
  facet_vars <- tryCatch(names(c(p$facet$params$facets, p$facet$params$rows, p$facet$params$cols)), error = function(e) character())
  rows <- list(); horiz <- FALSE
  for (i in seq_along(p$layers)) {
    L <- p$layers[[i]]
    geom <- tolower(sub("^Geom", "", class(L$geom)[1]))
    if (geom == "abline") {                                        # y = a + b x reference line: y = intercept, ymin = slope
      a <- L$aes_params$intercept %||% L$geom_params$intercept %||% 0; b <- L$aes_params$slope %||% L$geom_params$slope %||% 1
      rows[[length(rows) + 1]] <- data.table::data.table(panel = "", layer = i, geom = geom, series = "", x = NA_character_, y = a[1], ymin = b[1], ymax = NA_real_, xline = NA_real_, colour = NA_character_)
      next
    }
    if (!inherits(L$stat, c("StatIdentity", "StatAlign")) && !is.null(built) && !geom %in% c("hline", "vline")) {   # fitted/statistical layers: take computed data
      ld <- built$data[[i]]
      sc_x <- tryCatch(p$scales$get_scales("x"), error = function(e) NULL)
      xv <- if (!is.null(sc_x) && inherits(sc_x, "ScaleContinuousDate")) format(as.Date(ld$x, origin = "1970-01-01")) else as.character(ld$x)
      rows[[length(rows) + 1]] <- data.table::data.table(panel = if (data.table::uniqueN(ld$PANEL) > 1) as.character(ld$PANEL) else "", layer = i, geom = geom, series = as.character(ld$group),
        x = xv, y = as.numeric(ld$y), ymin = as.numeric(ld$ymin %||% NA), ymax = as.numeric(ld$ymax %||% NA), xline = NA_real_, colour = as.character(ld$colour %||% NA))
      next
    }
    d <- if (is.data.frame(L$data) && nrow(L$data)) L$data else if (is.function(L$data)) tryCatch(L$data(p$data), error = function(e) NULL) else p$data
    if (!is.data.frame(d) || !nrow(d)) {
      if (geom %in% c("hline", "vline")) {                       # constant reference lines given as parameters
        v <- L$aes_params$yintercept %||% L$geom_params$yintercept %||% L$aes_params$xintercept
        if (!is.null(v)) rows[[length(rows) + 1]] <- data.table::data.table(panel = "", layer = i, geom = geom, series = "", x = NA_real_,
                                                                            y = if (geom == "hline") v else NA_real_, ymin = NA_real_, ymax = NA_real_,
                                                                            xline = if (geom == "vline") v else NA_real_, colour = NA_character_)
      }
      next
    }
    m <- c(p$mapping[setdiff(names(p$mapping), names(L$mapping))], L$mapping)
    if (!isTRUE(L$inherit.aes)) m <- L$mapping
    g <- function(a) .ox_eval(m[[a]], d)
    n <- nrow(d)
    num <- function(v) if (is.null(v)) rep(NA_real_, n) else if (inherits(v, "Date")) as.numeric(v) else if (is.factor(v) || is.character(v)) rep(NA_real_, n) else suppressWarnings(as.numeric(v))
    xx <- g("x"); yy <- g("y")
    if (!is.null(yy) && !is.numeric(yy) && !inherits(yy, "Date") && is.numeric(xx)) { tmp <- xx; xx <- yy; yy <- tmp; horiz <- TRUE }   # horizontal bars
    if (geom %in% c("rect", "tile") && is.null(yy)) next          # regime shading (exported separately below)
    if (geom == "hline" && is.null(yy)) { yy <- g("yintercept") %||% L$aes_params$yintercept; if (length(yy) == 1) yy <- rep(yy, n) }
    if (geom == "vline") { v <- g("xintercept") %||% L$aes_params$xintercept; if (!is.null(v)) rows[[length(rows) + 1]] <- data.table::data.table(panel = "", layer = i, geom = geom, series = "",
                                x = NA_real_, y = NA_real_, ymin = NA_real_, ymax = NA_real_, xline = num(v)[seq_along(v)], colour = NA_character_); next }
    ser <- g("colour") %||% g("fill") %||% g("group") %||% g("linetype")
    ser <- if (is.null(ser)) rep("", n) else { a <- if (!is.null(m$colour)) "colour" else if (!is.null(m$fill)) "fill" else "group"; .ox_relabel(p, a, ser) }
    pan <- if (length(facet_vars)) do.call(paste, c(lapply(facet_vars, function(f) as.character(d[[f]] %||% rep("", n))), sep = " | ")) else rep("", n)
    col <- rep(NA_character_, n)
    if (!is.null(built)) { ld <- built$data[[i]]; cc <- if ("colour" %in% names(ld) && geom %in% c("line", "path", "step", "point", "hline")) ld$colour else ld$fill
      if (!is.null(cc) && length(cc) == n) col <- as.character(cc) }
    xr <- if (is.null(xx)) rep(NA, n) else xx
    rows[[length(rows) + 1]] <- data.table::data.table(panel = pan, layer = i, geom = geom, series = as.character(ser),
      x = if (inherits(xr, "Date")) format(xr) else gsub("\n", " ", as.character(xr)), y = num(yy), ymin = num(g("ymin")), ymax = num(g("ymax")), xline = NA_real_, colour = col)
  }
  if (!length(rows)) return(invisible(NULL))
  out <- data.table::rbindlist(rows, fill = TRUE)
  x_is_date <- any(grepl("^\\d{4}-\\d{2}-\\d{2}$", out$x[!is.na(out$x)]))
  x_is_num <- !x_is_date && all(is.na(suppressWarnings(as.numeric(out$x[!is.na(out$x)]))) == FALSE)
  data.table::fwrite(out, file.path(ORIGIN_DIR, "data", paste0(fig, ".csv")), bom = TRUE)
  # regime shading rectangles (geom_rect layers with an x-range), e.g. add_regimes() in 03
  rect <- NULL
  for (L in p$layers) if (inherits(L$geom, "GeomRect") && is.data.frame(L$data) && nrow(L$data)) {
    xm <- .ox_eval(L$mapping$xmin, L$data); xM <- .ox_eval(L$mapping$xmax, L$data)
    if (is.null(xm) || is.null(xM)) next
    fl <- .ox_eval(L$mapping$fill, L$data); fl <- if (is.null(fl)) rep("", length(xm)) else .ox_relabel(p, "fill", fl)
    rect <- data.table::data.table(xmin = format(as.Date(xm)), xmax = format(as.Date(xM)), regime = as.character(fl)); break }
  if (!is.null(rect)) data.table::fwrite(rect, file.path(ORIGIN_DIR, "data", paste0(fig, "__regimes.csv")), bom = TRUE)
  lb <- p$labels
  meta <- list(fig = fig, png = basename(file), title = .ox_lab(lb$title), subtitle = .ox_lab(lb$subtitle), caption = .ox_lab(lb$caption),
               x_title = .ox_lab(lb$x), y_title = .ox_lab(lb$y), y_percent = .ox_percent(p),
               x_type = if (x_is_date) "date" else if (x_is_num) "numeric" else "category",
               facets = as.list(facet_vars), horizontal = horiz, width_in = w, height_in = h, has_regimes = !is.null(rect),
               geoms = unique(out$geom), created = format(Sys.time(), "%Y-%m-%d %H:%M"))
  con <- file(file.path(ORIGIN_DIR, "meta", paste0(fig, ".json")), open = "w", encoding = "UTF-8")
  writeLines(as.character(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE)), con); close(con)
  invisible(fig)
}
