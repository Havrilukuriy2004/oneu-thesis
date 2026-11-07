# ============================================================================
# utils_figures_bw_v6.R — strictly black-and-white figures
# ----------------------------------------------------------------------------
# Applied by save_fig() when FIG_STYLE = "bw" (default). For every figure:
#   * lines:  grey tones + line types (+ sparse markers when ≥ 3 series),
#             one legend combining tone, line type and marker;
#   * points: grey tones + marker shapes;
#   * bars and stacked areas: grey tones + hatching patterns (45°, −45°, cross,
#             horizontal, vertical, dots), drawn as plain line segments — no
#             extra packages, works in PNG, PDF, SVG and EMF (Word);
#   * legend keys show the same hatching;
#   * any remaining coloured constant (single series, reference lines) -> black/grey.
# ============================================================================
BW_LINE_COL <- c("black", "grey45", "black", "grey45", "black", "grey60", "grey30", "grey60")
BW_LT       <- c("solid", "22", "4212", "13", "42", "solid", "1343", "73")
BW_SHAPE    <- c(NA, NA, 16, 1, 17, 2, 15, 0)           # first two series: tone + line type only
BW_SHAPE_PT <- c(16, 1, 17, 2, 15, 0, 4, 3)              # scatter plots
# bars / areas: alternate solid tones and hatched white (fill strings must be unique: they key the pattern)
BW_FILL     <- c("grey35", "white", "grey80", "#FFFFFF", "grey60", "#FEFEFE", "grey15", "#FDFDFD")
BW_PATTERN  <- c("none",   "45",    "none",   "cross",   "none",   "-45",     "none",   "horiz")
BW_REGIME   <- c("grey88", "grey95", "grey80", "white")
.HATCH <- new.env()

.is_grey <- function(x) { x <- as.character(x); ok <- !is.na(x) & x != "NA"
  r <- rep(TRUE, length(x)); if (any(ok)) { m <- grDevices::col2rgb(x[ok]); r[ok] <- (apply(m, 2, max) - apply(m, 2, min)) < 12 }; r }
.to_grey <- function(x, dark = "black") ifelse(.is_grey(x), x, dark)

# ---------- hatch geometry (physical cm space, then back to data space) -------
.hatch_lines <- function(pattern, W, H, s = 0.16) {
  # returns list of lines as (x0,y0,x1,y1) across the full panel [0,W]x[0,H]
  L <- list()
  add <- function(x0, y0, x1, y1) L[[length(L) + 1]] <<- c(x0, y0, x1, y1)
  if (pattern %in% c("45", "cross")) for (c in seq(-W, H, by = s * sqrt(2))) { a <- max(0, -c); b <- min(W, H - c); if (b > a) add(a, a + c, b, b + c) }
  if (pattern %in% c("-45", "cross")) for (c in seq(0, W + H, by = s * sqrt(2))) { a <- max(0, c - H); b <- min(W, c); if (b > a) add(a, c - a, b, c - b) }
  if (pattern == "horiz") for (y in seq(s / 2, H, by = s)) add(0, y, W, y)
  if (pattern == "vert")  for (x in seq(s / 2, W, by = s)) add(x, 0, x, H)
  if (!length(L)) return(NULL)
  do.call(rbind, L)
}
# clip each line to an "inside" predicate by dense sampling, return segments
.clip_lines <- function(lines, inside, W, H, n = 400) {
  if (is.null(lines)) return(NULL)
  out <- list()
  for (k in seq_len(nrow(lines))) {
    l <- lines[k, ]; t <- seq(0, 1, length.out = n); x <- l[1] + t * (l[3] - l[1]); y <- l[2] + t * (l[4] - l[2])
    ins <- inside(x, y); if (!any(ins)) next
    r <- rle(ins); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
    for (j in which(r$values)) if (ends[j] > starts[j]) out[[length(out) + 1]] <- c(x[starts[j]], y[starts[j]], x[ends[j]], y[ends[j]])
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

.hatch_overlay <- function(q, wcm, hcm) {
  b <- tryCatch(ggplot_build(q), error = function(e) NULL); if (is.null(b)) return(q)
  lay <- b$layout$layout; ncol <- max(lay$COL); nrow <- max(lay$ROW)
  W <- max(2, (wcm - 2.6) / ncol); H <- max(2, (hcm - 2.2) / nrow)            # approx. physical panel size, cm
  pmap <- .HATCH$map; if (is.null(pmap) || !length(pmap)) return(q)
  segs <- list()
  for (i in seq_along(q$layers)) {
    geom <- class(q$layers[[i]]$geom)[1]
    if (!geom %in% c("GeomCol", "GeomBar", "GeomArea", "GeomRect")) next
    ld <- b$data[[i]]; if (!"fill" %in% names(ld)) next
    if (geom == "GeomRect" && any(!is.finite(ld$ymin))) next                   # regime bands: tones only
    for (pn in unique(ld$PANEL)) {
      pp <- b$layout$panel_params[[as.integer(pn)]]
      xr <- pp$x.range %||% pp$x$continuous_range; yr <- pp$y.range %||% pp$y$continuous_range
      if (is.null(xr) || is.null(yr)) next
      tox <- function(x) (x - xr[1]) / diff(xr) * W; toy <- function(y) (y - yr[1]) / diff(yr) * H
      frx <- function(u) xr[1] + u / W * diff(xr); fry <- function(v) yr[1] + v / H * diff(yr)
      d <- ld[ld$PANEL == pn, , drop = FALSE]
      for (f in unique(d$fill)) {
        pat <- pmap[[as.character(f)]] %||% "none"; if (pat == "none") next
        dd <- d[d$fill == f, , drop = FALSE]
        lines <- .hatch_lines(pat, W, H)
        if (geom == "GeomArea") {
          dd <- dd[order(dd$x), ]
          inside <- function(u, v) { x <- frx(u); y <- fry(v)
            lo <- stats::approx(dd$x, dd$ymin, x, rule = 1)$y; hi <- stats::approx(dd$x, dd$ymax, x, rule = 1)$y
            !is.na(lo) & y > lo & y < hi }
          sg <- .clip_lines(lines, inside, W, H)
        } else {
          rx0 <- tox(pmin(dd$xmin, dd$xmax)); rx1 <- tox(pmax(dd$xmin, dd$xmax)); ry0 <- toy(pmin(dd$ymin, dd$ymax)); ry1 <- toy(pmax(dd$ymin, dd$ymax))
          keep <- (rx1 - rx0) > 0.12 & (ry1 - ry0) > 0.05                         # too thin for hatching -> tone only
          if (!any(keep)) next
          rx0 <- rx0[keep]; rx1 <- rx1[keep]; ry0 <- ry0[keep]; ry1 <- ry1[keep]
          inside <- function(u, v) { r <- rep(FALSE, length(u)); for (k in seq_along(rx0)) r <- r | (u > rx0[k] & u < rx1[k] & v > ry0[k] & v < ry1[k]); r }
          sg <- .clip_lines(lines, inside, W, H, n = 700)
        }
        if (is.null(sg)) next
        segs[[length(segs) + 1]] <- data.frame(PANEL = pn, x = frx(sg[, 1]), y = fry(sg[, 2]), xend = frx(sg[, 3]), yend = fry(sg[, 4]))
      }
    }
  }
  if (!length(segs)) return(q)
  S <- do.call(rbind, segs)
  fv <- setdiff(names(lay), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
  if (length(fv)) S <- merge(S, lay[, c("PANEL", fv), drop = FALSE], by = "PANEL")
  S$PANEL <- NULL
  if (inherits(b$layout$panel_scales_x[[1]], "ScaleContinuousDate")) { S$x <- as.Date(S$x, origin = "1970-01-01"); S$xend <- as.Date(S$xend, origin = "1970-01-01") }
  if (inherits(b$layout$panel_scales_y[[1]], "ScaleContinuousDate")) { S$y <- as.Date(S$y, origin = "1970-01-01"); S$yend <- as.Date(S$yend, origin = "1970-01-01") }
  q + geom_segment(data = S, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, linewidth = .18, colour = "black", lineend = "butt")
}

# legend key with the same hatching
draw_key_hatch <- function(data, params, size) {
  f <- data$fill %||% "grey70"; pat <- .HATCH$map[[as.character(f)]] %||% "none"
  g <- list(grid::rectGrob(gp = grid::gpar(fill = f, col = "black", lwd = .5)))
  l <- .hatch_lines(pat, 1, 1, s = .28)
  if (!is.null(l)) g[[2]] <- grid::segmentsGrob(l[, 1], l[, 2], l[, 3], l[, 4], default.units = "npc", gp = grid::gpar(col = "black", lwd = .5))
  grid::gTree(children = do.call(grid::gList, g))
}

# ggproto() resolves its parent lazily from the calling frame: build the child inside its own function
.with_hatch_key <- function(parent_geom) { force(parent_geom); ggproto(NULL, parent_geom, draw_key = draw_key_hatch) }

.layer_q <- function(q, L, a) { m <- L$mapping[[a]]; if (is.null(m) && isTRUE(L$inherit.aes)) m <- q$mapping[[a]]; m }

.to_bw <- function(q) {
  b <- tryCatch(ggplot_build(q), error = function(e) NULL); if (is.null(b)) return(q)
  .HATCH$map <- list()
  same_mapping <- function(a, b) identical(rlang::get_expr(a), rlang::get_expr(b))
  line_geoms <- c("GeomLine", "GeomPath", "GeomStep", "GeomSmooth")
  independent <- function(aesthetic, geoms = NULL) any(vapply(q$layers, function(L) {
    if (!is.null(geoms) && !class(L$geom)[1] %in% geoms) return(FALSE)
    colour <- .layer_q(q, L, "colour"); other <- .layer_q(q, L, aesthetic)
    !is.null(colour) && !is.null(other) && !same_mapping(colour, other)
  }, logical(1)))
  separate_linetype <- independent("linetype", line_geoms)
  separate_shape <- independent("shape")
  has_points <- any(vapply(q$layers, function(L) inherits(L$geom, "GeomPoint") &&
    !is.null(.layer_q(q, L, "colour")), logical(1)))
  # ---- colour: tones + line types + markers, merged legend ------------------
  csc <- b$plot$scales$get_scales("colour")
  if (!is.null(csc) && inherits(csc, "ScaleDiscrete")) {
    br <- csc$get_breaks(); br <- br[!is.na(br)]; lb <- csc$get_labels(br); n <- length(br)
    if (n) {
      nm <- as.character(br)
      # Independent aesthetics have different levels, e.g. shock size and model.
      # Applying shock breaks to a model linetype scale makes every line NA.
      tones <- if (separate_linetype) grDevices::grey.colors(n, start = .05, end = .65) else rep_len(BW_LINE_COL, n)
      q <- suppressMessages(q + scale_colour_manual(values = setNames(tones, nm), breaks = br, labels = lb, name = NULL))
      if (!separate_linetype) q <- suppressMessages(q + scale_linetype_manual(values = setNames(rep_len(BW_LT, n), nm), breaks = br, labels = lb, name = NULL))
      point_shapes <- if (has_points) BW_SHAPE_PT else if (n >= 3) BW_SHAPE else rep(NA, 8)
      if (!separate_shape) q <- suppressMessages(q + scale_shape_manual(values = setNames(rep_len(point_shapes, n), nm), breaks = br, labels = lb, name = NULL, na.value = NA))
      pts_scatter <- FALSE
      for (i in seq_along(q$layers)) {
        L <- q$layers[[i]]; g <- class(L$geom)[1]; cq <- .layer_q(q, L, "colour"); if (is.null(cq)) next
        if (!separate_linetype && g %in% line_geoms && is.null(.layer_q(q, L, "linetype"))) {
          q$layers[[i]]$mapping$linetype <- cq
          if (n >= 3) {                                                         # sparse markers on each series
            d <- if (is.data.frame(L$data)) L$data else q$data
            xq <- .layer_q(q, L, "x"); yq <- .layer_q(q, L, "y")
            gx <- tryCatch(rlang::eval_tidy(xq, d), error = function(e) NULL); gy <- tryCatch(rlang::eval_tidy(yq, d), error = function(e) NULL)
            gg <- tryCatch(rlang::eval_tidy(cq, d), error = function(e) NULL)
            if (!is.null(gx) && !is.null(gy) && !is.null(gg) && length(gx) == nrow(d)) {
              fv <- tryCatch(names(c(q$facet$params$facets, q$facet$params$rows, q$facet$params$cols)), error = function(e) character())
              md <- data.frame(.x = gx, .y = gy, .g = gg); for (v in intersect(fv, names(d))) md[[v]] <- d[[v]]
              md <- md[is.finite(as.numeric(md$.y)), , drop = FALSE]
              grpkey <- do.call(paste, c(list(md$.g), lapply(intersect(fv, names(md)), function(v) md[[v]])))
              lev <- unique(as.character(md$.g))
              md <- do.call(rbind, lapply(split(md, grpkey), function(z) { z <- z[order(z$.x), ]; k <- max(1L, floor(nrow(z) / 9))
                off <- (match(as.character(z$.g[1]), lev) - 1L) %% k; z[seq(1 + off, nrow(z), by = k), , drop = FALSE] }))
              if (!is.null(md) && nrow(md)) {
                # NA shapes intentionally omit markers on the first two lines.
                # Remove those marker rows instead of emitting missing-value warnings.
                shape_values <- setNames(rep_len(point_shapes, n), nm)
                md <- md[!is.na(shape_values[as.character(md$.g)]), , drop = FALSE]
                if (nrow(md)) q <- q + geom_point(data = md, aes(x = .x, y = .y, shape = .g, colour = .g), inherit.aes = FALSE, size = 1.3, stroke = .45, show.legend = NA)
              }
            }
          }
        }
        if (!separate_shape && g %in% c("GeomPoint") && is.null(.layer_q(q, L, "shape"))) { q$layers[[i]]$mapping$shape <- cq; pts_scatter <- TRUE }
      }
      if (pts_scatter) q <- suppressMessages(q + scale_shape_manual(values = setNames(rep_len(BW_SHAPE_PT, n), nm), breaks = br, labels = lb, name = NULL))
    }
  } else if (!is.null(csc) && inherits(csc, "ScaleContinuous")) q <- suppressMessages(q + scale_colour_gradient(low = "grey85", high = "black"))
  # ---- fill: tones (+ hatching for bars/areas) --------------------------------
  fsc <- b$plot$scales$get_scales("fill")
  if (!is.null(fsc) && inherits(fsc, "ScaleDiscrete")) {
    br <- fsc$get_breaks(); br <- br[!is.na(br)]; lb <- fsc$get_labels(br); n <- length(br); nm <- as.character(br)
    geoms <- vapply(q$layers, function(L) class(L$geom)[1], "")
    filled <- vapply(seq_along(q$layers), function(i) !is.null(.layer_q(q, q$layers[[i]], "fill")), logical(1))
    regime_only <- all(geoms[filled] %in% c("GeomRect", "GeomRibbon"))
    vals <- if (regime_only) rep_len(BW_REGIME, n) else rep_len(BW_FILL, n)
    if (n) q <- suppressMessages(q + scale_fill_manual(values = setNames(vals, nm), breaks = br, labels = lb, name = NULL))
    if (!regime_only) {
      pats <- rep_len(BW_PATTERN, n); .HATCH$map <- setNames(as.list(pats), vals)
      for (i in which(filled & geoms %in% c("GeomCol", "GeomBar", "GeomArea"))) {
        q$layers[[i]]$aes_params$colour <- "black"; q$layers[[i]]$aes_params$linewidth <- .2
        q$layers[[i]]$geom <- .with_hatch_key(q$layers[[i]]$geom)
      }
    }
  }
  # ---- constants: any coloured literal -> black / grey -------------------------
  for (i in seq_along(q$layers)) {
    ap <- q$layers[[i]]$aes_params
    if (!is.null(ap$colour)) q$layers[[i]]$aes_params$colour <- .to_grey(ap$colour)
    if (!is.null(ap$fill) && !.is_grey(ap$fill)) q$layers[[i]]$aes_params$fill <- "grey80"
  }
  q
}
