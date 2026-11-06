# ============================================================================
# utils_figures_academic_v6.R — academic (journal-style) figures for the thesis
# ----------------------------------------------------------------------------
# Sourced at the end of utils_models_v6.R and after the local style block in
# 03/04, so it overrides PAL, theme_thesis() and save_fig() everywhere.
#
# Conventions (as in ECB / BIS / AER figures and Ukrainian thesis standards):
#   * no title, subtitle or source inside the image — they go into the Word
#     caption; collected in figures/figure_captions.csv (Рисунок …, Джерело …);
#   * serif font matching the text (Times New Roman), 10 pt at print size;
#   * print width 16.5 cm (A4 text width), height from the figure's aspect;
#   * L-shaped axes, outward ticks, only faint horizontal grid;
#   * restrained colour-blind-safe palette (prints acceptably in grey);
#   * decimal comma on numeric axes (ДСТУ);
#   * every figure saved as PDF (vector), EMF (vector, for Word), SVG and
#     PNG 600 dpi:  figures/<name>.png, figures/pdf/, figures/emf/, figures/svg/.
# ============================================================================
ACAD_FONT      <- Sys.getenv("THESIS_FIG_FONT", unset = "Times New Roman")
ACAD_BASE_PT   <- 10
ACAD_WIDTH_CM  <- 16.5
ACAD_MAX_H_CM  <- 22
ACAD_DPI       <- 600
ACAD_FORMATS   <- c("png", "pdf", "emf", "svg")
FIG_STYLE      <- Sys.getenv("THESIS_FIG_STYLE", unset = "bw")      # "bw" = black-and-white (tones, line types, hatching); "colour"

.need(c("ragg", "systemfonts"))
.norm <- function(x) gsub("[^a-z]", "", tolower(x))
.font_ok <- function(f) {                         # TRUE if the system resolves f to that family (not to a fallback)
  m <- tryCatch(systemfonts::match_font(f)$path, error = function(e) ""); if (!nzchar(m)) return(FALSE)
  file <- .norm(basename(m)); key <- .norm(f)
  grepl(substr(key, 1, 5), file) || (key == "timesnewroman" && grepl("times|tinos|liberationserif", file))
}
if (!.font_ok(ACAD_FONT)) { alt <- c("Liberation Serif", "Tinos", "DejaVu Serif"); ok <- alt[vapply(alt, .font_ok, logical(1))]; ACAD_FONT <- if (length(ok)) ok[1] else "serif" }

# Palette: near-black, dark blue, vermilion, bluish green, grey, orange, purple, sky blue
# (black is reserved for totals / the main series drawn with INK)
PAL <- c("#1f5fa8", "#c4452d", "#2a8a5c", "#d98c1c", "#6b4c9a", "#5aa0d8", "#8a8a8a", "#1a1a1a")
INK <- "#000000"; INK2 <- "#4d4d4d"; GRID <- "#e3e3e3"

theme_academic <- function(base = ACAD_BASE_PT, family = ACAD_FONT) {
  theme_classic(base_size = base, base_family = family) + theme(
    text = element_text(colour = "black"),
    axis.line = element_line(linewidth = .3, colour = "black"),
    axis.ticks = element_line(linewidth = .3, colour = "black"),
    axis.ticks.length = unit(2.5, "pt"),
    axis.text = element_text(size = base - 1, colour = "black"),
    axis.title = element_text(size = base),
    axis.title.y = element_text(margin = margin(r = 4)),
    panel.grid.major.y = element_line(colour = GRID, linewidth = .25),
    legend.position = "bottom", legend.title = element_blank(),
    legend.text = element_text(size = base - 1), legend.key.width = unit(16, "pt"), legend.key.height = unit(8, "pt"),
    legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(3, "pt"), legend.spacing.y = unit(1, "pt"),
    legend.box = "vertical", legend.box.just = "left", legend.justification = "left",
    legend.background = element_blank(), legend.key = element_blank(),
    strip.background = element_blank(), strip.text = element_text(size = base, hjust = 0, margin = margin(b = 3)),
    panel.spacing = unit(10, "pt"),
    plot.title = element_blank(), plot.subtitle = element_blank(), plot.caption = element_blank(),
    plot.margin = margin(4, 8, 2, 2))
}
theme_thesis <- function() theme_academic()

# decimal comma on continuous (non-date) position scales
.comma_lab <- function(orig) { force(orig); function(br) {
  l <- if (is.function(orig)) orig(br) else format(br, trim = TRUE, scientific = FALSE, drop0trailing = TRUE, big.mark = " ")
  if (is.expression(l)) return(l)
  sub("^-", "\u2212", gsub(".", ",", l, fixed = TRUE))          # decimal comma, typographic minus
} }
.comma_axes <- function(p) {
  b <- tryCatch(ggplot_build(p), error = function(e) NULL); if (is.null(b)) return(p)
  for (a in c("x", "y")) {
    ps <- b$layout[[paste0("panel_scales_", a)]][[1]]
    if (is.null(ps) || !inherits(ps, "ScaleContinuousPosition") || inherits(ps, c("ScaleContinuousDate", "ScaleContinuousDatetime"))) next
    sc <- p$scales$get_scales(a)
    new <- if (!is.null(sc)) { s <- sc$clone(); s$labels <- .comma_lab(sc$labels); s } else
      (if (a == "x") scale_x_continuous else scale_y_continuous)(labels = .comma_lab(NULL))
    p <- suppressMessages(p + new)
  }
  p
}

# wrap long facet labels by editing the facet variable in the data (keeps factor order)
.wrap_facet_values <- function(q, wcm) {
  fv <- tryCatch(names(c(q$facet$params$facets, q$facet$params$rows, q$facet$params$cols)), error = function(e) character())
  if (!length(fv)) return(q)
  b <- tryCatch(ggplot_build(q), error = function(e) NULL)
  ncol <- if (!is.null(b)) max(b$layout$layout$COL) else 2
  width <- max(14, floor((wcm / ncol - 0.6) / (0.19 * ACAD_BASE_PT / 10)))
  w <- function(z) vapply(as.character(z), function(t) paste(strwrap(t, width), collapse = "\n"), "", USE.NAMES = FALSE)
  fix <- function(d) { if (!is.data.frame(d)) return(d); for (v in intersect(fv, names(d))) {
      if (is.factor(d[[v]])) levels(d[[v]]) <- w(levels(d[[v]])) else if (is.character(d[[v]])) d[[v]] <- w(d[[v]]) }; d }
  q$data <- fix(q$data)
  for (i in seq_along(q$layers)) if (is.data.frame(q$layers[[i]]$data)) {
    q$layers[[i]]$data <- fix(q$layers[[i]]$data) }
  q
}

# legend layout: smallest number of rows that fits the print width (overrides nrow set for screen)
.wrap_legend_rows <- function(q, wcm) {
  b <- tryCatch(ggplot_build(q), error = function(e) NULL); if (is.null(b)) return(q)
  for (a in c("colour", "fill")) {
    sc <- b$plot$scales$get_scales(a)
    if (is.null(sc) || identical(sc$guide, "none") || !inherits(sc, "ScaleDiscrete")) next
    g <- tryCatch(q$guides[[a]], error = function(e) NULL)
    if (identical(g, "none") || (is.character(g) && g == "none")) next
    labs <- tryCatch(as.character(sc$get_labels()), error = function(e) character()); n <- length(labs); if (n < 2) next
    cm_per_char <- 0.165 * (ACAD_BASE_PT - 1) / 9
    fits <- function(r) { cw <- tapply(nchar(labs), (seq_len(n) - 1) %/% r, max)      # byrow = FALSE: column k holds items k*r+1…
                          sum(cw * cm_per_char + 0.9) <= wcm - 0.3 }
    r <- 1L; while (r < n && !fits(r)) r <- r + 1L
    gl <- guide_legend(nrow = r, byrow = FALSE)
    q <- q + do.call(guides, setNames(list(gl), a))
    if (a == "colour") for (a2 in c("linetype", "shape")) if (!is.null(b$plot$scales$get_scales(a2))) q <- q + do.call(guides, setNames(list(gl), a2))
  }
  q
}

# long category labels on a discrete x axis: rotate when they cannot fit side by side
.fit_discrete_x <- function(q, wcm) {
  b <- tryCatch(ggplot_build(q), error = function(e) NULL); if (is.null(b)) return(q)
  ps <- b$layout$panel_scales_x[[1]]
  if (is.null(ps) || !inherits(ps, "ScaleDiscretePosition")) return(q)
  labs <- tryCatch(as.character(ps$get_labels()), error = function(e) character()); n <- length(labs); if (!n) return(q)
  lines <- strsplit(labs, "\n"); longest <- max(vapply(lines, function(z) max(nchar(z)), 0))
  npan <- max(1, length(unique(b$layout$layout$COL)))
  if (longest * 0.15 * n > (wcm - 1.5) / npan) q <- q + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  q
}

.record_caption <- function(f, p) {
  lb <- p$labels; nz <- function(x) if (is.null(x) || inherits(x, "waiver")) "" else paste(as.character(x), collapse = " ")
  path <- file.path(FIG, "figure_captions.csv")
  row <- data.table::data.table(file = sub("\\.[A-Za-z]+$", "", f), caption = nz(lb$title), note = nz(lb$subtitle),
                                source = sub("^Джерело:\\s*", "", nz(lb$caption)), y_axis = nz(lb$y), updated = format(Sys.time(), "%Y-%m-%d %H:%M"))
  old <- if (file.exists(path)) tryCatch(data.table::fread(path, encoding = "UTF-8"), error = function(e) NULL) else NULL
  out <- data.table::rbindlist(list(if (!is.null(old)) old[file != row$file], row), fill = TRUE)
  data.table::setorder(out, file); data.table::fwrite(out, path, bom = TRUE)
}

source(file.path(PROJECT_DIR, "scripts", "math_support.R"), encoding = "UTF-8")

save_fig <- function(p, f, w = 8.5, h = 4.8) {
  name <- sub("\\.[A-Za-z]+$", "", f)
  tryCatch(.record_caption(f, p), error = function(e) NULL)
  tryCatch(if (exists("origin_export")) origin_export(p, f, w, h), error = function(e) NULL)   # plotted data as CSV (any software)
  q <- .comma_axes(p + theme_academic() + labs(title = NULL, subtitle = NULL, caption = NULL))
  q <- .wrap_facet_values(q, wcm = ACAD_WIDTH_CM)                                             # long panel titles wrap
  wcm <- ACAD_WIDTH_CM; hcm <- min(ACAD_WIDTH_CM * h / w, ACAD_MAX_H_CM)
  if (FIG_STYLE == "bw") q <- .to_bw(q)
  q <- .wrap_legend_rows(q, wcm = ACAD_WIDTH_CM)
  q <- .fit_discrete_x(q, wcm = ACAD_WIDTH_CM)
  q <- .math_plot(q, name)
  if (FIG_STYLE == "bw") q <- tryCatch(.hatch_overlay(q, wcm, hcm), error = function(e) { log_msg("hatching skipped for ", name, ": ", conditionMessage(e)); q })
  for (d in c("pdf", "emf", "svg")) dir.create(file.path(FIG, d), showWarnings = FALSE)
  if ("png" %in% ACAD_FORMATS)
    ggsave(file.path(FIG, paste0(name, ".png")), q, device = ragg::agg_png, width = wcm, height = hcm, units = "cm", res = ACAD_DPI, bg = "white")
  if ("pdf" %in% ACAD_FORMATS)
    tryCatch(ggsave(file.path(FIG, "pdf", paste0(name, ".pdf")), q, device = grDevices::cairo_pdf, width = wcm, height = hcm, units = "cm", family = ACAD_FONT),
             error = function(e) log_msg("PDF skipped for ", name, ": ", conditionMessage(e)))
  if ("svg" %in% ACAD_FORMATS && requireNamespace("svglite", quietly = TRUE))
    tryCatch(ggsave(file.path(FIG, "svg", paste0(name, ".svg")), q, device = svglite::svglite, width = wcm, height = hcm, units = "cm"),
             error = function(e) log_msg("SVG skipped for ", name, ": ", conditionMessage(e)))
  if ("emf" %in% ACAD_FORMATS) {
    if (!requireNamespace("devEMF", quietly = TRUE) && isTRUE(AUTO_INSTALL) && !isTRUE(getOption("thesis.emf_tried"))) {
      options(thesis.emf_tried = TRUE); try(install.packages("devEMF", repos = "https://cloud.r-project.org"), silent = TRUE) }
    if (requireNamespace("devEMF", quietly = TRUE))
      tryCatch(ggsave(file.path(FIG, "emf", paste0(name, ".emf")), q, device = function(filename, ...) devEMF::emf(filename, ..., emfPlus = TRUE),
                      width = wcm / 2.54, height = hcm / 2.54, units = "in"),
               error = function(e) log_msg("EMF skipped for ", name, ": ", conditionMessage(e)))
  }
  invisible(q)
}

source(file.path(PROJECT_DIR, "utils_figures_bw_v6.R"), encoding = "UTF-8", local = environment())   # black-and-white mode
