# ============================================================================
# 08_thesis_tables_v6.R — ТАБЛИЦІ ДЛЯ ТЕКСТУ ДИСЕРТАЦІЇ (Word)
# ----------------------------------------------------------------------------
# Reads ONLY the CSV results of 03–07 (output_nbu/section_2_1_v2/tables and
# output_nbu/section_2_2/tables) and writes ready-to-paste academic tables:
#   output_nbu/thesis_tables/Tables_Section_2.docx
#   output_nbu/thesis_tables/Tables_Section_2_index.csv   (number, title, source CSV)
# Format (ДСТУ 3008 / вимоги до магістерських робіт):
#   * "Таблиця 2.N" праворуч, назва по центру над таблицею;
#   * Times New Roman, 12 pt текст, 10 pt (широкі — 9/8 pt) таблиці;
#   * десяткова кома, знак мінус "−";
#   * коефіцієнти з зірочками (* p < 0,10; ** p < 0,05; *** p < 0,01),
#     стандартні похибки в дужках під коефіцієнтом;
#   * рядки N / банків / ефекти / тип с.п. у регресійних таблицях;
#   * примітки та джерело під таблицею;
#   * широкі таблиці — на альбомних сторінках.
# A table whose source CSV is missing is skipped (listed in the index).
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL) %||%
  Sys.getenv("THESIS_CODE_DIR", unset = getwd())
source(file.path(.this_dir, "00_config_v6.R"), encoding = "UTF-8")
for (p in c("data.table", "officer", "flextable")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({ library(data.table); library(officer); library(flextable) })

T21 <- file.path(OUTPUT_DIR, "section_2_1_v2", "tables")
T22 <- file.path(OUTPUT_DIR, "section_2_2", "tables")
TEX <- file.path(OUTPUT_DIR, "expectations_money", "tables")
T23 <- file.path(OUTPUT_DIR, "section_2_3", "tables")
T24 <- file.path(OUTPUT_DIR, "section_2_4", "tables")
TOU <- file.path(OUTPUT_DIR, "ou_dynamics", "tables")
OUT <- file.path(OUTPUT_DIR, "thesis_tables"); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
FONT <- Sys.getenv("THESIS_FIG_FONT", unset = "Times New Roman")
SRC_NOTE <- "Джерело: розраховано автором за даними НБУ (OpenData, фінансова звітність банків, статистика процентних ставок, грошова статистика, опитування очікувань і BLS, UIRD)."

# ------------------------------ formatting -----------------------------------
fnum <- function(x, d = 3) {
  x <- suppressWarnings(as.numeric(x)); s <- formatC(round(x, d), format = "f", digits = d, big.mark = " ")
  s <- gsub(".", ",", s, fixed = TRUE); s <- sub("^-", "−", s); s[!is.finite(x)] <- ""
  s[s %in% c("−0", paste0("−0,", strrep("0", d)))] <- if (d == 0) "0" else paste0("0,", strrep("0", d)); s }
fpct <- function(x, d = 2) fnum(100 * as.numeric(x), d)                 # share -> % / п.п.
fint <- function(x) fnum(x, 0)
pval <- function(b, se) 2 * pnorm(-abs(b / se))
star <- function(p) { p <- suppressWarnings(as.numeric(p)); out <- fifelse(p < .01, "***", fifelse(p < .05, "**", fifelse(p < .1, "*", "")), na = ""); out }
fb   <- function(b, se = NULL, p = NULL, d = 3) { if (is.null(p) && !is.null(se)) p <- pval(b, se); paste0(fnum(b, d), if (!is.null(p)) star(p) else "") }
fse  <- function(se, d = 3) ifelse(is.finite(as.numeric(se)), paste0("(", fnum(se, d), ")"), "")
fp   <- function(p) ifelse(is.finite(as.numeric(p)), fnum(p, 3), "")
fdate <- function(x) format(as.Date(x), "%m.%Y")
REG_UA <- c("Tightening" = "Посилення", "Easing" = "Пом'якшення", "High-rate plateau" = "Плато високої ставки",
            "Stable/low-rate" = "Стабільна / низька ставка")
reg_ua <- function(x) { y <- REG_UA[as.character(x)]; ifelse(is.na(y), as.character(x), y) }
STAR_NOTE <- "* p < 0,10; ** p < 0,05; *** p < 0,01; у дужках — стандартні похибки."

# two-row cells (coefficient, s.e. under it): wide table with a row pair per entity
stack_bse <- function(D, id, cols_b, cols_se, d = 3, id_names = id) {
  rows <- lapply(seq_len(nrow(D)), function(i) {
    r1 <- c(as.list(D[i, ..id]), setNames(lapply(seq_along(cols_b), function(j) fb(D[[cols_b[j]]][i], D[[cols_se[j]]][i], d = d)), cols_b))
    r2 <- c(setNames(as.list(rep("", length(id))), id), setNames(lapply(cols_se, function(c) fse(D[[c]][i], d)), cols_b))
    rbind(as.data.table(r1), as.data.table(r2)) })
  out <- rbindlist(rows); setnames(out, id, id_names); out }

csv <- function(dir, f) { p <- file.path(dir, paste0(f, ".csv")); if (!file.exists(p)) stop("немає файлу ", basename(p)); fread(p, encoding = "UTF-8") }

# ------------------------------ document -------------------------------------
tp  <- function(size = 12, bold = FALSE, italic = FALSE) fp_text(font.family = FONT, font.size = size, bold = bold, italic = italic)
pp  <- function(align = "left", before = 0, after = 0, indent = 0) fp_par(text.align = align, padding.top = before, padding.bottom = after, line_spacing = 1)
ps_port <- prop_section(page_size = page_size(width = 21 / 2.54, height = 29.7 / 2.54, orient = "portrait"),
                        page_margins = page_mar(top = 2 / 2.54, bottom = 2 / 2.54, left = 2.5 / 2.54, right = 1.5 / 2.54, header = .5, footer = .5, gutter = 0),
                        type = "nextPage")
ps_land <- prop_section(page_size = page_size(width = 21 / 2.54, height = 29.7 / 2.54, orient = "landscape"),
                        page_margins = page_mar(top = 2.5 / 2.54, bottom = 1.5 / 2.54, left = 2 / 2.54, right = 2 / 2.54, header = .5, footer = .5, gutter = 0),
                        type = "nextPage")
W_PORT <- (21 - 2.5 - 1.5) / 2.54; W_LAND <- (29.7 - 2 - 2) / 2.54

doc <- read_docx()
doc <- body_set_default_section(doc, ps_port)
# Fast assembly: officer/xml2 make every body_add_* cost O(document size) (cursor XPath and
# namespace reconciliation), i.e. minutes for ~50 tables. Instead every block is kept as a
# WordprocessingML string and the whole <w:body> is replaced ONCE at the end (finalize_doc).
BLOCKS <- character()
put_xml <- function(str) { BLOCKS[length(BLOCKS) + 1L] <<- str; invisible(TRUE) }
finalize_doc <- function() {
  root <- doc$doc_obj$get(); body <- xml2::xml_find_first(root, "/w:document/w:body")
  sp <- as.character(xml2::xml_find_first(body, "./w:sectPr"))
  ns <- xml2::xml_ns(root); ns <- ns[!duplicated(names(ns)) & !grepl("^d[0-9]+$", names(ns))]
  decl <- paste0("xmlns:", names(ns), "=\"", unname(ns), "\"", collapse = " ")
  newb <- xml2::read_xml(paste0("<w:body ", decl, ">", paste(BLOCKS, collapse = ""), sp, "</w:body>"))
  xml2::xml_replace(body, newb); invisible(TRUE) }
put_fpar <- function(fp) put_xml(to_wml(fp, add_ns = FALSE))
put_ft <- function(ft) { ft$properties$align <- "center"; put_xml(flextable:::gen_raw_wml(ft, doc = doc)) }
N_TAB <- 0L; INDEX <- list(); CUR_LAND <- FALSE

heading <- function(txt, size = 14) {
  put_fpar(fpar(ftext(txt, tp(size, bold = TRUE)), fp_p = pp("left", before = 12, after = 6))) }
para <- function(txt, size = 12, italic = FALSE) {
  put_fpar(fpar(ftext(txt, tp(size, italic = italic)), fp_p = pp("justify", after = 4, indent = 1.25 / 2.54 * 72 / 72))) }

set_orient <- function(land) {
  if (land == CUR_LAND) return(invisible())
  # a section break closes the preceding content with the CURRENT orientation
  put_xml(to_wml(block_section(if (CUR_LAND) ps_land else ps_port), add_ns = FALSE))
  CUR_LAND <<- land }

# df: data.frame of character columns. header: optional 2-level header (named vector col -> top label)
add_table <- function(df, title, source, note = NULL, land = FALSE, size = 10, first_w = NULL,
                      header2 = NULL, bold_rows = NULL, star_note = TRUE) {
  df <- as.data.frame(df, stringsAsFactors = FALSE); df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  set_orient(land)
  N_TAB <<- N_TAB + 1L; num <- sprintf("Таблиця 2.%d", N_TAB)
  INDEX[[length(INDEX) + 1L]] <<- data.table(number = num, title = title, source_csv = source, rows = nrow(df), status = "ok")
  put_fpar(fpar(ftext(num, tp(12, italic = TRUE)), fp_p = pp("right", before = 10)))
  put_fpar(fpar(ftext(title, tp(12, bold = TRUE)), fp_p = pp("center", after = 4)))
  ft <- flextable(df)
  if (!is.null(header2)) {
    top <- unname(header2[names(df)]); top[is.na(top)] <- names(df)[is.na(top)]
    ft <- add_header_row(ft, values = rle(top)$values, colwidths = rle(top)$lengths)
    ft <- merge_v(ft, part = "header")
  }
  ft <- border_remove(ft)
  thick <- fp_border(color = "black", width = 1.25); thin <- fp_border(color = "black", width = .5)
  ft <- hline_top(ft, border = thick, part = "header"); ft <- hline_bottom(ft, border = thin, part = "header")
  if (!is.null(header2)) ft <- hline(ft, i = 1, border = thin, part = "header")
  ft <- hline_bottom(ft, border = thick, part = "body")
  ft <- font(ft, fontname = FONT, part = "all"); ft <- fontsize(ft, size = size, part = "all")
  ft <- align(ft, align = "center", part = "all"); ft <- align(ft, j = 1, align = "left", part = "body")
  ft <- valign(ft, valign = "center", part = "header")
  ft <- padding(ft, padding.top = 1, padding.bottom = 1, padding.left = 3, padding.right = 3, part = "all")
  ft <- line_spacing(ft, space = 1, part = "all")
  if (!is.null(bold_rows) && length(bold_rows)) ft <- bold(ft, i = bold_rows, part = "body")
  W <- if (land) W_LAND else W_PORT
  # column widths proportional to content: longest cell (capped) vs longest header word
  nc <- ncol(df)
  need <- vapply(seq_len(nc), function(j) {
    cell <- max(c(nchar(df[[j]]), 1)); hw <- max(nchar(strsplit(names(df)[j], "[ /–-]+")[[1]]), 1)
    max(min(cell, if (j == 1) 38 else 16), hw, 6) }, numeric(1))
  wj <- need / sum(need) * W
  if (!is.null(first_w)) { wj[1] <- max(wj[1], first_w); if (nc > 1) wj[-1] <- wj[-1] / sum(wj[-1]) * (W - wj[1]) }
  ft <- width(ft, j = seq_len(nc), width = wj)
  ft <- set_table_properties(ft, layout = "fixed")
  put_ft(ft)
  notes <- c(if (star_note) STAR_NOTE, note)
  if (length(notes)) put_fpar(fpar(ftext("Примітка. ", tp(10, italic = TRUE)), ftext(paste(notes, collapse = " "), tp(10)), fp_p = pp("justify", before = 3)))
  put_fpar(fpar(ftext(SRC_NOTE, tp(10)), fp_p = pp("justify", after = 8)))
  invisible(TRUE) }

safe <- function(name, expr) {
  t0 <- Sys.time(); on.exit(message(sprintf("  %-32s %.1f с", name, as.numeric(difftime(Sys.time(), t0, units = "secs")))), add = TRUE)
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) {
    message("  [пропущено] ", name, ": ", conditionMessage(e))
    INDEX[[length(INDEX) + 1L]] <<- data.table(number = "—", title = name, source_csv = "", rows = 0L, status = paste("пропущено:", conditionMessage(e)))
    FALSE })
  invisible(ok) }

# title page
put_fpar(fpar(ftext("Таблиці до розділу 2", tp(16, bold = TRUE)), fp_p = pp("center", before = 0, after = 6)))
para(sprintf("Сформовано автоматично скриптом 08_thesis_tables_v6.R (%s) з результатів 03–07. Номери таблиць наскрізні в межах розділу 2; при перенесенні в текст їх можна перенумерувати. Ставки та частки — у відсотках (%%), зміни ставок — у процентних пунктах (п. п.); коефіцієнти передачі — п. п. на 1 п. п. облікової ставки.", format(Sys.time(), "%d.%m.%Y %H:%M")), size = 11, italic = TRUE)

# ============================================================================
# 2.1 ВАРТІСТЬ ФОНДУВАННЯ ТА СТРУКТУРА АКТИВІВ
# ============================================================================
heading("2.1. Вартість фондування, структура пасивів і активів банків України")

safe("T20 режими", {
  d <- csv(T21, "T20_regime_summary")[regime != "" & !is.na(regime)]
  out <- d[, .(`Режим монетарної політики` = reg_ua(regime), `Місяців` = fint(months),
               `Облікова ставка, %` = fpct(policy_avg), `Вартість депозитів, %` = fpct(cod4_total),
               `Частка коштів на вимогу, %` = fpct(demand_share), `Валютна частка, %` = fpct(fx_share),
               `ДС НБУ / депозити, %` = fpct(cd_to_dep), `ОВДП / депозити, %` = fpct(ovdp_to_dep),
               `Δ вартості, п. п.` = fpct(delta), `ціновий ефект` = fpct(price), `ефект структури` = fpct(mix))]
  add_table(out, "Вартість і структура депозитного фондування за режимами монетарної політики (середні за режим), 2018–2026 рр.",
            "T20_regime_summary.csv", note = "Δ вартості — сумарна зміна вартості депозитів за місяці режиму, розкладена на ціновий ефект і ефект структури (зсув-частка).",
            star_note = FALSE, land = TRUE, size = 9)
})

safe("T06 ціна/структура за циклами", {
  d <- csv(T21, "T06_price_mix_by_cycle_4products")
  out <- d[, .(`Цикл` = fint(cycle), `Режим` = reg_ua(regime), `Період` = paste0(fdate(start), "–", fdate(end)), `Місяців` = fint(months),
               `Δ вартості, п. п.` = fpct(delta), `Ціновий ефект, п. п.` = fpct(price), `Ефект структури, п. п.` = fpct(mix),
               `Частка структури, %` = fpct(mix_share, 1))]
  add_table(out, "Декомпозиція зміни вартості депозитів (4 продукти) на ціновий ефект і ефект структури за циклами облікової ставки",
            "T06_price_mix_by_cycle_4products.csv", note = "Чотири продукти: кошти на вимогу та строкові депозити фізичних осіб і суб'єктів господарювання. Частка структури = ефект структури / Δ вартості.",
            star_note = FALSE, size = 9, first_w = .6)
})

safe("T10 коефіцієнти циклу", {
  d <- csv(T21, "T10_cycle_ratios_descriptive")[!is.na(regime) & regime != ""]
  out <- d[, .(`Цикл` = fint(cycle), `Режим` = reg_ua(regime), `Період` = paste0(fdate(start), "–", fdate(end)), `Місяців` = fint(months),
               `Облікова на початку, %` = fpct(policy_start), `Облікова в кінці, %` = fpct(policy_end), `Δ облікової, п. п.` = fpct(d_policy),
               `Δ вартості депозитів, п. п.` = fpct(d_cod), `Коефіцієнт циклу` = fnum(cycle_ratio, 2))]
  add_table(out, "Коефіцієнти передачі за циклами облікової ставки (описові)", "T10_cycle_ratios_descriptive.csv",
            note = "Коефіцієнт циклу = Δ вартості депозитів / Δ облікової ставки; наводиться лише для циклів зі зміною облікової ставки, достатньою для інтерпретації.",
            star_note = FALSE, size = 9, first_w = .6)
})

safe("T09b орієнтири", {
  d <- csv(T21, "T09b_benchmarks_by_regime")[regime != "" & !is.na(regime)]
  for (v in c("uonia", "ovdp_primary", "wedge_uonia")) if (!v %in% names(d)) d[, (v) := NA_real_]
  out <- d[, .(`Режим` = reg_ua(regime), `Вартість депозитів` = fpct(cod4), `Облікова ставка` = fpct(policy), `ДС овернайт` = fpct(cd_on),
               `UONIA` = fpct(uonia), `ОВДП (первинний ринок)` = fpct(ovdp_primary),
               `ДС (книжкова)` = fpct(cd_book), `ОВДП (книжкова)` = fpct(refin_book), `ОВДП 1 рік (NSS)` = fpct(zc_1y),
               `до облікової` = fpct(wedge_policy), `до ДС овернайт` = fpct(wedge_cd_on), `до ОВДП 1 рік` = fpct(wedge_zc_1y),
               `ДС` = fpct(carry_cd), `ОВДП` = fpct(carry_refin))]
  h2 <- c(`Режим` = "Режим", `Вартість депозитів` = "Ставки, %", `Облікова ставка` = "Ставки, %", `ДС овернайт` = "Ставки, %",
          `UONIA` = "Ставки, %", `ОВДП (первинний ринок)` = "Ставки, %",
          `ДС (книжкова)` = "Ставки, %", `ОВДП (книжкова)` = "Ставки, %", `ОВДП 1 рік (NSS)` = "Ставки, %",
          `до облікової` = "Клин вартості депозитів, п. п.", `до ДС овернайт` = "Клин вартості депозитів, п. п.", `до ОВДП 1 рік` = "Клин вартості депозитів, п. п.",
          `ДС` = "Керрі над депозитами, п. п.", `ОВДП` = "Керрі над депозитами, п. п.")
  add_table(out, "Вартість депозитів і ринкові орієнтири за режимами монетарної політики", "T09b_benchmarks_by_regime.csv",
            note = "Клин = вартість депозитів − орієнтир; керрі = дохідність інструмента − вартість депозитів. ОВДП 1 рік — нульова крива Нельсона–Сігеля–Свенсона; UONIA — до 06.2020 Pre-UONIA; ОВДП первинний ринок — середньозважена дохідність розміщень у гривні (табл. 4.2.3 статистики НБУ).",
            star_note = FALSE, land = TRUE, size = 8, header2 = h2)
})

safe("T19 структурні зламки", {
  d <- csv(T21, "T19_structural_breaks_bai_perron")
  out <- d[, .(`Ряд` = series, `Зламів` = fint(n_breaks), `Дата зламу` = fdate(break_month),
               `95% ДІ` = paste0(fdate(ci_low), "–", fdate(ci_high)), `Середнє до, %` = fpct(mean_before), `Середнє після, %` = fpct(mean_after),
               `Зміна, п. п.` = fpct(mean_after - mean_before))]
  out[duplicated(`Ряд`), `:=`(`Ряд` = "", `Зламів` = "")]
  add_table(out, "Структурні зламки в рівнях вартості та структури фондування (Бай–Перрон)", "T19_structural_breaks_bai_perron.csv",
            note = "Кількість зламів обрано за BIC; мінімальний сегмент — 15 % вибірки; ДІ — 95 % довірчий інтервал дати зламу.", star_note = FALSE, size = 9)
})

safe("T21 панельні асоціації", {
  d <- csv(T21, "T21_panel_associations")
  TERM <- c(policy = "Облікова ставка", demand = "Частка коштів на вимогу", fx = "Валютна частка", hh = "Частка ФО",
            size = "Розмір (log активів)", cd = "ДС НБУ / активи", ovdp = "ОВДП / активи")
  MOD <- c(A_bankFE_policy = "(1) ФЕ банку", B_bank_monthFE = "(2) ФЕ банку + місяця", C_bank_monthFE_wtd = "(3) (2), зважено", D_bank_monthFE_war = "(4) (2), воєнний період")
  d[, m := MOD[model]]; d[is.na(m), m := model]
  terms <- intersect(names(TERM), unique(d$term)); mods <- unique(d$m)
  rows <- rbindlist(lapply(terms, function(.tt) {
    r1 <- c(list(`Змінна` = TERM[[.tt]]), setNames(lapply(mods, function(mm) { x <- d[term == .tt & m == mm]; if (nrow(x)) fb(x$estimate, p = x$p.value) else "" }), mods))
    r2 <- c(list(`Змінна` = ""), setNames(lapply(mods, function(mm) { x <- d[term == .tt & m == mm]; if (nrow(x)) fse(x$std.error) else "" }), mods))
    rbind(as.data.table(r1), as.data.table(r2)) }))
  st <- d[, .(n = n_obs[1], b = n_banks[1], r2 = within_r2[1]), by = m]
  foot <- rbind(as.data.table(c(list(`Змінна` = "Спостережень"), setNames(as.list(fint(st$n)), st$m))),
                as.data.table(c(list(`Змінна` = "Банків"), setNames(as.list(fint(st$b)), st$m))),
                as.data.table(c(list(`Змінна` = "R² (within)"), setNames(as.list(fnum(st$r2, 3)), st$m))),
                as.data.table(c(list(`Змінна` = "Ефекти банку"), setNames(as.list(rep("так", nrow(st))), st$m))),
                as.data.table(c(list(`Змінна` = "Ефекти місяця"), setNames(as.list(ifelse(grepl("^A", names(MOD)[match(st$m, MOD)]), "ні", "так")), st$m))), fill = TRUE)
  add_table(rbind(rows, foot, fill = TRUE), "Детермінанти вартості депозитів банку: панельні регресії з фіксованими ефектами",
            "T21_panel_associations.csv", note = "Залежна змінна — вартість депозитів банку (частка). Пояснювальні змінні лаговані на 1 місяць. Стандартні похибки кластеризовані за банками.")
})

safe("D1b опуклість", {
  d <- csv(T21, "D1b_convexity_summary_12m")
  out <- d[, .(`Ставка` = label, `γ₂ (кривизна)` = fb(gamma2, p = p_gamma2), `p-значення` = fp(p_gamma2),
               `β при 6 %` = fnum(beta_at_6pct), `β при 10 %` = fnum(beta_at_10pct), `β при 15 %` = fnum(beta_at_15pct),
               `β при 20 %` = fnum(beta_at_20pct), `β при 25 %` = fnum(beta_at_25pct), `N` = fint(n))]
  add_table(out, "Залежність 12-місячного коефіцієнта передачі від рівня облікової ставки (опуклість)", "D1b_convexity_summary_12m.csv",
            note = "β(i) = β₁ + 2γ₂·i з регресії 12-місячної зміни ставки на зміну облікової та її взаємодію з рівнем; від'ємне γ₂ — передача слабшає при високій ставці (ефект депозитної франшизи).",
            land = TRUE, size = 9)
})

safe("D3 банківська панель", {
  d <- csv(T21, "D3_bank_panel_convexity")
  TERM <- c(di = "Δ облікової", di_i = "Δ облікової × рівень", di_dem = "Δ облікової × частка на вимогу", di_size = "Δ облікової × розмір",
            di_hh = "Δ облікової × частка ФО", di_state = "Δ облікової × державний", di_foreign = "Δ облікової × іноземний")
  outs <- unique(d$outcome)
  rows <- rbindlist(lapply(intersect(names(TERM), unique(d$term)), function(.tt) {
    r1 <- c(list(`Змінна` = TERM[[.tt]]), setNames(lapply(outs, function(o) { x <- d[term == .tt & outcome == o]; if (nrow(x)) fb(x$estimate, p = x$p.value) else "" }), outs))
    r2 <- c(list(`Змінна` = ""), setNames(lapply(outs, function(o) { x <- d[term == .tt & outcome == o]; if (nrow(x)) fse(x$std.error) else "" }), outs))
    rbind(as.data.table(r1), as.data.table(r2)) }))
  st <- d[, .(n = n_obs[1], b = n_banks[1]), by = outcome]
  foot <- rbind(as.data.table(c(list(`Змінна` = "Спостережень"), setNames(as.list(fint(st$n)), st$outcome))),
                as.data.table(c(list(`Змінна` = "Банків"), setNames(as.list(fint(st$b)), st$outcome))),
                as.data.table(c(list(`Змінна` = "Ефекти банку"), setNames(as.list(rep("так", nrow(st))), st$outcome))))
  add_table(rbind(rows, foot), "Неоднорідність передачі облікової ставки у ставки депозитів банків (панель банків)", "D3_bank_panel_convexity.csv",
            note = "Залежна змінна — місячна зміна ставки банку за продуктом. Характеристики банку лаговані на 1 місяць. Стандартні похибки кластеризовані за банками.")
})

safe("D5 ефективна дюрація", {
  d <- csv(T21, "D5_effective_duration_nmd")
  PR <- c(hh_demand = "ФО: кошти на вимогу", corp_demand = "СГ: кошти на вимогу")
  d[, pr := fifelse(is.na(PR[product]), product, PR[product])]
  w <- dcast(d, pr + delta ~ i0, value.var = "duration")
  bt <- d[, .(beta = beta[1]), by = pr]
  out <- w[, c(list(`Продукт` = pr, `Швидкість відтоку δ` = fnum(delta, 2)), lapply(.SD, fnum, d = 1)), .SDcols = setdiff(names(w), c("pr", "delta"))]
  setnames(out, setdiff(names(out), c("Продукт", "Швидкість відтоку δ")), paste0("i₀ = ", fpct(as.numeric(setdiff(names(w), c("pr", "delta"))), 0), " %"))
  out[duplicated(`Продукт`), `Продукт` := ""]
  add_table(out, "Ефективна дюрація безстрокових депозитів (коштів на вимогу), роки", "D5_effective_duration_nmd.csv",
            note = paste0("Дюрація франшизи за моделлю Дрекслера–Савова–Шнабля: D = (1 − β) / (i₀ + δ) з урахуванням ставки продукту; β — довгостроковий коефіцієнт передачі (",
                          paste(sprintf("%s: %s", bt$pr, fnum(bt$beta, 3)), collapse = "; "), "); δ — річна швидкість відтоку; i₀ — рівень ставки дисконтування."),
            star_note = FALSE)
})

safe("D6 стік-потік", {
  d <- csv(T21, "D6_stock_flow_lambda")
  PR <- c(hh_term = "ФО: строкові", corp_term = "СГ: строкові", hh_demand = "ФО: на вимогу", corp_demand = "СГ: на вимогу")
  PE <- c(all = "Уся вибірка", prewar = "До війни", war = "Війна (з 2022-03)")
  d[, pr := fifelse(is.na(PR[product]), product, PR[product])]
  out <- d[, .(`Продукт` = pr, `Період` = fifelse(is.na(PE[period]), period, PE[period]), `λ (частка переоцінки за місяць)` = fb(l, se), `с. п.` = fse(se),
               `Середній строк переоцінки, міс.` = fnum(horizon_m, 1), `N` = fint(n))]
  out[duplicated(`Продукт`), `Продукт` := ""]
  add_table(out, "Швидкість переоцінки портфеля строкових депозитів: модель «запас–потік»", "D6_stock_flow_lambda.csv",
            note = "r_книжк,t = (1 − λ) r_книжк,t−1 + λ r_нові,t; середній строк переоцінки = 1/λ.")
})

safe("D7b Шеплі", {
  d <- csv(T21, "D7b_shapley_mix_by_cycle")
  out <- d[, .(`Цикл` = fint(cycle), `Режим` = reg_ua(regime), `Період` = paste0(fdate(start), "–", fdate(end)), `Місяців` = fint(months),
               `Ефект структури, п. п.` = fpct(mix, 3), `строкова структура` = fpct(maturity, 3), `секторна структура` = fpct(sector, 3))]
  add_table(out, "Розклад ефекту структури депозитів на строкову та секторну складові (значення Шеплі) за циклами", "D7b_shapley_mix_by_cycle.csv",
            star_note = FALSE, size = 9, first_w = .6, header2 = c(`Цикл` = "Цикл", `Режим` = "Режим", `Період` = "Період", `Місяців` = "Місяців",
              `Ефект структури, п. п.` = "Ефект структури, п. п.", `строкова структура` = "у т. ч. за рахунок, п. п.", `секторна структура` = "у т. ч. за рахунок, п. п."))
})

safe("D8 власність", {
  d <- csv(T21, "D8_ownership_gap_decomposition")
  PA <- c("State − Private" = "Державні − приватні", "Foreign − Private" = "Іноземні − приватні", "State − Foreign" = "Державні − іноземні")
  d[, yr := format(as.Date(month), "%Y")]; d[, pa := fifelse(is.na(PA[pair]), pair, PA[pair])]
  a <- d[, .(gap = mean(gap, na.rm = TRUE), comp = mean(composition, na.rm = TRUE), price = mean(price, na.rm = TRUE)), by = .(pa, yr)]
  out <- a[, .(`Пара груп` = pa, `Рік` = yr, `Різниця вартості, п. п.` = fpct(gap), `Ефект структури` = fpct(comp), `Ціновий ефект` = fpct(price))]
  out[duplicated(`Пара груп`), `Пара груп` := ""]
  add_table(out, "Декомпозиція різниці у вартості депозитів між групами банків за формою власності (Оксака–Блайндер, середні за рік)",
            "D8_ownership_gap_decomposition.csv", note = "Різниця = ефект структури (різні частки продуктів) + ціновий ефект (різні ставки за однаковими продуктами). Останній рік — неповний.",
            star_note = FALSE)
})

safe("A3c переоцінка кредитів", {
  d <- csv(T21, "A3c_loan_book_repricing_lambda")
  PE <- c(all = "Уся вибірка", prewar = "До війни", war = "Війна (з 2022-03)")
  out <- d[, .(`Сектор` = sector, `Період` = fifelse(is.na(PE[period]), period, PE[period]), `λ` = fb(lambda, se), `с. п.` = fse(se),
               `Строк переоцінки, міс.` = ifelse(reliable, fnum(horizon_m, 1), "—"), `N` = fint(n), `Надійна оцінка` = ifelse(reliable, "так", "ні"))]
  out[duplicated(`Сектор`), `Сектор` := ""]
  add_table(out, "Швидкість переоцінки кредитного портфеля: модель «запас–потік»", "A3c_loan_book_repricing_lambda.csv",
            note = paste("Книжкова дохідність кредитів (фінансова звітність) на нові ставки (статистика НБУ).", if (nrow(d)) unique(d$source)[1] else "",
                         "Оцінка вважається ненадійною, якщо λ ∉ (0; 1) або t < 2."))
})

safe("A4b премії", {
  d <- csv(T21, "A4b_premia_last12m_by_maturity")
  nm <- setdiff(names(d), "bucket")
  out <- d[, c(list(`Строк кредиту` = bucket), lapply(.SD, fpct)), .SDcols = nm]
  add_table(out, "Премія за ризик у ставках нових кредитів СГ у гривні відносно альтернативних орієнтирів, останні 12 місяців, п. п.",
            "A4b_premia_last12m_by_maturity.csv", note = "Премія = ставка нових кредитів − орієнтир відповідного строку.", star_note = FALSE)
})

safe("A5b активи за циклами", {
  d <- csv(T21, "A5b_asset_price_mix_by_cycle")
  out <- d[, .(`Цикл` = fint(cycle), `Режим` = reg_ua(regime), `Період` = paste0(fdate(start), "–", fdate(end)), `Місяців` = fint(months),
               `Δ дохідності активів, п. п.` = fpct(delta), `Ціновий ефект, п. п.` = fpct(price), `Ефект структури, п. п.` = fpct(mix))]
  add_table(out, "Декомпозиція зміни дохідності процентних активів на ціновий ефект і ефект структури за циклами", "A5b_asset_price_mix_by_cycle.csv",
            star_note = FALSE, size = 9, first_w = .6)
})

safe("A9b узгодження", {
  d <- csv(T21, "A9b_matching_regression")
  out <- d[, .(`Специфікація` = spec, `Нахил` = fb(slope, se), `с. п.` = fse(se), `Банків` = fint(n_banks), `R²` = fnum(r2, 3))]
  add_table(out, "Узгодження чутливості активів і пасивів: регресія β активів банку на β його фондування", "A9b_matching_regression.csv",
            note = "Нахил = 1 означає повне узгодження (банки з дорожчим фондуванням переоцінюють активи так само швидко).")
})

safe("A10b 5-7-9", {
  d <- csv(T21, "A10b_579_premia_summary")
  out <- d[, .(`Вікно` = window, `Місяців` = fint(months), `Частка 5-7-9 у нових, %` = fpct(s_new), `Частка в портфелі, %` = fpct(s_stock),
               `UIRD 3M, %` = fpct(uird_ref), `Ставка банку за 5-7-9, %` = fpct(bank_rate_579), `ДС овернайт, %` = fpct(cd_on),
               `Премія сира` = fpct(prem_raw), `Премія скоригована` = fpct(prem_adj), `межі` = paste0(fpct(prem_adj_lo), "…", fpct(prem_adj_hi)),
               `Ринкові кредити` = fpct(prem_market), `Кредити 5-7-9` = fpct(prem_579))]
  h2 <- setNames(c(rep("", 7), rep("Премія до ДС овернайт, п. п.", 5)), names(out)); h2[h2 == ""] <- names(out)[h2 == ""]
  add_table(out, "Програма «Доступні кредити 5-7-9»: частка в кредитуванні та вплив на премії за кредитами СГ", "A10b_579_premia_summary.csv",
            note = "Ставка банку за програмою = UIRD 3M + маржа (обмеження компенсації); скоригована премія виключає компенсаційну складову з ринкової ставки; межі — для ставки позичальника 0 % і 9 %.",
            star_note = FALSE, land = TRUE, size = 9, header2 = h2)
})

safe("G3 динаміка", {
  d <- csv(T21, "G3_dynamics_annual_means_key")
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  for (c in num) set(d, j = c, value = if (grepl("рік$|Рік", c)) as.character(d[[c]]) else fnum(d[[c]], 2))
  d[duplicated(`Блок`), `Блок` := ""]
  add_table(d, "Середні показники динаміки ключових індикаторів (ланцюгові, середні за 2018–2026 рр.)", "G3_dynamics_annual_means_key.csv",
            note = "Ставки та частки — у %, абсолютний приріст — у п. п.; обсяги — у млн грн. * 2026 — неповний рік (середнє за наявні місяці).",
            star_note = FALSE, land = TRUE, size = 8)
})

safe("G4 рівні", {
  d <- csv(T21, "G4_annual_levels_wide"); yrs <- setdiff(names(d), c("group", "label"))
  for (c in yrs) set(d, j = c, value = fnum(d[[c]], 2))
  d[, group := fifelse(duplicated(group), "", group)]; setnames(d, c("group", "label"), c("Блок", "Показник"))
  add_table(d, "Річні рівні ключових показників фондування, активів і монетарних умов, 2018–2026 рр.", "G4_annual_levels_wide.csv",
            note = "Середні за рік. Ставки та частки — у %, обсяги — у млн грн. * 2026 — неповний рік.", star_note = FALSE, land = TRUE, size = 8, first_w = 1.1)
})

safe("G5 абсолютні зміни", {
  d <- csv(T21, "G5_annual_abs_change_wide"); yrs <- setdiff(names(d), c("group", "label"))
  for (c in yrs) set(d, j = c, value = fnum(d[[c]], 2))
  d[, group := fifelse(duplicated(group), "", group)]; setnames(d, c("group", "label"), c("Блок", "Показник"))
  add_table(d, "Абсолютні ланцюгові прирости ключових показників за рік, 2019–2026 рр.", "G5_annual_abs_change_wide.csv",
            note = "Ставки та частки — у п. п., обсяги — у млн грн.", star_note = FALSE, land = TRUE, size = 8, first_w = 1.1)
})

# ---- 2.1: expectations, full NBU rate statistics, monetary statistics (09) ----
# by_group tables of 09 have groups in rows; for the text they are transposed: indicators × regimes
tgroup <- function(f, digits = 2, first = "Показник") {
  d <- csv(TEX, f); d <- d[!is.na(d[[1]]) & nzchar(as.character(d[[1]]))]; g <- as.character(d[[1]]); v <- setdiff(names(d), c(names(d)[1]))
  out <- data.table(x = v); setnames(out, "x", first)
  for (k in seq_along(g)) out[[g[k]]] <- vapply(v, function(c) if (c == "Місяців") fint(d[[c]][k]) else fnum(d[[c]][k], digits), "")
  out }
safe("EX21 E1 очікування", add_table(tgroup("EX21_E1_expectations_by_regime"),
  "Очікування економічних агентів, реальні ex ante ставки та курсові очікування за режимами монетарної політики, середні, %",
  "EX21_E1_expectations_by_regime.csv", note = "Інфляційні та курсові очікування на 12 місяців (опитування НБУ); реальна облікова = облікова − π^e аналітиків; реальна ставка депозитів = ставка нових депозитів ФО до 1 р. у гривні − π^e домогосподарств; надлишкова дохідність = ставка гривневого депозиту − ставка валютного − очікувана девальвація ФО; очікувана зміна облікової — нульова крива ОВДП 1 р. мінус облікова ставка.",
  star_note = FALSE, land = TRUE, size = 9, first_w = 2.6))
safe("EX21 E1d заякореність", {
  d <- csv(TEX, "EX21_E1d_expectations_anchoring")
  out <- d[, .(`Група` = group, `Змінна` = fifelse(term == "cpi", "Фактична інфляція (ІСЦ, р/р)", "Облікова ставка"), `Коефіцієнт` = fb(b, se), `с. п.` = fse(se),
               `R²` = fnum(r2, 2), `Середній розрив π^e − ІСЦ, в. п.` = fnum(mean_gap, 2), `N` = fint(n))]
  out[duplicated(`Група`), `:=`(`Група` = "", `R²` = "", `Середній розрив π^e − ІСЦ, в. п.` = "", N = "")]
  add_table(out, "Заякореність інфляційних очікувань: чутливість до фактичної інфляції та облікової ставки", "EX21_E1d_expectations_anchoring.csv",
            note = "π^e_t = a + b·ІСЦ_t + c·i_t + ε; с. п. Ньюї–Веста (3 лаги). Менший коефіцієнт при фактичній інфляції — краща заякореність очікувань.")
})
safe("EX21 E2 ставки НБУ", add_table(tgroup("EX21_E2_nbu_rates_by_regime"),
  "Процентні ставки за новими та непогашеними операціями (статистика НБУ, розділ 4) за режимами монетарної політики, середні, %",
  "EX21_E2_nbu_rates_by_regime.csv", note = "Середньозважені ставки депозитних корпорацій за новими операціями й залишками (4.1.1–4.1.5) та дохідність ОВДП на первинному ринку (4.2.3).",
  star_note = FALSE, land = TRUE, size = 9, first_w = 2.6))
safe("EX21 E2b запас-потік", {
  d <- csv(TEX, "EX21_E2b_stock_flow_official_rates")
  PE <- c(all = "Уся вибірка", prewar = "До війни", war = "Війна (з 2022-03)")
  out <- d[, .(`Портфель` = label, `Період` = PE[period], `λ` = fb(lambda, se), `с. п.` = fse(se), `Строк переоцінки, міс.` = fnum(horizon_m, 1), `N` = fint(n))]
  out[duplicated(`Портфель`), `Портфель` := ""]
  add_table(out, "Швидкість переоцінки портфелів за офіційною статистикою ставок НБУ: модель «запас–потік»", "EX21_E2b_stock_flow_official_rates.csv",
            note = "r_залишки,t − r_залишки,t−1 = λ (r_нові,t − r_залишки,t−1); λ — частка портфеля, переоцінена за місяць; с. п. Ньюї–Веста (6 лагів).")
})
safe("EX21 E3 гроші", add_table(tgroup("EX21_E3_money_by_regime"),
  "Грошові агрегати та структура депозитів за режимами монетарної політики, середні",
  "EX21_E3_money_by_regime.csv", note = "Грошова статистика НБУ: M1 = M0 + переказні депозити в гривні; M2 = M1 + переказні депозити у валюті + інші депозити; M3 = M2 + цінні папери. Доларизація — частка валютних депозитів у депозитах, включених до M3; реальний M3 — дефльований ІСЦ.",
  star_note = FALSE, land = TRUE, size = 9, first_w = 2.6))
safe("EX21 E3b узгодженість", {
  d <- csv(TEX, "EX21_E3b_panel_vs_monetary_deposits")
  out <- data.table(`Показник` = c("Місяців", "Середнє співвідношення панель / грошова статистика", "Мінімум", "Максимум", "Кореляція місячних темпів приросту"),
                    `Значення` = c(fint(d$months), fnum(d$ratio_mean, 3), fnum(d$ratio_min, 3), fnum(d$ratio_max, 3), fnum(d$corr_mom_growth, 3)))
  add_table(out, "Узгодженість депозитів банківської панелі (OSB) з грошовою статистикою НБУ", "EX21_E3b_panel_vs_monetary_deposits.csv",
            note = "Панель: депозити ФО і СГ (на вимогу + строкові, усі валюти) за рахунками OSB; грошова статистика: депозити домогосподарств і нефінансових корпорацій у M3. Співвідношення > 1 через ширше охоплення рахунків OSB (зокрема ФОП і нерезидентів).",
            star_note = FALSE)
})

# ============================================================================
# 2.2 ТРАНСМІСІЯ
# ============================================================================
set_orient(FALSE)
heading("2.2. Передача облікової ставки у вартість фондування, дохідність активів і обсяги балансу")

LP <- tryCatch(csv(T22, "S22_P1_P5_panel_local_projections"), error = function(e) NULL)
HS <- c(0, 1, 3, 6, 12, 24)
lp_table <- function(kind_sel, title, note) {
  if (is.null(LP)) stop("немає S22_P1_P5")
  d <- LP[spec == "base" & term == ".s" & kind == kind_sel & h %in% HS]
  keys <- unique(d$key)
  w <- dcast(d, key + label + side ~ h, value.var = c("b", "se"))
  w <- w[match(keys, key)]
  cb <- paste0("b_", HS); cs <- paste0("se_", HS); cb <- cb[cb %in% names(w)]; cs <- cs[cs %in% names(w)]
  n12 <- d[h == 12, .(n = n[1], nb = nb[1]), by = key]; w <- merge(w, n12, by = "key", sort = FALSE)
  w <- w[match(keys, key)]
  out <- stack_bse(w, c("side", "label"), cb, cs, d = 3, id_names = c("Блок", "Показник"))
  setnames(out, cb, paste0("h = ", sub("b_", "", cb)))
  out[, `N (h = 12)` := ""]; out[, `Банків` := ""]
  idx <- seq(1, nrow(out), 2); out[idx, `N (h = 12)` := fint(w$n)]; out[idx, `Банків` := fint(w$nb)]
  out[`Блок` != "" & duplicated(`Блок`), `Блок` := ""]
  add_table(out, title, "S22_P1_P5_panel_local_projections.csv", note = note, land = TRUE, size = 9, first_w = .9)
}
LP_NOTE <- "Панельні локальні проєкції (Жорда, 2005): y_{i,t+h} − y_{i,t−1} = β_h Δi_t + Σ лагів Δy та Δi + контролі (інфляція, курс, характеристики банку, лаговані) + α_i + ε. Фіксовані ефекти банку; стандартні похибки Дріскола–Краая (лаг ≥ h + 1). h — горизонт, міс."
safe("P1 ставки", lp_table("rate", "Кумулятивна передача облікової ставки у ставки пасивів і активів банків: панельні локальні проєкції, п. п. на 1 п. п.",
                           paste(LP_NOTE, "Ставки — у п. п.")))
safe("P7 обсяги", lp_table("log", "Реакція обсягів депозитів, кредитів і ліквідних активів на зміну облікової ставки: панельні локальні проєкції, % на 1 п. п.",
                           paste(LP_NOTE, "Обсяги — 100 × log, тобто відсоткова зміна обсягу на 1 п. п. облікової ставки (депозитний канал Дрекслера–Савова–Шнабля; кредитний канал Кашьяпа–Стейна).")))

safe("P2/P3/P5 асиметрія і стани", {
  if (is.null(LP)) stop("немає S22_P1_P5")
  d <- LP[kind == "rate" & h == 12 & spec %in% c("asym", "level", "war", "ms")]
  keys <- unique(d$key); lab <- unique(d[, .(key, label)])
  pick <- function(sp, tm) { x <- d[spec == sp & term == tm, .(key, b, se)]; setnames(x, c("b", "se"), paste0(c("b_", "se_"), sp, tm)); x }
  w <- Reduce(function(a, b) merge(a, b, by = "key", all = TRUE),
              list(lab, pick("asym", ".s_up"), pick("asym", ".s_dn"), pick("level", ".s_hi"), pick("level", ".s_lo"),
                   pick("war", ".s_hi"), pick("war", ".s_lo"), pick("ms", ".s_hi"), pick("ms", ".s_lo")))
  pd <- d[!is.na(p_diff), .(p = p_diff[1]), by = .(key, spec)]; pw <- dcast(pd, key ~ spec, value.var = "p")
  setnames(pw, setdiff(names(pw), "key"), paste0("p_", setdiff(names(pw), "key"))); w <- merge(w, pw, by = "key", all.x = TRUE)
  w <- w[match(keys, key)]
  cb <- c("b_asym.s_up", "b_asym.s_dn", "b_level.s_hi", "b_level.s_lo", "b_war.s_hi", "b_war.s_lo", "b_ms.s_hi", "b_ms.s_lo")
  cs <- sub("^b_", "se_", cb)
  out <- stack_bse(w, "label", cb, cs, id_names = "Показник")
  for (s in c("asym", "level", "war", "ms")) { pc <- paste0("p_", s); v <- rep("", nrow(out)); v[seq(1, nrow(out), 2)] <- fp(w[[pc]] %||% NA); out[[pc]] <- v }
  setcolorder(out, c("Показник", "b_asym.s_up", "b_asym.s_dn", "p_asym", "b_level.s_hi", "b_level.s_lo", "p_level", "b_war.s_hi", "b_war.s_lo", "p_war", "b_ms.s_hi", "b_ms.s_lo", "p_ms"))
  lv <- unique(LP[spec == "level" & term == ".s_hi", term_lab])[1] %||% "висока"
  setnames(out, c("b_asym.s_up", "b_asym.s_dn", "p_asym", "b_level.s_hi", "b_level.s_lo", "p_level", "b_war.s_hi", "b_war.s_lo", "p_war", "b_ms.s_hi", "b_ms.s_lo", "p_ms"),
           c("Δi > 0", "Δi < 0", "p (1)", "висока", "низька", "p (2)", "війна", "до війни", "p (3)", "сильний", "слабкий", "p (4)"))
  h2 <- c(`Показник` = "Показник", `Δi > 0` = "(1) Знак зміни", `Δi < 0` = "(1) Знак зміни", `p (1)` = "(1) Знак зміни",
          `висока` = "(2) Рівень облікової", `низька` = "(2) Рівень облікової", `p (2)` = "(2) Рівень облікової",
          `війна` = "(3) Період", `до війни` = "(3) Період", `p (3)` = "(3) Період",
          `сильний` = "(4) Режим Маркова", `слабкий` = "(4) Режим Маркова", `p (4)` = "(4) Режим Маркова")
  add_table(out, "Асиметрія та залежність передачі від стану: 12-місячний коефіцієнт передачі, п. п. на 1 п. п.", "S22_P1_P5_panel_local_projections.csv",
            note = paste0("Специфікація як у панельних локальних проєкціях, з взаємодією шоку зі станом (стан визначається в t − 1). (2) ", lv,
                          " vs нижче медіани облікової ставки; (4) — фільтрована ймовірність сильного режиму моделі Маркова для вартості депозитів, t − 1. p — Вальд-тест рівності коефіцієнтів."),
            land = TRUE, size = 8, header2 = h2, first_w = 1.3)
})

safe("P2/P3/P5 тести за горизонтами", {
  d <- csv(T22, "S22_P2_P3_P5_state_tests")
  SP <- c(asym = "Δi⁺ − Δi⁻", level = "висока − низька", war = "війна − до війни", ms = "сильний − слабкий режим", exp = "очікувана − несподівана Δi")
  hs <- c(3, 6, 12, 24); hs <- hs[paste0("diff_h", hs) %in% names(d)]
  out <- d[, c(list(`Показник` = label, `Порівняння` = fifelse(is.na(SP[spec]), spec, SP[spec])),
               setNames(lapply(hs, function(h) fb(get(paste0("diff_h", h)), p = get(paste0("p_diff_h", h)))), paste0("h = ", hs)))]
  setorder(out, `Показник`); out[duplicated(`Показник`), `Показник` := ""]
  add_table(out, "Різниця коефіцієнтів передачі між станами за горизонтами, п. п. на 1 п. п.", "S22_P2_P3_P5_state_tests.csv",
            note = "Зірочки — за p-значенням Вальд-тесту рівності коефіцієнтів (Дріскол–Краай).", size = 9)
})

# ---- Markov ----
MS_SER <- c(cod4 = "Вартість депозитів (книжк.)", uird3m = "UIRD 3M", mir_hh = "Нові депозити ФО ≤1 р., грн", mir_nfc = "Нові депозити СГ ≤1 р., грн",
            mir_loan = "Нові кредити СГ, грн", y_iea = "Дохідність активів")
ms_terms <- function(term) {
  t <- sub(" \\| (режим )?", "|", term); base <- sub("\\|.*$", "", t); reg <- ifelse(grepl("\\|", t), sub("^.*\\|", "", t), "")
  B <- c(const = "Константа", di = "Δ облікової", di_l1 = "Δ облікової (t − 1)", dy_l1 = "Δy (t − 1)", i = "Облікова ставка",
         r_l1 = "y (t − 1)", sigma = "σ", p11 = "p₁₁", p22 = "p₂₂", p33 = "p₃₃")
  lb <- ifelse(is.na(B[base]), base, B[base])
  ifelse(reg == "", lb, ifelse(reg == "спільний", paste0(lb, ", спільний"), paste0(lb, ", режим ", reg))) }
ms_table <- function(mod, title, note) {
  d <- csv(T22, "S22_P4a_markov_coefficients")[model == mod]
  d[, tl := ms_terms(term)]; d[, s := fifelse(is.na(MS_SER[series]), series, MS_SER[series])]
  sers <- unique(d$s); tls <- unique(d$tl)
  rows <- rbindlist(lapply(tls, function(.tt) {
    r1 <- c(list(`Параметр` = .tt), setNames(lapply(sers, function(ss) { x <- d[tl == .tt & s == ss]; if (nrow(x)) fb(x$estimate, x$se) else "" }), sers))
    r2 <- c(list(`Параметр` = ""), setNames(lapply(sers, function(ss) { x <- d[tl == .tt & s == ss]; if (nrow(x)) fse(x$se) else "" }), sers))
    rbind(as.data.table(r1), as.data.table(r2)) }))
  f <- tryCatch(csv(T22, "S22_P4b_markov_fit_vs_linear")[model == mod], error = function(e) NULL)
  if (!is.null(f) && nrow(f)) {
    f[, s := fifelse(is.na(MS_SER[series]), series, MS_SER[series])]
    g <- function(lab, v) as.data.table(c(list(`Параметр` = lab), setNames(as.list(v[match(sers, f$s)]), sers)))
    rows <- rbind(rows, g("Спостережень", fint(f$n)), g("log L (Марков)", fnum(f$loglik, 1)), g("log L (лінійна)", fnum(f$loglik_linear, 1)),
                  g("LR (Марков vs лінійна)", fnum(f$LR, 1)), g("BIC (Марков / лінійна)", paste0(fnum(f$BIC, 1), " / ", fnum(f$BIC_linear, 1))),
                  g("RCM", fnum(f$RCM, 1)), g("Тривалість режимів, міс.", gsub("\\.", ",", f$dur)),
                  g("Дисперсія перемикається", if ("sigma_switching" %in% names(f)) ifelse(f$sigma_switching, "так", "ні") else rep("так", nrow(f))),
                  g("Режим-викид (p_kk < 0,5)", if ("outlier_regime" %in% names(f)) ifelse(f$outlier_regime, "так", "ні") else rep("—", nrow(f))))
  }
  add_table(rows, title, "S22_P4a_markov_coefficients.csv; S22_P4b_markov_fit_vs_linear.csv", note = note, land = length(sers) > 4, size = 9, first_w = 1.5)
}
safe("P4 Марков A2", ms_table("A2", "Модель Маркова з перемиканням режимів (MS-ADL у різницях, K = 2): оцінки параметрів",
  "Δy_t = c_S + β₀,S Δi_t + β₁,S Δi_{t−1} + ρ Δy_{t−1} + ε_t, ε_t ~ N(0, σ²_S); режими впорядковано за β₀ (режим 2 — сильніша передача). Оцінювання ММП (фільтр Гамільтона, згладжування Кіма); с. п. — з числового гессіана. RCM — показник якості класифікації режимів (0 — чітка, 100 — відсутня). LR має нестандартний розподіл (параметри переходу не ідентифіковані за H₀), тому порівняння доповнено BIC."))
safe("P4 Марков B2", ms_table("B2", "Модель Маркова з частковим пристосуванням у рівнях (K = 2): оцінки параметрів",
  "y_t = a_S + b_S i_t + ρ_S y_{t−1} + ε_t; довгостроковий коефіцієнт передачі в режимі S: b_S / (1 − ρ_S). Інші позначення — як у попередній таблиці."))

safe("P4c довгострокові", {
  d <- csv(T22, "S22_P4c_markov_longrun_by_regime")
  out <- d[, .(`Показник` = label, `Режим` = fint(regime), `Короткострокова β` = fnum(b_short), `ρ` = fnum(rho), `Довгострокова θ` = fnum(theta_long),
               `Напіврозпад, міс.` = fnum(half_life_m, 1), `σ` = fnum(sigma), `p_ss` = fnum(p_stay), `Тривалість, міс.` = fnum(duration_m, 1))]
  out[duplicated(`Показник`), `Показник` := ""]
  add_table(out, "Довгострокова передача та стійкість режимів моделі часткового пристосування", "S22_P4c_markov_longrun_by_regime.csv",
            note = "θ = b / (1 − ρ); напіврозпад = ln 0,5 / ln ρ (лише для 0 < ρ < 1); тривалість режиму = 1 / (1 − p_ss).", star_note = FALSE, size = 9)
})

safe("P4e режими vs фази", {
  d <- csv(T22, "S22_P4e_markov_vs_policy_phases"); ph <- setdiff(names(d), "label")
  out <- d[, c(list(`Показник` = label), lapply(.SD, fnum, d = 2)), .SDcols = ph]; setnames(out, ph, reg_ua(ph))
  add_table(out, "Середня ймовірність режиму сильної передачі за фазами монетарної політики", "S22_P4e_markov_vs_policy_phases.csv",
            note = "Згладжена ймовірність режиму 2 (MS-ADL), усереднена за місяцями кожної фази.", star_note = FALSE)
})

# ---- heterogeneity ----
het_table <- function(hh) {
  b <- csv(T22, "S22_P6b_heterogeneity_joint_b"); s <- csv(T22, "S22_P6c_heterogeneity_joint_se"); col <- paste0("h", hh)
  if (!col %in% names(b)) stop("немає горизонту ", hh)
  labs <- unique(b$label); chars <- unique(b$char)
  wb <- dcast(b, label ~ char, value.var = col); ws <- dcast(s, label ~ char, value.var = col)
  wb <- wb[match(labs, label)]; ws <- ws[match(labs, label)]
  M <- merge(wb, ws, by = "label", suffixes = c("", "__se"), sort = FALSE)[match(labs, label)]
  out <- stack_bse(M, "label", chars, paste0(chars, "__se"), id_names = "Показник")
  add_table(out, sprintf("Неоднорідність передачі між банками: спільна специфікація взаємодій, h = %d міс.", hh), "S22_P6b/P6c_heterogeneity_joint.csv",
            note = "Коефіцієнти при Δi × z-характеристика банку (стандартизована в кожному місяці, лагована): зміна передачі (п. п. на 1 п. п.) для банку, вищого на 1 с. в. за характеристикою. Фіксовані ефекти банку і місяця (агрегатний шок поглинається ефектами місяця); с. п. кластеризовані за банками. Від'ємний коефіцієнт — слабша передача.",
            land = TRUE, size = 8, first_w = 1.6)
}
safe("P6 h12", het_table(12)); safe("P6 h24", het_table(24))

safe("P7 обсяги неоднорідність", {
  d <- csv(T22, "S22_P7_quantity_channels_heterogeneity")
  hs <- c(6, 12, 24); hs <- hs[paste0("b_h", hs) %in% names(d)]
  out <- stack_bse(d, c("label", "char_lab"), paste0("b_h", hs), paste0("se_h", hs), id_names = c("Обсяг", "Характеристика"))
  setnames(out, paste0("b_h", hs), paste0("h = ", hs))
  out[`Обсяг` != "" & duplicated(`Обсяг`), `Обсяг` := ""]
  add_table(out, "Неоднорідність реакції обсягів на облікову ставку за характеристиками банку, % на 1 п. п. × 1 с. в.", "S22_P7_quantity_channels_heterogeneity.csv",
            note = "Двосторонні фіксовані ефекти (банк, місяць); с. п. кластеризовані за банками.", size = 9, first_w = 1.4)
})

# ---- ECM ----
safe("P8b ECM MG", {
  d <- csv(T22, "S22_P8b_ecm_mean_group")
  out <- d[, .(`Показник` = label, `Банків` = fint(banks), `θ (MG)` = fb(theta_mg, theta_se), `с. п. θ` = fse(theta_se), `θ (медіана)` = fnum(theta_med),
               `φ (MG)` = fb(phi_mg, phi_se), `с. п. φ` = fse(phi_se), `Напіврозпад, міс. (медіана)` = fnum(half_life_med, 1),
               `β₀ (MG)` = fnum(beta0_mg), `φ⁺ (вище рівноваги)` = fnum(phi_above), `φ⁻ (нижче)` = fnum(phi_below), `p (φ⁺ = φ⁻)` = fp(p_asym))]
  add_table(out, "Довгострокова передача та швидкість пристосування: модель корекції помилок на рівні банків (середня група)", "S22_P8b_ecm_mean_group.csv",
            note = "Δy_{it} = φ_i (y_{i,t−1} − α_i − θ_i i_{t−1}) + β₀,i Δi_t + Σ лагів + ε (Песаран–Сміт, 1995). θ — довгостроковий коефіцієнт передачі; φ — швидкість пристосування; φ⁺/φ⁻ — пристосування, коли ставка банку вище/нижче рівноваги («ракети і пір'я»). с. п. MG — з розкиду оцінок між банками.",
            land = TRUE, size = 9)
})

safe("P8c ECM система", {
  d <- csv(T22, "S22_P8c_ecm_system")
  out <- d[, .(`Показник` = label, `θ` = fnum(theta), `φ` = fnum(phi), `β₀` = fnum(beta0), `Напіврозпад, міс.` = fnum(hl, 1),
               `φ⁺` = fnum(phi_above), `φ⁻` = fnum(phi_below), `N` = fint(n))]
  add_table(out, "Модель корекції помилок для агрегованих (системних) рядів", "S22_P8c_ecm_system.csv", star_note = FALSE,
            note = "Специфікація як у моделі для банків, оцінена на системних місячних рядах.")
})

safe("P8d детермінанти", {
  d <- csv(T22, "S22_P8d_ecm_determinants")
  keys <- unique(d$key)[1:min(5, uniqueN(d$key))]; d <- d[key %in% keys]
  lab <- unique(d[, .(key, label)]); d[, col := paste0(key, "_", dep)]
  cols <- as.vector(t(outer(keys, c("theta", "phi"), paste, sep = "_"))); cols <- cols[cols %in% d$col]
  chars <- unique(d$char)
  rows <- rbindlist(lapply(chars, function(ch) {
    r1 <- c(list(`Характеристика` = ch), setNames(lapply(cols, function(c) { x <- d[char == ch & col == c]; if (nrow(x)) fb(x$b, x$se) else "" }), cols))
    r2 <- c(list(`Характеристика` = ""), setNames(lapply(cols, function(c) { x <- d[char == ch & col == c]; if (nrow(x)) fse(x$se) else "" }), cols))
    rbind(as.data.table(r1), as.data.table(r2)) }))
  nn <- d[, .(n = n[1]), by = col]
  rows <- rbind(rows, as.data.table(c(list(`Характеристика` = "Банків"), setNames(as.list(fint(nn$n[match(cols, nn$col)])), cols))))
  top <- setNames(lab$label[match(sub("_(theta|phi)$", "", cols), lab$key)], cols)
  newn <- ifelse(grepl("_theta$", cols), "θ", "φ"); setnames(rows, cols, paste0(newn, " [", top, "]"))
  h2 <- c(`Характеристика` = "Характеристика", setNames(top, paste0(newn, " [", top, "]")))
  setnames(rows, names(rows), sub(" \\[.*\\]$", "", names(rows)) |> make.unique(sep = " "))
  names(h2) <- names(rows)
  add_table(rows, "Детермінанти довгострокової передачі θ та швидкості пристосування φ між банками", "S22_P8d_ecm_determinants.csv",
            note = "Крос-секційна МНК-регресія оцінок θ_i та φ_i на середні стандартизовані характеристики банку; с. п. HC1.",
            land = TRUE, size = 8, header2 = h2, first_w = 1.6)
})

# ---- daily event study ----
safe("P11 подієве дослідження", {
  d <- csv(T22, "S22_P11b_daily_event_study_uird"); ks <- c(0, 1, 5, 10, 20, 30, 40, 60); ks <- ks[ks %in% d$k]
  sers <- unique(d$ser); d <- d[k %in% ks]
  mk <- function(bc, sc) {
    rows <- rbindlist(lapply(ks, function(kk) {
      r1 <- c(list(`Робочих днів після рішення` = fint(kk)), setNames(lapply(sers, function(s) { x <- d[k == kk & ser == s]; if (nrow(x)) fb(x[[bc]], x[[sc]]) else "" }), sers))
      r2 <- c(list(`Робочих днів після рішення` = ""), setNames(lapply(sers, function(s) { x <- d[k == kk & ser == s]; if (nrow(x)) fse(x[[sc]]) else "" }), sers))
      rbind(as.data.table(r1), as.data.table(r2)) }))
    rows }
  ev <- d$events[1]
  add_table(mk("b", "se"), "Денна реакція оголошених ставок за депозитами ФО (UIRD) на зміну облікової ставки: подієве дослідження, п. п. на 1 п. п.",
            "S22_P11b_daily_event_study_uird.csv",
            note = sprintf("Зміна UIRD від бази (2 робочі дні до рішення) до дня k, нормована на величину зміни облікової ставки; середнє за %s рішеннями НБУ 2018–2026 рр.; вікно обрізається на наступному рішенні. Ставки в доларах США — плацебо.", ev),
            land = length(sers) > 5, size = 9)
  add_table(mk("b_up", "se_up"), "Денна реакція UIRD: лише підвищення облікової ставки, п. п. на 1 п. п.", "S22_P11b_daily_event_study_uird.csv", land = length(sers) > 5, size = 9)
  add_table(mk("b_dn", "se_dn"), "Денна реакція UIRD: лише зниження облікової ставки, п. п. на 1 п. п.", "S22_P11b_daily_event_study_uird.csv", land = length(sers) > 5, size = 9)
})

safe("P11a рішення НБУ", {
  d <- csv(T22, "S22_P11a_nbu_rate_changes")
  out <- d[, .(`Дата рішення` = format(as.Date(date), "%d.%m.%Y"), `Облікова ставка, %` = fnum(r, 1), `Зміна, п. п.` = fnum(dr, 1),
               `Наступна зміна` = format(as.Date(next_date), "%d.%m.%Y"))]
  add_table(out, "Рішення НБУ щодо облікової ставки, використані в подієвому дослідженні", "S22_P11a_nbu_rate_changes.csv", star_note = FALSE, size = 9)
})

safe("P12 реформи ДС", {
  d <- csv(T22, "S22_P12b_cd_reforms_summary")
  out <- d[, .(`Реформа` = reform, `Показник` = label, `Середній ефект після реформи` = fb(post_avg, p = p), `с. п.` = fse(se), `p` = fp(p),
               `Перед-тренд χ²` = fnum(pretrend_chi2, 2), `p (перед-тренд)` = fp(pretrend_p), `Банків` = fint(banks), `Спостережень` = fint(obs))]
  out[duplicated(`Реформа`), `Реформа` := ""]
  add_table(out, "Квазіексперименти: зміни правил депозитних сертифікатів НБУ і ставки та обсяги строкових депозитів ФО (безперервна DiD)",
            "S22_P12b_cd_reforms_summary.csv",
            note = "y_{it} = α_i + λ_t + Σ_k δ_k · 1(t = T₀ + k) · E_i + ε, де E_i — частка ДС в активах банку до реформи (стандартизована). Наведено середнє δ_k після реформи (k = 0…6) і Вальд-тест перед-трендів (k < 0). Ставки — у п. п., обсяги — 100 × log; с. п. кластеризовані за банками.",
            land = TRUE, size = 9, first_w = 1.6)
})

# ---- 2.2: expectations, anticipated vs surprise policy, money (07 P13a + 09) ----
safe("P13a передбачуваність", {
  d <- csv(T22, "S22_P13a_policy_predictability")
  TL <- c("(Intercept)" = "Константа", p_xp3m_eom = "Нахил кривої ОВДП: 3 міс. − облікова, кінець місяця (t−1)", p_di_eom = "Δ облікової (t−1)", p_exp_path_3m = "Нахил кривої ОВДП: 3 міс. − облікова (t−1)", p_pie_mean = "Інфляційні очікування, середнє груп (t−1)",
          p_edep_mean = "Очікувана девальвація, середнє груп (t−1)", p_di = "Δ облікової (t−1)", p_dcpi = "Δ інфляції ІСЦ (t−1)")
  out <- stack_bse(d[, .(lab = fifelse(is.na(TL[term]), term, TL[term]), b, se)], "lab", "b", "se", id_names = "Змінна")
  setnames(out, "b", "Δi_t, в. п.")
  out <- rbind(out, data.table(c("R²", "p (Вальд: усі коефіцієнти = 0)", "N", "С. в. Δi / несподіваної складової, в. п."),
                               c(fnum(d$r2[1], 3), if ("f_p" %in% names(d)) fp(d$f_p[1]) else "", fint(d$n[1]), paste0(fnum(d$sd_di[1], 2), " / ", fnum(d$sd_sur[1], 2)))), use.names = FALSE)
  add_table(out, "Передбачуваність рішень щодо облікової ставки: розклад на очікувану та несподівану складові", "S22_P13a_policy_predictability.csv",
            note = "Δi_t — зміна облікової ставки на кінець місяця (сума рішень місяця). Δi_t = a + b′X_{t−1} + e_t, де X — інформація, доступна до рішення (ринкова крива, опитування очікувань, минулі зміни). Прогноз = очікувана складова, залишок = несподівана (логіка Каттнера, 2001; Ромерів, 2004). с. п. Ньюї–Веста (3 лаги).")
})
safe("P13 очікувані vs несподівані (LP)", {
  if (is.null(LP)) stop("немає S22_P1_P5")
  d <- LP[spec == "exp" & kind == "rate" & h %in% c(0, 3, 6, 12)]; if (!nrow(d)) stop("немає специфікації exp")
  w <- dcast(d, key + label ~ paste0(fifelse(term == ".s_ant", "a", "s"), h), value.var = c("b", "se"))
  w <- w[match(unique(d$key), key)]
  hs <- c(0, 3, 6, 12); cb <- c(paste0("b_a", hs), paste0("b_s", hs)); cb <- cb[cb %in% names(w)]
  out <- stack_bse(w, "label", cb, sub("^b_", "se_", cb), id_names = "Показник")
  pd <- d[term == ".s_ant" & h == 12, .(key, p = p_diff)]; v <- rep("", nrow(out)); v[seq(1, nrow(out), 2)] <- fp(pd$p[match(w$key, pd$key)]); out[["p (h = 12)"]] <- v
  nm <- sub("^b_a", "оч. h = ", sub("^b_s", "несп. h = ", cb)); setnames(out, cb, nm)
  h2 <- c(`Показник` = "Показник", setNames(ifelse(grepl("^оч", nm), "Очікувана зміна облікової", "Несподівана зміна облікової"), nm), `p (h = 12)` = "Тест рівності")
  add_table(out, "Реакція ставок на очікувані та несподівані зміни облікової ставки: панельні локальні проєкції, п. п. на 1 п. п.", "S22_P1_P5_panel_local_projections.csv",
            note = "Обидві складові входять у рівняння одночасно; інші елементи специфікації — як у базових панельних LP. Якщо банки ціноутворюють з огляду на очікуваний шлях ставки, очікувана складова відображається у ставках до рішення і має меншу реакцію після нього.",
            land = TRUE, size = 8, header2 = h2, first_w = 1.5)
})
safe("P13b DOLS", {
  d <- csv(TEX, "EX22_P13b_dols_expectations")
  w <- dcast(d, key + label + model ~ fifelse(term == "i", "th", fifelse(grepl("^pie", term), "ga", "de")), value.var = c("b", "se"))
  if (!"adf_t" %in% names(d)) d[, `:=`(adf_t = NA_real_, eg_cv5 = NA_real_)]
  st <- unique(d[, .(key, model, n, r2, phi, phi_se, adf_t, eg_cv5)], by = c("key", "model")); w <- merge(w, st, by = c("key", "model"), sort = FALSE)
  w <- w[order(match(key, unique(d$key)), model)]
  cb <- intersect(c("b_th", "b_ga", "b_de"), names(w))
  out <- stack_bse(w, c("label", "model"), c(cb, "phi"), c(sub("^b_", "se_", cb), "phi_se"), id_names = c("Ставка", "Модель"))
  setnames(out, c(cb, "phi"), c(c(b_th = "θ (облікова)", b_ga = "γ (інфляційні очікування)", b_de = "δ (очікувана девальвація)")[cb], "φ (корекція)"))
  out[, `ADF (залишки)` := ""]; out[, `R²` := ""]; out[, N := ""]; idx <- seq(1, nrow(out), 2)
  out[idx, `ADF (залишки)` := paste0(fnum(w$adf_t, 2), ifelse(is.finite(w$adf_t) & w$adf_t < w$eg_cv5, "†", ""))]
  out[idx, `R²` := fnum(w$r2, 2)]; out[idx, N := fint(w$n)]
  out[`Ставка` != "" & duplicated(`Ставка`), `Ставка` := ""]
  add_table(out, "Довгострокова передача з урахуванням очікувань: динамічний МНК (DOLS) і швидкість корекції", "EX22_P13b_dols_expectations.csv",
            note = "y_t = α + θ i_t + γ π^e_t + δ Δs^e_t + Σ_{j=−2..2} Δx_{t+j} + u_t (Сток–Вотсон, 1993); для ставок ФО — очікування домогосподарств, для СГ — середнє груп. φ — коефіцієнт при u_{t−1} у рівнянні Δy_t (Енгл–Грейнджер). ADF — t-статистика тесту на одиничний корінь у залишках; † — коінтеграцію не відхилено на 5 % (критичні значення Енгла–Грейнджера / МакКіннона). Модель (4) виключає березень–грудень 2022 р. (воєнний шок). с. п. Ньюї–Веста (6 лагів).",
            land = TRUE, size = 8, first_w = 1.7)
})
safe("P13c антиципація", {
  d <- csv(TEX, "EX22_P13c_anticipation_daily_uird")[term != "(Intercept)"]
  w <- dcast(d, tenor + window ~ term, value.var = c("b", "se")); st <- unique(d[, .(tenor, window, n, r2)]); w <- merge(w, st, by = c("tenor", "window"))
  w[, `:=`(tl = fifelse(tenor == "3m", "UIRD 3 міс.", "UIRD 12 міс."), wl = fifelse(window == "pre", "До рішення [−22; −2]", "Після рішення [−2; +20]"))]
  w <- w[order(tenor != "3m", window != "pre")]
  out <- stack_bse(w, c("tl", "wl"), c("b_dr_exp", "b_dr_sur"), c("se_dr_exp", "se_dr_sur"), id_names = c("Ставка", "Вікно, роб. дні"))
  setnames(out, c("b_dr_exp", "b_dr_sur"), c("Очікувана частина рішення", "Несподівана частина рішення"))
  out[, `R²` := ""]; out[, N := ""]; idx <- seq(1, nrow(out), 2); out[idx, `R²` := fnum(w$r2, 2)]; out[idx, N := fint(w$n)]
  out[`Ставка` != "" & duplicated(`Ставка`), `Ставка` := ""]
  add_table(out, "Чи закладають банки очікувані рішення НБУ в оголошені ставки заздалегідь: денні дані UIRD", "EX22_P13c_anticipation_daily_uird.csv",
            note = "Для кожного рішення: зміна UIRD до рішення (від −22 до −2 роб. днів) і після нього (від −2 до +20, не далі наступного рішення), регресована на очікувану (прогноз з нахилу кривої ОВДП наприкінці попереднього місяця) та несподівану частини зміни облікової ставки; с. п. HC1.")
})
safe("P13d доларизація", {
  d <- csv(TEX, "EX22_P13d_dollarisation_uip")
  d[, term_lab := fifelse(grepl("_l$", term) & grepl("dollar", term), "Залежна змінна (t−1)", term_lab)]
  w <- dcast(d, term_lab ~ dep, value.var = c("b", "se")); ord <- unique(d$term_lab); w <- w[match(ord, term_lab)]
  deps <- intersect(c("d_hh_dollar", "d_dollar"), unique(d$dep))
  out <- stack_bse(w, "term_lab", paste0("b_", deps), paste0("se_", deps), id_names = "Змінна")
  setnames(out, paste0("b_", deps), c(d_hh_dollar = "Δ доларизації депозитів ФО", d_dollar = "Δ доларизації депозитів у M3")[deps])
  st <- unique(d[, .(dep, n, r2)])[match(deps, dep)]
  out <- rbind(out, as.data.table(c(list("R²"), as.list(fnum(st$r2, 2)))), as.data.table(c(list("N"), as.list(fint(st$n)))), use.names = FALSE)
  add_table(out, "Валютне заміщення: доларизація депозитів, очікувана девальвація і надлишкова дохідність гривневих депозитів", "EX22_P13d_dollarisation_uip.csv",
            note = "Місячні дані, в. п.; Δ log курсу контролює механічну переоцінку валютних депозитів у гривні. с. п. Ньюї–Веста (3 лаги).")
})
safe("P13e кредити: попит vs пропозиція", {
  d <- csv(TEX, "EX22_P13e_lp_loans_demand_controls")[h %in% c(0, 3, 6, 12)]
  w <- dcast(d, spec ~ h, value.var = c("b", "se")); hs <- intersect(c(0, 3, 6, 12), d$h)
  out <- stack_bse(w, "spec", paste0("b_", hs), paste0("se_", hs), id_names = "Специфікація"); setnames(out, paste0("b_", hs), paste0("h = ", hs))
  add_table(out, "Реакція реального обсягу кредитів СГ на облікову ставку з контролем попиту (BLS, ІОДА): місячні LP, % на 1 п. п.", "EX22_P13e_lp_loans_demand_controls.csv",
            note = "y = 100·log(кредити СГ / ІСЦ). Попит на кредит (баланс відповідей BLS) лагований на квартал, ІОДА — на місяць. Якщо реакція кредиту зберігається після контролю попиту, це свідчить на користь каналу банківського кредитування (пропозиції). с. п. Ньюї–Веста (h + 1).")
  b <- tryCatch(csv(TEX, "EX22_P13e_bls_standards"), error = function(e) NULL)
  if (!is.null(b)) {
    TL <- c("(Intercept)" = "Константа", di_q = "Δ облікової за квартал", di_l = "Δ облікової за попередній квартал", dem = "Попит на кредит (BLS)",
            pie = "Інфляційні очікування", edep = "Очікувана девальвація", std_l = "Стандарти (t−1)")
    out <- stack_bse(b[, .(lab = TL[term], b, se)], "lab", "b", "se", id_names = "Змінна"); setnames(out, "b", "Стандарти кредитування СГ (баланс)")
    out <- rbind(out, data.table(c("R²", "N (кварталів)"), c(fnum(b$r2[1], 2), fint(b$n[1]))), use.names = FALSE)
    add_table(out, "Стандарти кредитування корпоративного сектору (BLS): облікова ставка, попит і очікування", "EX22_P13e_bls_standards.csv",
              note = "Квартальні дані; додатне значення стандартів — посилення. с. п. Ньюї–Веста (2 лаги).")
  }
})
safe("P13f режими vs очікування", {
  d <- csv(TEX, "EX22_P13f_markov_regimes_vs_expectations")
  w <- dcast(d, term_lab ~ series, value.var = c("b", "se")); ord <- unique(d$term_lab); w <- w[match(ord, term_lab)]
  sers <- unique(d$series); labs <- unique(d[, .(series, label)])
  out <- stack_bse(w, "term_lab", paste0("b_", sers), paste0("se_", sers), id_names = "Змінна")
  setnames(out, paste0("b_", sers), labs$label[match(sers, labs$series)])
  st <- unique(d[, .(series, n, r2)])[match(sers, series)]
  out <- rbind(out, as.data.table(c(list("R²"), as.list(fnum(st$r2, 2)))), as.data.table(c(list("N"), as.list(fint(st$n)))), use.names = FALSE)
  add_table(out, "Чи пов'язаний режим сильної передачі (модель Маркова) з очікуваннями економічних агентів", "EX22_P13f_markov_regimes_vs_expectations.csv",
            note = "Залежна змінна — згладжена ймовірність режиму сильної передачі (MS-ADL), %. Пояснювальні змінні лаговані на 1 місяць. с. п. Ньюї–Веста (6 лагів).",
            land = TRUE, size = 9, first_w = 2.2)
})
safe("P13g гроші", {
  d <- csv(TEX, "EX22_P13g_lp_money")[h %in% c(0, 3, 6, 12)]
  w <- dcast(d, label + shock ~ h, value.var = c("b", "se")); hs <- intersect(c(0, 3, 6, 12), d$h)
  w <- w[order(match(label, unique(d$label)), shock)]
  out <- stack_bse(w, c("label", "shock"), paste0("b_", hs), paste0("se_", hs), id_names = c("Показник", "Шок")); setnames(out, paste0("b_", hs), paste0("h = ", hs))
  out[`Показник` != "" & duplicated(`Показник`), `Показник` := ""]
  add_table(out, "Грошова трансмісія: реакція реальних грошових агрегатів і структури депозитів на облікову ставку, місячні LP", "EX22_P13g_lp_money.csv",
            note = "Реальні агрегати — 100·log(агрегат / ІСЦ), % на 1 п. п.; частки — п. п. на 1 п. п. Контроль: Δ log курсу. с. п. Ньюї–Веста (h + 1).",
            size = 9, first_w = 1.6)
})

safe("P10 стійкість", {
  d <- csv(T22, "S22_P10_robustness_long")[h == 12]
  vars <- unique(d$variant); keys <- unique(d$key)
  rows <- rbindlist(lapply(keys, function(kk) {
    x <- d[key == kk]; l <- x$label[1]
    r1 <- c(list(`Показник` = l), setNames(lapply(vars, function(v) { y <- x[variant == v]; if (nrow(y)) fb(y$b, y$se) else "" }), vars))
    r2 <- c(list(`Показник` = ""), setNames(lapply(vars, function(v) { y <- x[variant == v]; if (nrow(y)) fse(y$se) else "" }), vars))
    rbind(as.data.table(r1), as.data.table(r2)) }))
  add_table(rows, "Перевірка стійкості: 12-місячний коефіцієнт передачі за альтернативних специфікацій, п. п. на 1 п. п.", "S22_P10_robustness_long.csv",
            note = "Базова специфікація — панельні локальні проєкції з ФЕ банку та с. п. Дріскола–Краая. «Без лагів залежної змінної» — перевірка зміщення Нікелла.",
            land = TRUE, size = 8, first_w = 1.5)
})

safe("P9 підсумок", {
  d <- csv(T22, "S22_P9_summary_repricing")[kind == "rate"]
  out <- d[, .(`Блок` = side, `Показник` = label, `β₀` = fnum(b0), `β₃` = fnum(b3), `β₆` = fnum(b6), `β₁₂` = fb(b12, se12), `β₂₄` = fnum(b24),
               `β max` = fnum(b_max), `Половина max, міс.` = fint(half_life), `θ (ECM, MG)` = fnum(theta_mg), `φ (ECM, MG)` = fnum(phi_mg),
               `Напіврозпад ECM, міс.` = fnum(half_life_ecm, 1), `Δi⁺ − Δi⁻ (h = 12)` = fb(asym_up_minus_down_12, p = p_asym_12))]
  setorder(out, `Блок`); out[duplicated(`Блок`), `Блок` := ""]
  add_table(out, "Підсумок: швидкість і повнота переоцінки ставок пасивів і активів банків", "S22_P9_summary_repricing.csv",
            note = "β_h — кумулятивна передача з панельних локальних проєкцій; «половина max» — перший горизонт, на якому досягнуто 50 % максимальної реакції; θ, φ — модель корекції помилок (середня група). Ці показники — вхід для розрахунку розриву переоцінки в підрозділі 2.3.",
            land = TRUE, size = 8, first_w = .8)
})

# ============================================================================
# 2.3 ВИЯВЛЕНИЙ РОЗРИВ ПЕРЕОЦІНКИ (10_section_2_3_repricing_gap_v6.R)
# ============================================================================
OWN_UA <- c(State = "Державні", Foreign = "Іноземні", Private = "Приватні", All = "Усі банки")
own_ua <- function(x) { y <- OWN_UA[as.character(x)]; ifelse(is.na(y), as.character(x), y) }
PROD_UA <- c(hh_demand = "Кошти ФО на вимогу", hh_term = "Строкові депозити ФО", corp_demand = "Кошти СГ на вимогу", corp_term = "Строкові депозити СГ")
heading("2.3. Виявлений розрив переоцінки: чутливість чистого процентного доходу та економічної вартості капіталу")

safe("S23 структура балансу", {
  d <- csv(T23, "S23_R1_structure_latest")
  out <- d[, .(`Стаття` = get(names(d)[1]), `Бік балансу` = get(names(d)[2]), `Гривня, млрд грн` = fnum(get(names(d)[3]), 1), `Валюта, млрд грн` = fnum(get(names(d)[4]), 1),
               `Частка в активах, %` = fnum(get(names(d)[5]), 1))]
  add_table(out, "Статті балансу банківської системи, що переоцінюються за процентною ставкою, на останню дату", "S23_R1_structure_latest.csv", star_note = FALSE, size = 9, first_w = 2.6,
            note = "Статті згруповано за способом переоцінки: строкові інструменти — за контрактним строком (λ — частка залишку, що переоцінюється щомісяця), кошти на вимогу — за поведінковою передачею з підрозділу 2.2.")
})
safe("S23 система: договірний vs виявлений", {
  d <- csv(T23, "S23_R3_system_monthly")[, month := as.Date(month)]
  pick <- d[month %in% c(as.Date(paste0(2019:2026, "-08-01")), max(month))][!duplicated(month)]
  out <- pick[, .(`Дата` = fdate(month), `Банків` = fint(banks), `Договірний розрив 12 міс., % активів` = fpct(CG12, 1), `Виявлений розрив 12 міс., % активів` = fpct(RG12, 1),
                  `Довгостроковий виявлений, % активів` = fpct(RGLR, 1), `ΔЧПД +100 б.п.: договірний, %` = fnum(dNII_c_pct, 1), `ΔЧПД +100 б.п.: виявлений, %` = fnum(dNII_r_pct, 1),
                  `ΔEVE +200 б.п.: договірний, % капіталу` = fnum(dEVE_c_eq, 1), `ΔEVE +200 б.п.: поведінковий, % капіталу` = fnum(dEVE_b_eq, 1),
                  `Частка банків з додатним виявленим розривом, %` = fpct(share_asset_sensitive, 0))]
  add_table(out, "Договірний і виявлений розриви переоцінки банківської системи", "S23_R3_system_monthly.csv", star_note = FALSE, land = TRUE, size = 8,
            note = "Розрив 12 міс. = Σ(активи·s_A(12) − пасиви·s_L(12)) / активи, де s(12) — частка статті, переоцінена за 12 міс.: договірний — 1 − (1 − λ)^12 (кошти на вимогу переоцінюються одразу), виявлений — кумулятивна передача β₁₂ з панельних LP (п. 2.2). ΔЧПД — зміна чистого процентного доходу за перший рік при паралельному зростанні ставок на 100 б.п., % річного ЧПД. ΔEVE — зміна економічної вартості капіталу при +200 б.п.: договірна дюрація коштів на вимогу = 0, поведінкова — дюрація депозитної франшизи (Drechsler, Savov & Schnabl, 2021).")
})
safe("S23 власність", {
  d <- csv(T23, "S23_R3_by_ownership_latest")
  out <- d[, .(`Група банків` = own_ua(owner), `Банків` = fint(banks), `Активи, млрд грн` = fnum(TA_bn, 0), `Договірний розрив, % активів` = fnum(CG12_pp, 1), `Виявлений розрив, % активів` = fnum(RG12_pp, 1),
               `ΔЧПД +100 б.п., %` = fnum(dNII_r_pct, 1), `ΔEVE +200 б.п. (поведінковий), % капіталу` = fnum(dEVE_b_eq, 1), `Частка «чутливих до активів», %` = fpct(share_asset_sensitive, 0),
               `Частка з ΔEVE > 15 % капіталу, %` = fpct(share_SOT_b, 0))]
  add_table(out, "Розриви переоцінки за групами банків на останню дату", "S23_R3_by_ownership_latest.csv", star_note = FALSE, size = 9, land = TRUE,
            note = "Поріг 15 % капіталу — тест на виброс (supervisory outlier test) Базельського комітету для ризику процентної ставки банківської книги (BCBS, 2016).")
})
safe("S23 валідація", {
  d <- csv(T23, "S23_R4_validation_nim")
  TL <- c(x_rev = "Виявлений розрив_{t−12} × Δ₁₂i", x_con = "Договірний розрив_{t−12} × Δ₁₂i", x_lr = "Довгостроковий розрив_{t−12} × Δ₁₂i")
  w <- dcast(d, term ~ model, value.var = c("b", "se")); w[, lab := TL[term]]; w <- w[order(match(term, names(TL)))]
  ms <- sort(unique(d$model)); out <- stack_bse(w, "lab", paste0("b_", ms), paste0("se_", ms), id_names = "Змінна", d = 3)
  setnames(out, paste0("b_", ms), ms)
  st <- unique(d[, .(model, n, banks)]); out <- rbind(out, as.data.table(as.list(c("Спостережень", fint(st$n[match(ms, st$model)])))), as.data.table(as.list(c("Банків", fint(st$banks[match(ms, st$model)])))), use.names = FALSE)
  add_table(out, "Валідація розриву: чи прогнозує він фактичну зміну чистої процентної маржі банків", "S23_R4_validation_nim.csv", size = 9, first_w = 2.4,
            note = "Δ₁₂ЧПМ_{it} = α_i + λ_t + γ · (Розрив_{i,t−12} × Δ₁₂i_t) + ε_{it}; ЧПМ — % дохідних активів, розрив — частка активів, Δi — п. п. За точного виміру γ = 1. Ефекти банку і місяця; с. п. кластеризовані за банками (за підходом English, Van den Heuvel & Zakrajšek, 2018).")
})
safe("S23 сценарії", {
  d <- csv(T23, "S23_R5_scenarios_12m"); nm <- names(d)
  out <- d[, lapply(.SD, function(x) fnum(x, 2))]; out[[1]] <- fint(d[[1]])
  add_table(out, "Сценарії облікової ставки: зміна ЧПД банківської системи за 12 місяців, % річного ЧПД", "S23_R5_scenarios_system.csv", star_note = FALSE, size = 10,
            note = "Баланс постійний. Асиметричні β — окремі шляхи передачі для підвищень і знижень облікової ставки (п. 2.2).")
})

# ---- 2.3: стохастична модель депозитів (процеси Орнштейна–Уленбека, 14_ou_deposit_dynamics_v6.R) ----
bse <- function(b, se, d = 2) ifelse(is.finite(b), paste0(fb(b, se, d = d), " ", fse(se, d)), "")
safe("OU1 коротка ставка", {
  d <- csv(TOU, "OU_O1_short_rate")
  out <- d[, .(`Ряд` = series, `Специфікація середнього` = spec, `κ, на рік` = fnum(kappa, 2), `Напіврозпад, міс.` = fnum(half_life_m, 1),
               `b (π^e)` = bse(gamma_pie, gamma_pie_se), `φ (шлях ринку)` = fnum(phi_xpath, 2), `σ, в.п.` = fnum(100 * sigma, 1), `Стрибків` = fint(jumps), `Місяці стрибків` = jump_months, `R²` = fnum(r2, 2))]
  out[duplicated(`Ряд`), `Ряд` := ""]
  add_table(out, "Коротка ставка як процес Орнштейна–Уленбека з рухомим середнім і стрибками", "OU_O1_short_rate.csv", land = TRUE, size = 8, first_w = 1.6,
            note = "dx = κ(μ_t − x)dt + σdW + JdN; точна дискретизація x_{t+1} = e^{−κ/12}x_t + (1 − e^{−κ/12})μ_t + ε. μ_t = a + b·π^e_t (середні інфляційні очікування агентів) і, у третій специфікації, + φ·очікувана ринком зміна облікової (крива ОВДП). Стрибки — залишки понад 4 MAD; с. п. Ньюї–Веста (6 лагів), для довгострокових коефіцієнтів — дельта-метод.")
})
safe("OU2 ставки", {
  d <- csv(TOU, "OU_O2_rates_ou")[spec != "μ = a + β·i"]
  out <- d[, .(`Ставка` = series, `Вибірка` = fifelse(grepl("без", spec), "без 02–12.2022", "уся"), `κ, на рік` = fnum(kappa, 2), `Напіврозпад, міс.` = fnum(half_life_m, 1),
               `β (облікова)` = bse(beta_i, beta_i_se), `γ (π^e)` = bse(gamma_pie, gamma_pie_se), `δ (E[Δs])` = bse(delta_edep, delta_edep_se), `Стрибків` = fint(jumps), `N` = fint(n))]
  out[duplicated(`Ставка`), `Ставка` := ""]
  add_table(out, "Ставки за депозитами й активами як процеси Орнштейна–Уленбека з очікуваннями", "OU_O2_rates_ou.csv", land = TRUE, size = 8, first_w = 1.8,
            note = "μ = a + β·i + γ·π^e + δ·E[Δs]; очікування відповідних агентів: домогосподарств — для продуктів ФО, підприємств — для СГ, середні — для активів. β, γ, δ — довгострокові (на 1 п. п.). Для коштів ФО на вимогу ρ ≈ 1: довгострокові коефіцієнти на системному рівні не ідентифіковані (див. панельну оцінку).")
})
safe("OU2c панель", {
  d <- csv(TOU, "OU_O2c_bank_panel_ou")
  out <- d[, .(`Продукт` = series, `Банків` = fint(banks), `N` = fint(n), `κ, на рік` = fnum(kappa, 2), `Напіврозпад, міс.` = fnum(half_life_m, 1),
               `β (облікова)` = bse(beta_i, beta_i_se), `γ (π^e)` = bse(gamma_pie, gamma_pie_se), `δ (E[Δs])` = bse(delta_edep, delta_edep_se))]
  add_table(out, "Панельна модель Орнштейна–Уленбека ставок за депозитами банків", "OU_O2c_bank_panel_ou.csv", land = TRUE, size = 9, first_w = 1.8,
            note = "r_{b,t+1} = α_b + ρ r_{bt} + g₁i_t + g₂π^e_t + g₃E[Δs]_t + ε; ефекти банку; с. п. кластеризовані за банками; довгострокові коефіцієнти = g/(1 − ρ).")
})
safe("OU3 асиметрія", {
  d <- csv(TOU, "OU_O2b_asymmetric_reversion")
  out <- d[, .(`Ставка` = series, `Пристосування, ціль вище` = bse(adj_up, adj_up_se, 3), `Пристосування, ціль нижче` = bse(adj_dn, adj_dn_se, 3),
               `Напіврозпад ↑, міс.` = fnum(hl_up_m, 1), `Напіврозпад ↓, міс.` = fnum(hl_dn_m, 1), `p (рівність)` = fp(p_equal), `N` = fint(n))]
  add_table(out, "Асиметричне повернення ставок до цілі: «ракети і пір'я»", "OU_O2b_asymmetric_reversion.csv", land = TRUE, size = 9, first_w = 1.8,
            note = "Δr_{t+1} = a + κ⁺Δ·max(ціль − r, 0) + κ⁻Δ·min(ціль − r, 0); ціль — довгострокове середнє OU з очікуваннями. Для депозитів «ракети і пір'я» означають швидше зниження ставок, ніж підвищення.")
})
safe("OU4 обсяги", {
  d <- csv(TOU, "OU_O3_volumes_ou_jump")
  out <- d[, .(`Ряд` = series, `ρ` = fnum(rho, 2), `σ, %/рік^0,5` = fnum(100 * sigma, 1), `Реальна ex ante ставка` = bse(b_real_rate, b_real_rate_se, 3),
               `Очікувана девальвація` = bse(b_edep, b_edep_se, 3), `Очікування банків (BLS)` = bse(b_bls, b_bls_se, 3), `Стрибків` = fint(jumps), `λ, на рік` = fnum(lambda_yr, 2), `Місяці стрибків` = jump_months)]
  add_table(out, "Динаміка обсягів депозитів: процес Орнштейна–Уленбека зі стрибками і очікуваннями", "OU_O3_volumes_ou_jump.csv", land = TRUE, size = 8, first_w = 1.6,
            note = "Місячний приріст Δlog D як OU з середнім μ = a + b₁(r − π^e) + b₂E[Δs] + b₃·BLS (очікування банків щодо попиту на депозити); останній рядок — частка коштів ФО на вимогу (μ залежить від спреду строкових і поточних ставок та очікувань). Коефіцієнти — довгострокові.")
})
safe("OU5 Монте-Карло", {
  d <- csv(TOU, "OU_O4_policy_change_quantiles")
  out <- d[, .(`Модель` = model, `Горизонт, міс.` = fint(h_m), `Середнє` = fnum(1e4 * mean, 0), `0,1 %` = fnum(1e4 * q0.1, 0), `1 %` = fnum(1e4 * q1, 0), `5 %` = fnum(1e4 * q5, 0),
               `Медіана` = fnum(1e4 * q50, 0), `95 %` = fnum(1e4 * q95, 0), `99 %` = fnum(1e4 * q99, 0), `99,9 %` = fnum(1e4 * q99.9, 0))]
  out[duplicated(`Модель`), `Модель` := ""]
  add_table(out, "Розподіл зміни облікової ставки за Монте-Карло процесу Орнштейна–Уленбека, б.п.", "OU_O4_policy_change_quantiles.csv", size = 9, first_w = 1.6, star_note = FALSE,
            note = "10 000 траєкторій з корельованими шоками, невизначеністю параметрів і пуассонівськими стрибками (емпіричні розміри). Квантилі 99,9 % і 0,1 % за 6 міс. використано як сценарії OU у стрес-тесті EVE (крива — за Васічеком).")
})
safe("OU6 стабільна частка", {
  d <- csv(TOU, "OU_O5_stable_core_system"); n <- csv(TOU, "OU_O6_system_nii_distribution"); if ("model" %in% names(n)) n <- n[model == model[1]]
  out <- d[, .(`Продукт` = label, `Стабільна частка (1 %)` = fpct(stable_q1, 1), `Стабільна частка (5 %)` = fpct(stable_q5, 1), `β довгострокова` = fnum(beta_lr, 2),
               `Джерело β` = beta_source, `Ядро` = fpct(core, 1))]
  add_table(out, "Поведінкова модель коштів на вимогу з OU-симуляції: стабільна частка і ядро", "OU_O5_stable_core_system.csv", size = 9, first_w = 1.6, star_note = FALSE,
            note = paste0("Стабільна частка — 1-й (5-й) процентиль мінімуму обсягу за 12 міс. до поточного; ядро = стабільна × (1 − β). ЧПД системи за 12 міс. (Монте-Карло): медіана ",
                          fpct(n$pct_nii[n$stat == "медіана"], 1), " %, 1-й процентиль ", fpct(n$pct_nii[n$stat == "1-й процентиль"], 1), " %, ES 1 % ", fpct(n$pct_nii[n$stat == "ES 1 %"], 1), " % річного ЧПД."))
})

safe("OU7 стрибки облікової", {
  d <- csv(TOU, "OU_O1b_policy_jump_summary"); e <- csv(TOU, "OU_O1b_policy_jump_events")
  out <- d[, .(`Вибірка` = sample, `Років` = fnum(years, 1), `Стрибків` = fint(jumps), `вгору / вниз` = paste0(jumps_up, " / ", jumps_down), `λ (ОМП), на рік` = fnum(lambda_mle, 2),
               `λ апостеріорна, на рік` = fnum(lambda_post_mean, 2), `90 % інтервал` = paste0(fnum(lambda_post_q05, 2), "–", fnum(lambda_post_q95, 2)), `Сер. |стрибок|, в.п.` = fnum(mean_abs_pp, 1))]
  add_table(out, "Стрибки облікової ставки НБУ (зміна за місяць ≥ 3 в.п.) і їхня частота", "OU_O1b_policy_jump_summary.csv", size = 9, first_w = 1.6, star_note = FALSE,
            note = paste0("Події: ", paste(sprintf("%s (%+.1f)", format(as.Date(e$month), "%m.%Y"), e$change_pp), collapse = "; "), ". Апостеріорна частота — Гамма–Пуассон з неінформативним апріорним розподілом Джеффріса; у симуляції частота береться випадково для кожної траєкторії, розмір — емпіричний × U(0,75; 1,25)."))
})
safe("OU8 бутстреп κ", {
  d <- csv(TOU, "OU_O1c_bootstrap_kappa")
  out <- d[, .(`Ряд` = series, `κ` = fnum(kappa, 2), `90 % інтервал κ` = paste0(fnum(kappa_lo, 2), "–", fnum(kappa_hi, 2)), `κ з поправкою (бутстреп)` = fnum(kappa_bbc, 2), `κ з поправкою Кендалла` = fnum(kappa_kendall, 2),
               `Напіврозпад, міс.` = fnum(half_life_m, 1), `90 % інтервал, міс.` = paste0(fnum(hl_lo, 1), "–", fnum(hl_hi, 1)))]
  add_table(out, "Невизначеність швидкості повернення до середнього: блочний бутстреп і поправка на зміщення", "OU_O1c_bootstrap_kappa.csv", land = TRUE, size = 9, first_w = 1.8, star_note = FALSE,
            note = "Рекурсивний бутстреп залишків блоками по 6 міс., 499 повторень, регресори фіксовані. Поправка Кендалла ρ̂ + (1 + 3ρ̂)/T — для чистого AR(1) і завищує корекцію за наявності регресорів; бутстреп-поправка 2κ̂ − середнє(κ*) не наводиться («–»), якщо виходить за 90 % інтервал (скошений розподіл).")
})
safe("OU9 2МНК очікування", {
  d <- csv(TOU, "OU_O2d_iv_expectations")
  out <- d[, .(`Ставка` = series, `β (облікова)` = bse(beta_i, beta_i_se), `γ (π^e)` = bse(gamma_pie, gamma_pie_se), `δ (E[Δs])` = bse(delta_edep, delta_edep_se), `F 1-го етапу (π^e)` = fnum(first_stage_F_pie, 1), `N` = fint(n))]
  add_table(out, "Ставки з очікуваннями: оцінка 2МНК з урахуванням помилки вимірювання очікувань", "OU_O2d_iv_expectations.csv", land = TRUE, size = 9, first_w = 1.8,
            note = "Ендогенні: очікування групи (π^e, E[Δs]); інструменти — їхні лаги (t−1) і лаги середніх очікувань агентів (t−2). с. п. HC0, довгострокові коефіцієнти — дельта-метод. F > 10 — інструменти не слабкі.")
})
safe("OU10 крива", {
  f <- csv(TOU, "OU_O4b_pca_factors"); d <- csv(TOU, "OU_O4b_curve_scenarios")
  tc <- grep("^t_", names(d), value = TRUE)
  out <- d[, c(list(`Сценарій` = name), lapply(.SD, function(x) fnum(x, 0))), .SDcols = tc]; setnames(out, tc, sub("^t_", "", tc))
  add_table(out, "Сценарії кривої нульового купона НБУ з двофакторної моделі OU (рівень і нахил), б.п.", "OU_O4b_curve_scenarios.csv", land = TRUE, size = 9, first_w = 2.2, star_note = FALSE,
            note = sprintf("Головні компоненти місячних змін кривої (2018–2026): рівень пояснює %s %%, нахил — %s %% дисперсії; фактори — OU зі стрибками і t-шоками; сценарій — 99,9 %% (0,1 %%) квантиль 6-місячної зміни фактора × навантаження.",
                           fnum(100 * f$var_share[1], 0), fnum(100 * f$var_share[2], 0)))
})
safe("OU11 ЧПД варіанти", {
  d <- csv(TOU, "OU_O6_system_nii_distribution"); if (!"model" %in% names(d)) stop("стара версія")
  w <- dcast(d, model ~ stat, value.var = "pct_nii")
  out <- w[, .(`Модель` = model, `Медіана, %` = fpct(`медіана`, 1), `5-й процентиль, %` = fpct(`5-й процентиль`, 1), `1-й процентиль, %` = fpct(`1-й процентиль`, 1), `ES 1 %, %` = fpct(`ES 1 %`, 1))]
  add_table(out, "Зміна ЧПД банківської системи за 12 міс. (Монте-Карло OU), % річного ЧПД", "OU_O6_system_nii_distribution.csv", size = 9, first_w = 2.6, star_note = FALSE,
            note = "Передача банків — зміна ставки статті масштабована на θ банку з п. 2.2 відносно медіани; динамічний баланс — обсяги депозитів за траєкторіями OU, різниця фондування розміщується (залучається) за обліковою ставкою.")
})
safe("OU12 обмеження", {
  d <- csv(TOU, "OU_O7_limitations")
  out <- d[, .(`№` = id, `Обмеження` = limitation, `Як враховано` = treatment, `Результат` = result, `Що залишається` = remaining)]
  add_table(out, "Обмеження стохастичної моделі депозитів і їх врахування", "OU_O7_limitations.csv", land = TRUE, size = 8, first_w = 0.4, star_note = FALSE)
})

# ============================================================================
# 2.4 ТІНЬОВА ВАРТІСТЬ ФОНДУВАННЯ (11_section_2_4_shadow_cost_v6.R)
# ============================================================================
heading("2.4. Тіньова вартість фондування: обов'язкові резерви, внески до ФГВФО та валютна структура")

safe("S24 нормативи", {
  d <- csv(T24, "S24_rr_schedule")[, from := as.Date(from)]
  GR <- c(hh_demand = "ФО: поточні / на вимогу", hh_term_short = "ФО: строкові ≤ 92 дні", hh_term_long = "ФО: строкові ≥ 93 дні", corp_demand = "СГ: на вимогу", corp_term = "СГ: строкові")
  dts <- sort(unique(d$from)); g <- CJ(group = names(GR), ccy = c("uah", "fx"), from = dts)
  g[, ratio := { z <- d[group == .BY$group & ccy == .BY$ccy]; z$ratio[findInterval(as.numeric(from), as.numeric(z$from))] }, by = .(group, ccy)]
  g[, cell := fpct(ratio, 0)]
  w <- dcast(g, from ~ group + ccy, value.var = "cell")
  cov <- d[, .(cov = max(cov_max)), by = from]; w <- merge(w, cov, by = "from")
  cols <- as.vector(outer(c("uah", "fx"), names(GR), function(c, g) paste(g, c, sep = "_")))
  out <- w[, c("from", cols, "cov"), with = FALSE]
  out[, from := format(from, "%d.%m.%Y")]; out[, cov := fifelse(cov > 0, fpct(cov, 0), "–")]
  zw <- function(k) strrep("\u200b", k)                                      # invisible suffixes keep repeated sub-headers unique
  setnames(out, c("Діє з", as.vector(rbind(paste0("грн", zw(seq_along(GR))), paste0("вал.", zw(seq_along(GR))))), "Покриття ОВДП, % нормативу"))
  h2 <- setNames(c("Діє з", rep(unname(GR), each = 2), "Покриття ОВДП, % нормативу"), names(out))
  add_table(out, "Нормативи обов'язкового резервування НБУ, % бази", "S24_rr_schedule.csv (data_nbu/local_inputs/reserve_requirements.csv)", star_note = FALSE, land = TRUE, size = 8,
            header2 = h2, first_w = 1.0,
            note = "Джерела: рішення Правління НБУ від 12.12.2019 № 926-рш, 16.12.2022 № 587-рш, 30.01.2023 № 40-рш, 23.02.2023 № 71-рш, 24.03.2023, 09.08.2023 № 275-рш, 20.09.2024 № 345-рш. З 11.03.2023 частина нормативу за коштами ФО на вимогу (і з 11.05.2023 — строкових до 92 днів) у 10 в.п. не може покриватися бенчмарк-ОВДП. Резерви формуються в гривні на кореспондентському рахунку в НБУ.")
})
safe("S24 продукти", {
  d <- csv(T24, "S24_R2_products_latest"); d[[1]] <- PROD_UA[d[[1]]]; d[[2]] <- c(uah = "грн", fx = "валюта")[d[[2]]]
  out <- d[, lapply(.SD, as.character)]
  add_table(out, "Номінальна та економічна вартість депозитного фондування за продуктами на останню дату", "S24_R2_products_system_monthly.csv", star_note = FALSE, land = TRUE, size = 8, first_w = 1.6,
            note = "Економічна вартість EC = r + ρ_кор·i + ρ_ОВДП·(i − y_ОВДП) + f, де ρ — норматив, i — ставка депозитних сертифікатів НБУ овернайт (безризикова альтернатива), y_ОВДП — дохідність бенчмарк-ОВДП (крива нульового купона, 3 роки), f — регулярний збір ФГВФО (0,5 % для гривні, 0,8 % для валюти; лише кошти ФО). Гривневий еквівалент валютних депозитів — з очікуваною банками девальвацією (опитування НБУ). Поріг SFC = (r + f − ρ_ОВДП·y_ОВДП)/(1 − ρ) — мінімальна дохідність вільних коштів, за якої депозит беззбитковий (Reinhart & Reinhart, 1999).")
})
safe("S24 періоди", {
  d <- csv(T24, "S24_R3_periods")
  out <- d[, .(`Період` = period, `Місяців` = fint(months), `Номінальна вартість депозитів, %` = fnum(cod_nom, 2), `Економічна вартість, %` = fnum(ec, 2), `Неявний податок, в.п.` = fnum(wedge, 2),
               `у т.ч. резерви на кор. рахунку` = fnum(tax_cash, 2), `у т.ч. резерви в ОВДП` = fnum(tax_cov, 2), `у т.ч. ФГВФО` = fnum(tax_dgf, 2), `Ефективний норматив, %` = fnum(rho_eff, 1),
               `Маржа ДС − поріг SFC, в.п.` = fnum(carry_cd, 2), `Маржа ДС − номінальна вартість, в.п.` = fnum(carry_cd_nominal, 2))]
  add_table(out, "Номінальна й економічна вартість депозитів банківської системи за періодами резервної політики", "S24_R3_system_monthly.csv", star_note = FALSE, land = TRUE, size = 8,
            note = "Середні за місяцями періоду, зважені за депозитами банків. Маржа ДС — різниця між ставкою депозитних сертифікатів НБУ овернайт і вартістю фондування: справжня маржа «паркування» ліквідності (вхід для розділу 3).")
})
safe("S24 власність", {
  d <- csv(T24, "S24_R3_by_ownership_latest")
  out <- d[, .(`Група банків` = own_ua(owner), `Банків` = fint(banks), `Депозити, млрд грн` = fnum(dep_bn, 0), `Номінальна вартість, %` = fnum(cod_nom, 2), `Економічна вартість, %` = fnum(ec, 2),
               `Податок, в.п.` = fnum(wedge_pp, 2), `Ефективний норматив, %` = fnum(rho_eff, 1), `Частка валюти, %` = fnum(fx_share, 1), `Маржа ДС − SFC, в.п.` = fnum(carry_cd, 2))]
  add_table(out, "Вартість депозитного фондування за групами банків на останню дату", "S24_R3_by_ownership_latest.csv", star_note = FALSE, size = 9, land = TRUE)
})
safe("S24 гривня vs валюта", {
  d <- csv(T24, "S24_R4_uah_vs_fx_summary")
  out <- d[, .(`Продукт` = PROD_UA[prod], `Період` = period, `Місяців` = fint(months), `Валюта − гривня: номінально, в.п.` = fnum(gap_nominal, 2),
               `… у грн-еквіваленті, в.п.` = fnum(gap_ueq, 2), `… економічна вартість, грн-екв., в.п.` = fnum(gap_sfc_ueq, 2),
               `Валюта дешевша: номінально, % міс.` = fnum(share_fx_cheaper_nominal, 0), `… грн-екв., % міс.` = fnum(share_fx_cheaper_ueq, 0), `… з резервами, % міс.` = fnum(share_fx_cheaper_shadow, 0))]
  out[duplicated(`Продукт`), `Продукт` := ""]
  add_table(out, "Чи дешевше валютне фондування: номінально, у гривневому еквіваленті та з урахуванням резервів", "S24_R4_uah_vs_fx_monthly.csv", star_note = FALSE, land = TRUE, size = 8, first_w = 1.6,
            note = "Різниця «валюта − гривня» для однакового продукту, середня за місяцями. Валютна ставка в гривневому еквіваленті: (1 + r)(1 + E[Δs]) − 1; економічна вартість додає резервний податок і збір ФГВФО (вищі для валюти).")
})
safe("S24 реформи резервування", {
  d <- csv(T24, "S24_R6_rr_reforms_summary")
  out <- d[, .(`Реформа` = reform, `Показник` = label, `Середній ефект після реформи` = fb(post_avg, p = p, d = 2), `с. п.` = fse(se, 2), `p` = fp(p),
               `p (перед-тренд)` = fp(pretrend_p), `Банків` = fint(banks), `Спостережень` = fint(obs))]
  out[duplicated(`Реформа`), `Реформа` := ""]
  add_table(out, "Реформи обов'язкового резервування як квазіексперименти: безперервна DiD", "S24_R6_rr_reforms_summary.csv", land = TRUE, size = 8, first_w = 1.8,
            note = "y_{it} = α_i + λ_t + Σ_k δ_k · 1(t = T₀ + k) · E_i + ε, де E_i — частка бази, що стала резервованою (або резервується за вищим нормативом), у депозитах банку до реформи (стандартизована). Середнє δ_k для k = 0…9; Вальд-тест перед-трендів (k = −6…−2). Ставки — п. п., обсяги — 100 × log; с. п. кластеризовані за банками (за дизайном Glocker & Towbin, 2015; Alper, Binici & Demiralp, 2018).")
})
safe("S24 доларизація", {
  d <- csv(T24, "S24_R8_dollarization_response")
  TL <- c(ret_gap = "Перевага валютного депозиту в грн-еквіваленті, в.п.", tax_gap = "Різниця резервного податку валюта − гривня, в.п.")
  out <- d[, .(`Рівень` = level, `Продукт` = PROD_UA[prod], `Змінна` = TL[term], `Коефіцієнт` = fb(b, se, d = 3), `с. п.` = fse(se, 3), `N` = fint(n))]
  out[duplicated(paste(`Рівень`, `Продукт`)), `Продукт` := ""]; out[duplicated(`Рівень`), `Рівень` := ""]
  add_table(out, "Реакція валютної структури депозитів на відносну дохідність і резервний податок", "S24_R8_dollarization_response.csv", size = 9, first_w = 1.6,
            note = "Залежна змінна — зміна частки валюти в продукті за наступні 12 міс., в.п. Панель банків — ефекти банку і місяця (резервний податок однаковий для всіх банків і поглинається ефектами місяця); система — с. п. Ньюї–Веста (12 лагів). Логіка — De Nicolò, Honohan & Ize (2005), Basso, Calvo-González & Jurgilas (2011).")
})

if (CUR_LAND) doc <- body_set_default_section(doc, ps_land)   # the last section keeps the current orientation
t_fin <- Sys.time(); finalize_doc(); message(sprintf("  збірка документа %.1f с", as.numeric(difftime(Sys.time(), t_fin, units = "secs"))))
f_doc <- file.path(OUT, "Tables_Section_2.docx")
# officer's print() post-processes the whole body node by node (minutes for ~60 tables). The package
# is written from an empty document of the same template and only word/document.xml is replaced.
write_docx_fast <- function(doc, target) {
  xml_txt <- as.character(doc$doc_obj$get())
  base <- read_docx(); base <- body_set_default_section(base, ps_port)
  tmp <- tempfile(fileext = ".docx"); print(base, target = tmp)
  dd <- tempfile("docx_"); dir.create(dd); utils::unzip(tmp, exdir = dd)
  con <- file(file.path(dd, "word", "document.xml"), open = "wb"); writeBin(charToRaw(enc2utf8(xml_txt)), con); close(con)
  if (file.exists(target)) file.remove(target)
  zip::zip(zipfile = normalizePath(target, mustWork = FALSE), files = list.files(dd, recursive = TRUE, all.files = TRUE), root = dd, mode = "mirror")
  unlink(c(tmp, dd), recursive = TRUE); invisible(target)
}
t_fin <- Sys.time()
ok_fast <- tryCatch({ write_docx_fast(doc, f_doc); TRUE }, error = function(e) { message("  швидкий запис не вдався (", conditionMessage(e), ") — звичайний print()"); FALSE })
if (!ok_fast) print(doc, target = f_doc)
message(sprintf("  запис docx %.1f с", as.numeric(difftime(Sys.time(), t_fin, units = "secs"))))
idx <- rbindlist(INDEX, fill = TRUE); fwrite(idx, file.path(OUT, "Tables_Section_2_index.csv"), bom = TRUE)
message(sprintf("08: %d таблиць -> %s (пропущено: %d)", N_TAB, f_doc, sum(idx$status != "ok")))
