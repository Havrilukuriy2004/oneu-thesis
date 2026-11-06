# Presentation labels and calendar-safe shifts; do not mutate source identifiers.
clean_bank_name <- function(x) trimws(gsub("[[:space:]]+", " ", gsub('["«»„“”]', "", x)))
clean_bank_columns <- function(d) {
  for (v in intersect(c("fullname", "bank", "bank_name"), names(d))) {
    raw <- paste0(v, "_raw")
    if (!raw %in% names(d)) d[, (raw) := get(v)]
    d[, (v) := clean_bank_name(get(v))]
  }
  d
}
calendar_shift <- function(x, month, n = 1L, type = c("lag", "lead")) {
  type <- match.arg(type)
  target <- add_months(month, if (type == "lag") -n else n)
  x[match(target, month)]
}
