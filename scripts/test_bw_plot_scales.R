# Regression for independent visual dimensions, not synthetic banking results.
suppressPackageStartupMessages(library(ggplot2))
`%||%` <- function(a, b) if (is.null(a)) b else a
source("utils_figures_bw_v6.R", encoding = "UTF-8")
d <- expand.grid(month = 1:3, shock = factor(c("small", "large")), method = factor(c("first", "second")))
d$value <- seq_len(nrow(d))
p <- ggplot(d, aes(month, value, colour = shock, linetype = method,
                    group = interaction(shock, method))) + geom_line()
b <- ggplot_build(.to_bw(p))$data[[1]]
stopifnot(nrow(b) == nrow(d), !anyNA(b$linetype), !anyNA(b$colour), length(unique(b$group)) == 4)
p2 <- p + scale_linetype_manual(values = c(first = "dashed", second = "solid"))
b2 <- ggplot_build(.to_bw(p2))$data[[1]]
stopifnot(setequal(unique(b2$linetype), c("dashed", "solid")))
p3 <- ggplot(d, aes(month, value, colour = shock, shape = method)) +
  geom_point() + scale_shape_manual(values = c(first = 15, second = 17))
b3 <- ggplot_build(.to_bw(p3))$data[[1]]
stopifnot(setequal(unique(b3$shape), c(15, 17)))
p4 <- ggplot(d, aes(month, value, colour = shock, linetype = shock, group = shock)) +
  geom_line() + facet_wrap(~method)
b4 <- ggplot_build(.to_bw(p4))$data[[1]]
stopifnot(!anyNA(b4$linetype), length(unique(b4$PANEL)) == 2)
for (groups in c(2, 3)) {
  points <- data.frame(x = seq_len(groups), y = seq_len(groups), category = factor(seq_len(groups)))
  p5 <- ggplot(points, aes(x, y, colour = category, shape = category)) + geom_point()
  b5 <- ggplot_build(.to_bw(p5))$data[[1]]
  stopifnot(nrow(b5) == groups, !anyNA(b5$shape), length(unique(b5$shape)) == groups)
}
message("Black-and-white plot aesthetic regressions passed")
