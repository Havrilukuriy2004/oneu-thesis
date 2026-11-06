# ============================================================================
# utils_markov_v6.R — Markov-switching regression (Hamilton 1989; Kim 1994)
# ----------------------------------------------------------------------------
#   y_t = X_t' β_{S_t} + ε_t,   ε_t ~ N(0, σ²_{S_t}),   S_t ∈ {1..K} Markov chain,
#   P[i, j] = Pr(S_t = j | S_{t−1} = i).
# Any subset of regressors can switch (sw = TRUE) or be common across regimes;
# variance switching optional. Missing y (e.g. the MIR gap 2023-02…2024-04) is
# handled exactly: the filter only predicts, no likelihood contribution.
# ML by BFGS from many starting values; s.e. from the numerical Hessian
# (delta method for σ and p_ii). Regimes are ordered by the switching
# coefficient named in `order_by` (ascending: regime 1 = weakest pass-through).
# Dependency-free (numDeriv only for the Hessian, optional).
# ============================================================================

.ms_unpack <- function(th, p, sw, K, sw_var) {
  ps <- sum(sw); pc <- p - ps; i <- 0
  take <- function(n) { v <- th[i + seq_len(n)]; i <<- i + n; v }
  Bs <- matrix(take(ps * K), ps, K)                       # switching coefficients
  bc <- take(pc)                                          # common coefficients
  ls <- take(if (sw_var) K else 1); sig <- exp(rep_len(ls, K))
  A <- matrix(take(K * (K - 1)), K, K - 1)                # transition logits (last column = reference)
  P <- t(apply(A, 1, function(a) { e <- exp(c(a, 0) - max(c(a, 0))); e / sum(e) }))
  B <- matrix(0, p, K); B[sw, ] <- Bs; if (pc) B[!sw, ] <- bc
  if (!is.null(names(sw))) rownames(B) <- names(sw)
  list(B = B, sig = sig, P = P)
}

.ms_filter <- function(par, y, X, smooth = FALSE) {
  K <- ncol(par$B); n <- length(y); P <- par$P
  ev <- eigen(t(P)); pi0 <- Re(ev$vectors[, which.min(abs(ev$values - 1))]); pi0 <- abs(pi0) / sum(abs(pi0))
  if (!all(is.finite(pi0))) pi0 <- rep(1 / K, K)
  xp <- matrix(NA_real_, n, K); xf <- matrix(NA_real_, n, K); ll <- 0; pred <- pi0
  mu <- X %*% par$B
  for (t in seq_len(n)) {
    xp[t, ] <- pred
    if (is.finite(y[t]) && all(is.finite(mu[t, ]))) {
      eta <- dnorm(y[t], mu[t, ], par$sig); f <- sum(pred * eta)
      if (!is.finite(f) || f <= 0) return(list(ll = -1e10))
      ll <- ll + log(f); filt <- pred * eta / f
    } else filt <- pred
    xf[t, ] <- filt; pred <- as.vector(t(P) %*% filt)
  }
  out <- list(ll = ll, filtered = xf, predicted = xp)
  if (smooth) {
    xs <- xf
    for (t in (n - 1):1) xs[t, ] <- xf[t, ] * as.vector(P %*% (xs[t + 1, ] / pmax(xp[t + 1, ], 1e-300)))
    out$smoothed <- xs / rowSums(xs)
  }
  out
}

ms_fit <- function(y, X, K = 2, sw = rep(TRUE, ncol(X)), sw_var = TRUE, order_by = NULL,
                   n_start = 15, seed = 2026, maxit = 3000, dates = NULL) {
  X <- as.matrix(X); p <- ncol(X); if (is.null(colnames(X))) colnames(X) <- paste0("x", seq_len(p))
  sw <- as.logical(sw); names(sw) <- colnames(X)
  ok <- is.finite(y) & apply(is.finite(X), 1, all)
  ols <- lm.fit(X[ok, , drop = FALSE], y[ok]); b0 <- ols$coefficients; b0[!is.finite(b0)] <- 0
  s0 <- sd(ols$residuals); n_obs <- sum(ok)
  ll_lin <- sum(dnorm(ols$residuals, 0, sqrt(mean(ols$residuals^2)), log = TRUE))
  nll <- function(th) { par <- .ms_unpack(th, p, sw, K, sw_var); -.ms_filter(par, y, X)$ll }
  trans0 <- function(v) as.vector(matrix(v, K, K - 1) * outer(seq_len(K), seq_len(K - 1), function(i, j) ifelse(i == j, 1, -1)))
  # data-driven starts: separate OLS on K subsamples split by (a) time, (b) the first switching regressor, (c) |residual|
  idx <- which(ok); xs <- which(sw & apply(X, 2, function(c) sd(c[ok]) > 0))[1]
  splits <- list(cut(seq_along(idx), K, labels = FALSE),
                 if (!is.na(xs)) cut(rank(X[idx, xs], ties.method = "first"), K, labels = FALSE) else NULL,
                 cut(rank(abs(ols$residuals), ties.method = "first"), K, labels = FALSE))
  data_starts <- lapply(Filter(Negate(is.null), splits), function(g) {
    fits <- lapply(seq_len(K), function(k) { j <- idx[g == k]; if (length(j) <= p + 2) return(NULL); lm.fit(X[j, , drop = FALSE], y[j]) })
    if (any(vapply(fits, is.null, logical(1)))) return(NULL)
    Bk <- sapply(fits, function(f) { b <- f$coefficients; b[!is.finite(b)] <- 0; b[sw] })
    sk <- sapply(fits, function(f) max(sd(f$residuals), 1e-6))
    c(as.vector(matrix(Bk, sum(sw))), b0[!sw], log(if (sw_var) sk else mean(sk)), trans0(2.5)) })
  data_starts <- Filter(Negate(is.null), data_starts)
  set.seed(seed); best <- NULL
  for (s in seq_len(n_start + length(data_starts))) {
    if (s <= length(data_starts)) th0 <- data_starts[[s]] else {
      spread <- if (s == length(data_starts) + 1) seq(-1, 1, length.out = K) else sort(rnorm(K))
      Bs <- sapply(seq_len(K), function(k) b0[sw] + 0.3 * spread[k] * (abs(b0[sw]) + 1e-3) + rnorm(sum(sw), 0, 0.1 * (abs(b0[sw]) + 1e-3)))
      th0 <- c(as.vector(Bs), b0[!sw], log(s0 * (if (sw_var) exp(0.5 * spread) else 1)), trans0(runif(1, 1, 3)))
    }
    fit <- tryCatch(optim(th0, nll, method = "BFGS", control = list(maxit = maxit, reltol = 1e-10)), error = function(e) NULL)
    if (!is.null(fit) && is.finite(fit$value) && (is.null(best) || fit$value < best$value)) best <- fit
  }
  if (is.null(best)) stop("Markov-switching: no start converged")
  th <- best$par; par <- .ms_unpack(th, p, sw, K, sw_var)
  # order regimes
  ob <- if (!is.null(order_by) && order_by %in% colnames(X) && sw[order_by]) par$B[order_by, ] else par$sig
  o <- order(ob); par$B <- par$B[, o, drop = FALSE]; par$sig <- par$sig[o]; par$P <- par$P[o, o, drop = FALSE]
  fl <- .ms_filter(par, y, X, smooth = TRUE)
  # standard errors: numerical Hessian in the natural parameterisation (β, σ, p_ij)
  se_tab <- NULL
  nat <- function(th2) { q <- .ms_unpack(th2, p, sw, K, sw_var); q$B <- q$B[, o, drop = FALSE]; q$sig <- q$sig[o]; q$P <- q$P[o, o, drop = FALSE]
                         c(as.vector(q$B[sw, , drop = FALSE]), q$B[!sw, 1], q$sig, diag(q$P)) }
  H <- tryCatch(if (requireNamespace("numDeriv", quietly = TRUE)) numDeriv::hessian(nll, th) else optimHess(th, nll), error = function(e) NULL)
  V <- tryCatch(solve(H), error = function(e) tryCatch(MASS::ginv(H), error = function(e2) NULL))
  est <- nat(th)
  if (!is.null(V)) {
    J <- tryCatch(if (requireNamespace("numDeriv", quietly = TRUE)) numDeriv::jacobian(nat, th) else NULL, error = function(e) NULL)
    se <- if (!is.null(J)) sqrt(pmax(diag(J %*% V %*% t(J)), 0)) else rep(NA_real_, length(est))
  } else se <- rep(NA_real_, length(est))
  lab <- c(as.vector(outer(colnames(X)[sw], seq_len(K), function(a, k) paste0(a, " | режим ", k))),
           if (any(!sw)) paste0(colnames(X)[!sw], " | спільний"), paste0("sigma | режим ", seq_len(K)), paste0("p", seq_len(K), seq_len(K)))
  tab <- data.table::data.table(term = lab, estimate = est, se = se, z = est / se)
  npar <- length(th)
  durations <- 1 / (1 - diag(par$P))
  ergodic <- { ev <- eigen(t(par$P)); v <- abs(Re(ev$vectors[, which.min(abs(ev$values - 1))])); v / sum(v) }
  probs <- data.table::data.table(t = seq_along(y), smoothed = fl$smoothed, filtered = fl$filtered)
  if (!is.null(dates)) probs[, date := dates]
  list(par = par, coef = tab, ll = -best$value, ll_linear = ll_lin, n = n_obs, k = npar,
       aic = 2 * npar + 2 * best$value, bic = log(n_obs) * npar + 2 * best$value,
       aic_linear = 2 * (p + 1) - 2 * ll_lin, bic_linear = log(n_obs) * (p + 1) - 2 * ll_lin,
       LR = 2 * (-best$value - ll_lin), degenerate = (2 * (-best$value - ll_lin)) < 1e-3, durations = durations, ergodic = ergodic, probs = probs,
       regime = apply(fl$smoothed, 1, which.max), K = K, sw = sw, convergence = best$convergence)
}

# regime-classification quality (Ang & Bekaert 2002): RCM = 400 * mean(Π p_k) for K = 2 (0 = sharp, 100 = no information)
ms_rcm <- function(fit) { pr <- fit$par; s <- fit$probs; m <- as.matrix(s[, grep("^smoothed", names(s)), with = FALSE])
  K <- ncol(m); 100 * K^2 * mean(apply(m, 1, prod)) }
