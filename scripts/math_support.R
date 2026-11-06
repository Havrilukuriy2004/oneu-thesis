# plotmath expressions render as actual fractions, indices and powers in all R devices.
# No external TeX installation is needed.
math_label <- function(s) {
 out <- lapply(as.character(s), function(x) {
  if (is.na(x)) return(NA_character_)
  if (x == "β12") return(quote(beta[12]))
  if (x == "β0, в.п. на 1 в.п.") return(quote(beta[0]))
  if (x == "Рухоме середнє OU μ(π^e)") return(quote(mu(pi^e)))
  if (x == "Рухома ціль OU μ(i, π^e, E[Δs])") return(quote(mu(i,pi^e,E[Delta*s])))
  if (x == "Контроль очікувань (π^e, Δs^e)") return(quote(pi^e*","~Delta*s^e))
  if (grepl("^β = [-0-9.]+$", x)) {v <- as.numeric(sub("β = ","",x));return(bquote(beta == .(v)))}
  if (grepl("^δ = [0-9]+%$", x)) {v <- as.numeric(gsub("[^0-9]","",x));return(bquote(delta == .(v)*"%"))}
  x
 });as.expression(out)
}
.math_plot <- function(q, name) {
 for (i in seq_along(q$layers)) {
  lay <- q$layers[[i]]
  # Format only known formula labels; do not parse Ukrainian prose as code.
  if (inherits(lay$geom, "GeomText") && is.data.frame(lay$data) && "label" %in% names(lay$data)) {
   f <- grepl("^β = [-0-9.]+$", lay$data$label)
   if (all(f)) {lay$data$label <- sub("β = ", "beta == ", lay$data$label);lay$geom_params$parse <- TRUE;q$layers[[i]] <- lay}
  }
 }
 for (v in intersect(c("x","y"),names(q$labels))) if (is.character(q$labels[[v]])) q$labels[[v]] <- math_label(q$labels[[v]])
 equations <- list(
  D5_duration_nmd = quote(D(i) == frac(1,i+delta)-frac(beta(i),r(i)+delta)),
  D1_convexity_betas = quote(beta(i[0]) == gamma[1]+gamma[2]*i[0]),
  OU_F10_vasicek_decay = quote(Delta*R(tau) == Delta*i*frac(1-e^{-kappa*tau},kappa*tau)),
  S24_R3_nominal_vs_shadow = quote(EC == r+rho[cash]*i+rho[bond]*(i-y[b])+f),
  S23_R3_system_gap = quote(RG[h] == frac(sum(A[a]*beta[a*h])-sum(L[l]*beta[l*h]),TA)))
 if (name %in% names(equations)) {
  txt <- paste(deparse(equations[[name]],width.cutoff=500),collapse="")
  q <- q + labs(caption=txt) + theme(plot.caption=element_text(hjust=.5,size=10,margin=margin(t=9)))
  # ggplot captions do not parse strings; use a native expression instead.
  q$labels$caption <- as.expression(equations[[name]])
 }
 q
}
