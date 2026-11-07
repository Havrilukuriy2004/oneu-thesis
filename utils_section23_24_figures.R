# Supplemental figures use the existing model outputs, dates and denominators.
# Called by sections 2.3 and 2.4 after their numerical blocks have completed.
owner_label <- function(x) {
  labels <- c(State = "Державні", Foreign = "Іноземні", Private = "Приватні")
  y <- unname(labels[as.character(x)]); y[is.na(y)] <- "Інші / невідомі"; y
}
product_label <- function(x) {
  labels <- c(hh_demand = "ФО на вимогу", hh_term = "ФО строкові",
              corp_demand = "СГ на вимогу", corp_term = "СГ строкові")
  unname(labels[as.character(x)])
}
chart_date_scale <- function(month) {
  ends <- range(as.Date(month), na.rm = TRUE)
  mid <- seq(as.Date(paste0(format(ends[1], "%Y"), "-01-01")), ends[2], by = "2 years")
  br <- sort(unique(c(ends, mid[mid > ends[1] + 365 & mid < ends[2] - 365])))
  labels <- ifelse(br %in% ends, format(br, "%m.%Y"), format(br, "%Y"))
  scale_x_date(breaks = br, labels = labels, expand = expansion(mult = c(.02, .04)))
}

add_section23_figures <- function(B, ITEMS, BP, CP, Sr, Sc, h12, latest) {
  L <- B[month == latest]
  contribution <- rbindlist(lapply(seq_len(nrow(ITEMS)), function(k) {
    item <- ITEMS[k]; sign <- if (item$side == "A") 1 else -1
    stock <- sum(L[[paste0("U_", item$item)]])
    if (!is.finite(stock)) stop("Non-finite stocks in the NII contribution figure")
    data.table(month = latest, item = item$item, label = item$label, side = item$side,
      stock_uah_mn = stock, shock_bp = 100,
      nii_revealed_mn = sign * stock * sum(BP[[item$item]][2:h12]) / 12 / 100,
      nii_contract_mn = sign * stock * sum(CP[[item$item]][2:h12]) / 12 / 100)
  }))
  stopifnot(isTRUE(all.equal(sum(contribution$nii_revealed_mn), sum(L$dNII_r), tolerance = 1e-8)),
            isTRUE(all.equal(sum(contribution$nii_contract_mn), sum(L$dNII_c), tolerance = 1e-8)))
  save_tab(contribution, "S23_R9_nii_contributions_latest.csv", "R9_contributions")
  groups <- c(cd = "НБУ та міжбанк", ib = "НБУ та міжбанк", loan_corp = "Кредити", loan_hh = "Кредити",
    ovdp = "Цінні папери", sec = "Цінні папери", res = "Резерви", hh_dem = "Кошти на вимогу",
    corp_dem = "Кошти на вимогу", hh_term = "Строкові депозити", corp_term = "Строкові депозити",
    nbu = "Інше фондування", ibl = "Інше фондування", oth = "Інше фондування")
  contribution[, group := unname(groups[item])]
  a <- contribution[group != "Резерви", .(contract = sum(nii_contract_mn) / 1e3,
                  revealed = sum(nii_revealed_mn) / 1e3), by = group]
  a[, group := factor(group, levels = a$group[order(a$revealed)])]
  long <- melt(a, id.vars = "group", variable.name = "model")
  long[, model := factor(model, levels = c("contract", "revealed"), labels = c("Договірна", "Виявлена"))]
  p <- ggplot(long, aes(value, group, colour = model, shape = model)) +
    geom_vline(xintercept = 0, linewidth = .3) +
    geom_segment(data = a, aes(x = contract, xend = revealed, y = group, yend = group),
                 inherit.aes = FALSE, colour = "grey70", linewidth = .5) + geom_point(size = 2.2) +
    labs(title = "Внески статей балансу в річну зміну ЧПД", subtitle = paste(format(latest, "%m.%Y"), "+100 б.п., постійний баланс"),
         x = "Зміна ЧПД за 12 місяців, млрд грн", y = NULL, caption = CAP)
  save_fig(p, "S23_R9_nii_contributions.png", 8.5, 5.3)

  risk <- L[is.finite(NII12) & NII12 > 0 & is.finite(dNII_c_pct) & is.finite(dNII_r_pct),
    .(nkb, month, bank = fullname, owner, assets_mn = TA, nii12_mn = NII12,
      nii_contract_pct = dNII_c_pct, nii_revealed_pct = dNII_r_pct,
      nii_revealed_200bp_pct = 2 * dNII_r_pct, eve_behavioural_tier1_pct = dEVE_b_t1)]
  save_tab(risk, "S23_R9_bank_risk_views_latest.csv", "R9_bank_risk")
  box <- melt(risk, id.vars = c("nkb", "owner"), measure.vars = c("nii_contract_pct", "nii_revealed_pct"), variable.name = "model")
  box[, `:=`(owner = factor(owner_label(owner), levels = c("Державні", "Іноземні", "Приватні", "Інші / невідомі")),
             model = factor(model, levels = c("nii_contract_pct", "nii_revealed_pct"), labels = c("Договірна оцінка", "Виявлена оцінка")))]
  p <- ggplot(box, aes(owner, value)) + geom_hline(yintercept = 0, linewidth = .3) +
    geom_boxplot(fill = "grey90", width = .55, outlier.shape = 1, outlier.size = 1.4) + facet_wrap(~model, ncol = 1) +
    labs(title = "Розподіл чутливості ЧПД за формою власності", subtitle = sprintf("%s, +100 б.п., %d банків із додатним річним ЧПД; медіана та міжквартильний діапазон", format(latest, "%m.%Y"), nrow(risk)),
         x = NULL, y = "Зміна за 12 місяців, % річного ЧПД", caption = CAP)
  save_fig(p, "S23_R9_nii_distribution_ownership.png", 8.5, 7)

  joint <- risk[is.finite(eve_behavioural_tier1_pct)]
  joint[, owner := owner_label(owner)]
  p <- ggplot(joint, aes(nii_revealed_200bp_pct, eve_behavioural_tier1_pct, colour = owner, shape = owner)) +
    geom_hline(yintercept = 0, linewidth = .3) + geom_vline(xintercept = 0, linewidth = .3) +
    geom_hline(yintercept = -15, linetype = "22", linewidth = .35, colour = "grey40") + geom_point(size = 1.8, alpha = .85) +
    labs(title = "Чутливість ЧПД та економічної вартості капіталу", subtitle = sprintf("%s, +200 б.п., %d банків; умовні оцінки за незмінного балансу", format(latest, "%m.%Y"), nrow(joint)),
         x = "Зміна річного ЧПД, % ЧПД", y = "Зміна EVE, % фактичного Tier 1", caption = paste(CAP, "Пунктир позначає втрату 15% Tier 1; це дослідницький скринінг, не повний наглядовий тест."))
  save_fig(p, "S23_R9_nii_eve_joint.png", 8.5, 5.4)
}

add_section24_figures <- function(PRODSFC, SHADOW, MAC) {
  wm <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  P <- PRODSFC
  latest <- max(P$month)
  products <- P[month == latest, .(stock_mn = sum(stock), nominal_pct = 100 * wm(r, stock),
                  economic_pct = 100 * wm(ec, stock)), by = .(prod, ccy)]
  products[, month := latest]
  save_tab(products, "S24_R9_product_cost_comparison.csv", "R9_products")
  a <- melt(products, id.vars = c("prod", "ccy"), measure.vars = c("nominal_pct", "economic_pct"), variable.name = "measure")
  a[, `:=`(product = factor(product_label(prod), levels = c("ФО на вимогу", "ФО строкові", "СГ на вимогу", "СГ строкові")),
           currency = factor(ccy, levels = c("uah", "fx"), labels = c("Гривня", "Валюта")),
           measure = factor(measure, levels = c("nominal_pct", "economic_pct"), labels = c("Номінальна", "Економічна")))]
  p <- ggplot(a, aes(value, product, colour = measure, shape = measure)) + geom_point(size = 2.2) +
    facet_wrap(~currency, ncol = 1) +
    labs(title = "Вартість депозитного фондування за продуктами", subtitle = paste(format(latest, "%m.%Y"), "без очікуваної девальвації у цьому порівнянні"),
         x = "Річна вартість, %", y = NULL, caption = paste(CAP, "Покриття резервів ОВДП є модельним припущенням."))
  save_fig(p, "S24_R9_product_costs.png", 8.5, 6)

  by_product <- P[, .(nominal = wm(r, stock), economic_ueq = wm(ec_ueq, stock)), by = .(month, prod, ccy)]
  fx <- dcast(by_product, month + prod ~ ccy, value.var = c("nominal", "economic_ueq"))
  fx[, `:=`(gap_nominal_pp = 100 * (nominal_fx - nominal_uah),
             gap_economic_ueq_pp = 100 * (economic_ueq_fx - economic_ueq_uah))]
  save_tab(fx, "S24_R9_fx_cost_gap.csv", "R9_fx_gap")
  a <- melt(fx, id.vars = c("month", "prod"), measure.vars = c("gap_nominal_pp", "gap_economic_ueq_pp"), variable.name = "measure")
  a[, `:=`(product = factor(product_label(prod), levels = c("ФО на вимогу", "ФО строкові", "СГ на вимогу", "СГ строкові")),
           measure = factor(measure, levels = c("gap_nominal_pp", "gap_economic_ueq_pp"), labels = c("Номінальна різниця", "Економічна у гривнях")))]
  p <- ggplot(a, aes(month, value, colour = measure, linetype = measure)) + geom_hline(yintercept = 0, linewidth = .3) +
    geom_line(linewidth = .6) + facet_wrap(~product, ncol = 1) + chart_date_scale(a$month) +
    labs(title = "Різниця вартості валютного і гривневого фондування", subtitle = "Додатне значення означає вищу вартість валютного ресурсу",
         x = NULL, y = "Валюта мінус гривня, в.п.", caption = paste(CAP, "Гривневий еквівалент використовує очікувану девальвацію; витрати хеджування не включено."))
  save_fig(p, "S24_R9_fx_cost_gap.png", 8.5, 10)

  carry <- SHADOW[, .(nominal_spread_pp = 100 * wm(carry_cd_nominal, dep),
                      adjusted_spread_pp = 100 * wm(carry_cd, dep)), by = month][order(month)]
  save_tab(carry, "S24_R9_cd_spread.csv", "R9_cd_spread")
  a <- melt(carry, id.vars = "month", variable.name = "measure")
  a[, measure := factor(measure, levels = c("nominal_spread_pp", "adjusted_spread_pp"),
                        labels = c("До коригувань", "Резерви та гривневий еквівалент"))]
  p <- ggplot(a, aes(month, value, colour = measure, linetype = measure)) + geom_hline(yintercept = 0, linewidth = .3) +
    geom_line(linewidth = .7) + chart_date_scale(a$month) +
    labs(title = "Розрахунковий спред розміщення ресурсів у ДС", subtitle = "Ставка ДС овернайт мінус номінальна вартість або поріг дохідності вільних коштів",
         x = NULL, y = "Різниця річних ставок, в.п.", caption = paste(CAP, "Операційні витрати, податки та вартість капіталу не включено; це не фактичний прибуток."))
  save_fig(p, "S24_R9_cd_spread.png", 8.5, 4.8)
}
