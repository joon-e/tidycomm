
# Visualize as html- or pdf-table with linear model ---
#
# @param x a [tdcmm] model
#
# @return html-, pdf- or word-table
#
# @family tdcmm visualize
#
#' @export
knit_regress_table <- function(x,
                                    digits = 2,
                                    cap = NULL
                               ) {

  model <- model(x)

  SDs <- model$model |>
    dplyr::summarise(across(everything(), ~sd(.x, na.rm = TRUE))) |>
    tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "SD")

  sd_Y <- SDs[1,2] |>
    dplyr::pull()

  model_tibble <- x

  model_tibble <- model_tibble |>
    dplyr::left_join(SDs, by = "Variable") |>
    dplyr::mutate(beta = B * SD/sd_Y,
                  LL = stats::confint(model)[,1],
                  UL = stats::confint(model)[,2],
                  beta_LL = LL * SD/sd_Y,
                  beta_UL = UL * SD/sd_Y,
                  beta_LL_comp = stats::confint(model, level = .9)[,1]  * SD/sd_Y,
                  beta_UL_comp = stats::confint(model, level = .9)[,2]  * SD/sd_Y) |>
    dplyr::select(dplyr::any_of(c('Variable',
                           'B', 'StdErr', 'LL', 'UL',
                           'beta', 'beta_LL', 'beta_UL',
  #                         'beta_LL_compare', 'beta_UL_compare',
                           't', 'p',
                           'TOL', 'VIF')))

  model_summary <- summary(model)

  pf <- pf(model_summary$fstatistic[["value"]],
        model_summary$fstatistic[["numdf"]],
        model_summary$fstatistic[["dendf"]],
        lower.tail = FALSE) |>
    format.pval(eps = .001, nsmall = 3) %>%
    gsub("0\\.","\\.", .)

  R_squared <- model_summary$r.squared |>
    round(3)  %>%
    gsub("0\\.","\\.", .)

  R_squared_adj <- model_summary$adj.r.squared |>
    round(3) %>%
    gsub("0\\.","\\.", .)

  F <- model_summary$fstatistic[['value']] |>
    round(0)

  dependent_var <- model$terms[[2]] %>%
   gsub("_", " ", .) %>%
    stringr::str_to_title(.)

  if(!is.null(cap)) {
    cap <- glue::glue("Regression Model on {dependent_var}")
  }

  quality_notes <- glue::glue("{dependent_var}, R² = {R_squared}, R²adj = {R_squared_adj}, F({model_summary$fstatistic[['numdf']]},{model_summary$fstatistic[['dendf']]}) = {F}, p = {pf}, CI-Level = 95%")

  tab <- model_tibble
  tab_format <- tab |>
    select(dplyr::any_of(c('Variable',
                    'B', 'StdErr', 'LL', 'UL',
                    'beta', 'beta_LL', 'beta_UL',
       #             'beta_LL_compare', 'beta_UL_compare',
                    't', 'p',
                    'TOL', 'VIF'))) |>
    dplyr::mutate(dplyr::across(-1, ~round(.x, digits)),
                  dplyr::across(dplyr::any_of("p"), ~format.pval(.x, eps = .001, nsmall = 3, na.form = "—")),
                  dplyr::across(dplyr::any_of("p"), ~gsub("0\\.","\\.", .x))) |>
    mutate(
      across(dplyr::any_of(c("beta", "beta_LL", "beta_UL", "TOL")),
             ~ dplyr::if_else(is.na(.x), "—", sub("^(-?)0.", "\\1.", sprintf("%.3f", .x))))
    ) |>
    dplyr::mutate(dplyr::across(dplyr::any_of("VIF"), as.character))

tab_knit <- tab_format |>
    gt::gt() |>
    gt::cols_align(align = ("right"),
                   columns = -1) |>
    gt::tab_footnote(quality_notes) |>
    gt::tab_spanner(label = "unstd.",
                    columns =  c("B", "StdErr", starts_with("LL"), starts_with("UL"))) |>
    gt::tab_spanner(label = "std.",
                    columns = c("beta", "beta_LL", "beta_UL")) |>
    gt::tab_spanner(label = "sig.",
                    columns = c("t", "p")) |>
    gt::tab_spanner(label = "multicoll.",
                    columns = c(starts_with("TOL"), starts_with("VIF"))) |>
    gt::cols_label(StdErr = "SE B", beta_LL = "LL", beta_UL = "UL") |>
    gt::sub_missing()

  if (knitr::pandoc_to("docx")){

  tab_knit <- tab_knit |>
    gt::as_raw_html()
  }

  return(tab_knit)
}

