#' Compute linear regression
#'
#' Computes linear regression for all independent variables on the specified
#' dependent variable. Linear modeling of multiple independent variables uses
#' stepwise regression modeling. If specified, preconditions for
#' (multi-)collinearity and for homoscedasticity are checked.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param dependent_var The dependent variable on which the linear model is
#'   fitted. Specify as column name.
#' @param ... Independent variables to take into account as (one or many)
#'   predictors for the dependent variable. Specify as column names. At least
#'   one has to be specified.
#' @param check_independenterrors if set, the independence of errors among any
#'   two cases is being checked using a Durbin-Watson test
#' @param check_multicollinearity if set, multicollinearity among all specified
#'   independent variables is being checked using the variance inflation factor
#'   (VIF)
#' @param check_homoscedasticity if set, homoscedasticity is being checked
#'   using a Breusch-Pagan test
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples
#' WoJ %>% regress(autonomy_selection, ethics_1)
#' WoJ %>% regress(autonomy_selection, work_experience, trust_government)
#'
#' @export
regress <- function(data,
                    dependent_var,
                    ...,
                    check_independenterrors = FALSE,
                    check_multicollinearity = FALSE,
                    check_homoscedasticity = FALSE) {

  yvar <- expr({{ dependent_var }})
  xvars <- grab_vars(data, enquos(...), alternative = "none")

  yvar_string <- as_label(yvar)
  xvars_string <- purrr::map_chr(xvars, as_label)

  # basic checks
  if (length(xvars) == 0) {
    stop("No independent variable(s) given.")
  }

  count_columns <- data %>%
    dplyr::select(!!!xvars, !!yvar) %>%
    names() %>%
    length()

  count_columns_numeric <- data %>%
    dplyr::select(!!!xvars, !!yvar) %>%
    dplyr::select_if(is.numeric) %>%
    names() %>%
    length()

  if (count_columns_numeric < count_columns) {
    stop("At least one variable is not numeric.")
  }

  if (dplyr::is.grouped_df(data)) {
    warning("regress does not support grouped data. Groups will be dropped.",
            call. = FALSE)
    data <- dplyr::ungroup(data)
  }

  # main lm
  model_formula <- as.formula(sprintf("%s ~ %s",
                                      yvar_string,
                                      paste(xvars_string,
                                            collapse = " + ")))
  model <- lm(model_formula, data)
  model_summary <- summary(model)
  model_tibble <-
    tibble::tibble(
      Variable = c("(Intercept)", xvars_string),
      B = model_summary$coefficients[,1],
      StdErr = model_summary$coefficients[,2],
      beta = lm.beta::lm.beta(model)$standardized.coefficients,
      t = model_summary$coefficients[,3],
      p = model_summary$coefficients[,4]
    )

  # overall quality
  message(sprintf("F(%d, %d) = %f, p = %f, R² = %f",
                  model_summary$fstatistic[["numdf"]],
                  model_summary$fstatistic[["dendf"]],
                  model_summary$fstatistic[["value"]],
                  pf(model_summary$fstatistic[["value"]],
                     model_summary$fstatistic[["numdf"]],
                     model_summary$fstatistic[["dendf"]],
                     lower.tail = FALSE),
                  model_summary$r.squared))

  # preconditions
  if (check_independenterrors) {
    check_durbin_watson <- car::durbinWatsonTest(model)
    message(sprintf("- Check for independent errors: Durbin-Watson = %f (p = %f)",
                    check_durbin_watson$dw,
                    check_durbin_watson$p))
  }

  if (check_homoscedasticity) {
    check_breusch_pagan <- car::ncvTest(model)
    message(sprintf("- Check for homoscedasticity: Breusch-Pagan = %f (p = %f)",
                    check_breusch_pagan$ChiSquare,
                    check_breusch_pagan$p))
  }

  if (check_multicollinearity) {
    if (count_columns < 3) {
      warning(paste0("multicollinearity (VIF) checks are only applicable with ",
                     "2+ independent variables. No VIF will be computed."))
    } else {
      message("- Check for multicollinearity: VIF added to tibble")
      check_vif <- car::vif(model)
      model_tibble <- model_tibble %>%
        dplyr::bind_cols(tibble::tibble(VIF = c(NA, check_vif)))
    }
  }

  # return
  return(model_tibble)
}
