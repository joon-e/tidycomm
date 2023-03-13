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
#'   (VIF) and the tolerance (1/VIF); this check can only be performed if at
#'   least two independent variables are provided, and all provided variables
#'   need to be numeric
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

  yvar_numeric <- data %>%
    dplyr::select(!!yvar) %>%
    dplyr::select_if(is.numeric) %>%
    names() %>%
    length()

  if (yvar_numeric < 1) {
    stop("Dependent variable must be numeric.")
  }

  xvars_count <- data %>%
    dplyr::select(!!!xvars) %>%
    names() %>%
    length()

  xvars_count_numeric <- data %>%
    dplyr::select(!!!xvars) %>%
    dplyr::select_if(is.numeric) %>%
    names() %>%
    length()

  xvars_count_factor <- data %>%
    dplyr::select(!!!xvars) %>%
    dplyr::select_if(is.factor) %>%
    names() %>%
    length()

  if ((xvars_count_factor + xvars_count_numeric) < xvars_count) {
    stop("At least one independent variable is neither numeric nor a factor.")
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
      Variable = dimnames(model_summary$coefficients)[[1]],
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
    if (xvars_count < 2) {
      warning(paste0("multicollinearity (VIF) checks are only applicable with ",
                     "2+ independent variables. No VIF will be computed."))
    } else {
      if (xvars_count_factor > 0) {
        warning(paste0("multicollinearity (VIF) checks are only applicable ",
                       "to numeric (rather than factorial) independent ",
                       "variables. No VIF will be computed."))
      } else {
        check_vif <- car::vif(model)
        model_tibble <- model_tibble %>%
          dplyr::bind_cols(tibble::tibble(VIF = c(NA,
                                                  check_vif),
                                          tolerance = c(NA,
                                                        1/check_vif)))
        message("- Check for multicollinearity: VIF/tolerance added to output")
      }
    }
  }

  if (xvars_count_factor > 0) {
    for (xvar in xvars_string) {
      if (is.factor(data[[xvar]])) {
        message(sprintf(paste0("- %s is a factor and was split into one ",
                               "variable per level, each compared to '%s' (",
                               "reference level)"),
                        xvar,
                        levels(data[[xvar]])[[1]]))
      }
    }
  }

  # return
  return(model_tibble)
}