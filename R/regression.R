#' Compute linear regression
#'
#' Computes linear regression for all independent variables on the specified
#' dependent variable. Linear modeling of multiple independent variables uses
#' stepwise regression modeling. If specified, preconditions for
#' (multi-)collinearity and for homoscedasticity are checked.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
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
#' @param formula if exist, the lm-formula is used
#'
#' @return a [tdcmm] model
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

  # Check if 'data' is an lm model
  if (inherits(data, "lm")) {
    # Extract the formula and the data from the lm model
    model <- data
    model_formula <- stats::formula(model)  # Extract the formula from the model
    data <- dplyr::as_tibble(model$model)  # Extract the data from the model
    yvar_string <- all.vars(model_formula)[1]  # Dependent variable
    xvars_string <- all.vars(model_formula)[-1]  # Independent variables

    # Create dummy placeholders for yvar and xvars to ensure compatibility
    yvar <- rlang::sym(yvar_string)  # Convert yvar_string back to symbol
    xvars <- rlang::syms(xvars_string)  # Convert xvars_string back to symbols

  } else {
    # Original processing for when data and variables are provided separately
    yvar <- expr({{ dependent_var }})
    xvars <- grab_vars(data, enquos(...), alternative = "none")

    yvar_string <- as_label(yvar)  # Convert dependent variable to string
    xvars_string <- purrr::map_chr(xvars, as_label)  # Convert independent variables to string
  }

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

  model <- stats::lm(model_formula, data)

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

    # checks
  model_checks <- list()

  if (check_independenterrors) {
    check_durbin_watson <- car::durbinWatsonTest(model)
    model_checks[['independenterrors']] <- check_durbin_watson
  }

  if (check_homoscedasticity) {
    check_breusch_pagan <- car::ncvTest(model)
    model_checks[['homoscedasticity']] <- check_breusch_pagan
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
                                          TOL = c(NA,
                                                        1/check_vif)))
        model_checks[['multicollinearity']] <- check_vif
      }
    }
  }

  checks_factors <- list()
  if (xvars_count_factor > 0) {
    for (xvar in xvars_string) {
      if (is.factor(data[[xvar]])) {
        checks_factors[[length(checks_factors) + 1]] <-
          c(xvar, levels(data[[xvar]])[[1]])
      }
    }
  }
  model_checks[['factors']] <- checks_factors

  # return ----
  return(new_tdcmm_rgrssn(
    new_tdcmm(model_tibble,
              func = "regress",
              data = data,
              params = list(dependent_var = yvar_string,
                            vars = xvars_string,
                            check_independenterrors = check_independenterrors,
                            check_multicollinearity = check_multicollinearity,
                            check_homoscedasticity = check_homoscedasticity),
              model = list(model),
              checks = model_checks))
  )
}

#' @param which string to specify type of regression visualization. One of
#' "jitter" (default), "alpha", "correlogram", "residualsfitted" (or "resfit"),
#' "pp", "qq", "scalelocation" (or "scaloc"), "residualsleverage" (or "reslev").
#' See below for details.
#'
#' @rdname visualize
#' @export
visualize.tdcmm_rgrssn <- function(x,
                                   which = "jitter",
                                   ...,
                                   .design = design_lmu()) {
  if (attr(x, "func") == "regress") {
    which <- tolower(which)
    if (which == "correlogram") {
      return(visualize_regress_correlogram(x, .design))
    }
    if (which == "residualsfitted" | which == "resfit") {
      return(visualize_regress_resfit(x, .design))
    }
    if (which == "pp") {
      return(visualize_regress_pp(x, .design))
    }
    if (which == "qq") {
      return(visualize_regress_qq(x, .design))
    }
    if (which == "scalelocation" | which == "scaloc") {
      return(visualize_regress_scaloc(x, .design))
    }
    if (which == "residualsleverage" | which == "reslev") {
      return(visualize_regress_reslev(x, .design))
    }
    if (!which %in% c("jitter", "alpha")) {
      warning(glue('which must be one of "jitter", "alpha", "correlogram", ',
                   '"residualsfitted" (or "resfit"), "pp", "qq", ',
                   '"scalelocation" (or "scaloc"), ',
                   'or "residualsleverage" (or "reslev"). Since none was ',
                   'provided, "jitter" is considered by default.'),
              call. = FALSE)
      which <- "jitter"
    }
    return(visualize_regress_lm(x, which, .design))
  }

  return(warn_about_missing_visualization(x))
}


# Formatting ----

#' @export
tbl_format_footer.tdcmm_rgrssn <- function(x, ...) {
  default_footer <- NextMethod()
  footers <- c(default_footer)

  # Get values
  model <- model(x)
  model_summary <- summary(model)
  model_checks <- attr(x, "checks")
  model_check_names <- names(model_checks)

  # overall quality
  quality_footer <- sprintf("F(%d, %d) = %f, p = %f, R-square = %f",
                            model_summary$fstatistic[["numdf"]],
                            model_summary$fstatistic[["dendf"]],
                            model_summary$fstatistic[["value"]],
                            pf(model_summary$fstatistic[["value"]],
                               model_summary$fstatistic[["numdf"]],
                               model_summary$fstatistic[["dendf"]],
                               lower.tail = FALSE),
                            model_summary$r.squared)
  footers <- c(footers, glue("# {quality_footer}"))

  # checks
  if ("independenterrors" %in% model_check_names) {
    durbin_watson_footer <- sprintf(paste0("Check for independent errors: ",
                                           "Durbin-Watson = %f (p = %f)"),
                                    model_checks$independenterrors$dw,
                                    model_checks$independenterrors$p)
    footers <- c(footers, glue("- {durbin_watson_footer}"))
  }

  if ("homoscedasticity" %in% model_check_names) {
    breusch_pagan_footer <- sprintf(paste0("Check for homoscedasticity: ",
                                           "Breusch-Pagan = %f (p = %f)"),
                                    model_checks$homoscedasticity$ChiSquare,
                                    model_checks$homoscedasticity$p)
    footers <- c(footers, glue("- {breusch_pagan_footer}"))
  }

  if ("multicollinearity" %in% model_check_names) {
    footers <- c(footers,
                 "- Check for multicollinearity: VIF/tolerance added to output")
  }

  if (length(model_checks$factors) > 0) {
    for (i in 1:length(model_checks$factors)) {
      factor_footer <- sprintf(paste0("%s is a factor and was split into one ",
                                      "variable per level, each compared to ",
                                      "'%s' (reference level)"),
                               model_checks$factors[[i]][[1]],
                               model_checks$factors[[i]][[2]])
      footers <- c(footers, glue("- {factor_footer}"))
    }
  }

  style_subtle(footers)
}

### Internal functions ###

## Visualize as scatter plot with linear model.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_regress_lm <- function(x, which = "jitter", design = design_lmu()) {
  g <- attr(x, "data") %>%
    dplyr::select(!!sym(attr(x, "params")$dependent_var),
                  !!!syms(attr(x, "params")$vars)) %>%
    dplyr::rename(dependent_var = !!sym(attr(x, "params")$dependent_var)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.factor),
                                as.numeric)) %>%
    tidyr::pivot_longer(c(!!!syms(attr(x, "params")$vars)),
                        names_to = "iv") %>%
    dplyr::mutate(iv = forcats::fct(.data$iv,
                                    levels = attr(x, "params")$vars)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$value,
                                 y = .data$dependent_var))
  if (which == "jitter") {
    g <- g +
      ggplot2::geom_jitter(width = .3,
                           height = .3,
                           na.rm = TRUE)
  } else {
    g <- g +
      ggplot2::geom_point(alpha = .25,
                          na.rm = TRUE)
  }

  g +
    ggplot2::geom_smooth(method = "lm",
                         se = TRUE,
                         level = .95,
                         formula = "y ~ x",
                         na.rm = TRUE,
                         color = design$main_color_1,
                         linewidth = design$main_size) +
    ggplot2::facet_wrap(dplyr::vars(.data$iv),
                        scales = "free_x") +
    ggplot2::scale_x_continuous(NULL) +
    ggplot2::scale_y_continuous(attr(x, "params")$dependent_var) +
    design$theme()
}

## Visualize as correlogram between independent variables.
## Helps to determine indpendent errors and multicollinearity.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @see visualize.tdcmm_crrltn
#
## @keywords internal
visualize_regress_correlogram <- function(x, design = design_lmu()) {
  if (length(attr(x, "checks")$factors) > 0) {
    factor_variables <- c()
    for (i in 1:length(attr(x, "checks")$factors)) {
      factor_variables <- c(factor_variables,
                            attr(x, "checks")$factors[[i]][[1]])
    }
    warning(glue("only numeric variables will be included in this plot, ",
                 "all factor variables (",
                 paste(factor_variables, collapse = ", "),
                 ") have been dropped for this visualization"),
            call. = FALSE)
  }
  attr(x, "data") %>%
    dplyr::select(!!!syms(attr(x, "params")$vars)) %>%
    dplyr::select_if(is.numeric) %>%
    correlate() %>%
    to_correlation_matrix() %>%
    visualize(.design = design)
}

## Visualize as residuals v. fitted plot.
## Useful for determining distributions.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @keywords internal
visualize_regress_resfit <- function(x, design = design_lmu()) {
  m <- model(x)
  mf <- ggplot2::fortify(m)
  lowess_fit <- dplyr::as_tibble(stats::lowess(x = mf$.fitted,
                                               y = mf$.resid))
  mf %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.fitted,
                                 y = .data$.resid)) +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = design$comparison_linetype,
                        color = design$comparison_color,
                        linewidth = design$main_size) +
    ggplot2::geom_point() +
    ggplot2::geom_path(data = lowess_fit,
                       ggplot2::aes(x = .data$x,
                                    y = .data$y),
                       color = design$main_color_1,
                       linewidth = design$main_size) +
    ggplot2::scale_x_continuous("Fitted",
                                n.breaks = 8) +
    ggplot2::scale_y_continuous("Residuals",
                                n.breaks = 8) +
    design$theme(panel.grid = ggplot2::element_blank())
}

## Visualize as probability-probability plot.
## Useful for checking multicollinearity (focus on the IQR).
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_regress_pp <- function(x, design = design_lmu()) {
  x %>%
    model() %>%
    ggplot2::fortify() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(.p_theoretical = (1:dplyr::n())/dplyr::n()-.5/dplyr::n(),
                  .p_sample = sort(stats::pnorm(.data$.stdresid,
                                                mean(.data$.stdresid),
                                                sd(.data$.stdresid)))) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.p_theoretical,
                                 y = .data$.p_sample)) +
    ggplot2::geom_abline(intercept = 0,
                         slope = 1,
                         color = design$comparison_color,
                         linewidth = design$comparison_size,
                         linetype = design$comparison_linetype) +
    ggplot2::geom_point(alpha = .25) +
    ggplot2::scale_x_continuous("Theoretical Probability",
                                n.breaks = 8) +
    ggplot2::scale_y_continuous("Sample Residual Probability",
                                n.breaks = 8) +
    design$theme(panel.grid = ggplot2::element_blank())
}

## Visualize as quantile-quantile plot.
## Useful for checking multicollinearity (focus on outliers).
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_regress_qq <- function(x, design = design_lmu()) {
  x %>%
    model() %>%
    ggplot2::ggplot(ggplot2::aes(sample = .data$.stdresid)) +
    ggplot2::stat_qq(alpha = .25) +
    ggplot2::geom_qq_line(color = design$main_color_1,
                          linewidth = design$main_size) +
    ggplot2::scale_x_continuous("Theoretical Quantiles",
                                n.breaks = 8) +
    ggplot2::scale_y_continuous("Sample Quantiles",
                                n.breaks = 8) +
    design$theme(panel.grid = ggplot2::element_blank())
}

## Visualize as scale-location plot.
## Useful for checking homoscedasticity.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @keywords internal
visualize_regress_scaloc <- function(x, design = design_lmu()) {
  x %>%
    model() %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.fitted,
                                 y = sqrt(abs(.data$.stdresid)))) +
    ggplot2::geom_point(na.rm = T,
                        alpha = .25) +
    ggplot2::stat_smooth(method = "loess",
                         se = FALSE,
                         color = design$main_color_1,
                         linewidth = design$main_size,
                         formula = "y ~ x") +
    ggplot2::scale_x_continuous("Fitted Values",
                                n.breaks = 8) +
    ggplot2::scale_y_continuous(expression(sqrt("|Standardized Residuals|")),
                                n.breaks = 8) +
    design$theme(panel.grid = ggplot2::element_blank())
}

## Visualize as residuals v. leverage plot.
## Useful for checking for influential single cases ("outliers").
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @keywords internal
visualize_regress_reslev <- function(x, design = design_lmu()) {
  data_labels <- x %>%
    model() %>%
    ggplot2::fortify() %>%
    dplyr::as_tibble() %>%
    tibble::rownames_to_column(var = "case_number") %>%
    dplyr::slice_max(.data$.cooksd,
                     n = 5)
  x %>%
    model() %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.hat,
                                 y = .data$.stdresid)) +
    ggplot2::geom_point(alpha = .25) +
    ggplot2::geom_smooth(method = "loess",
                         se = FALSE,
                         color = design$main_color_1,
                         linewidth = design$main_size,
                         formula = "y ~ x") +
    ggplot2::geom_text(data = data_labels,
                       ggplot2::aes(label = .data$case_number),
                       check_overlap = TRUE,
                       nudge_x = .00075,
                       color = design$main_color_1) +
    ggplot2::scale_x_continuous("Leverage",
                                n.breaks = 8) +
    ggplot2::scale_y_continuous("Standardized Residuals",
                                n.breaks = 8) +
    design$theme(panel.grid = ggplot2::element_blank())
}

# Constructors ----

new_tdcmm_rgrssn <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_rgrssn", class(x))
  )
}
