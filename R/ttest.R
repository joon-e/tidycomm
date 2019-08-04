# TO-DO: define levels, paired ttest
t_test <- function(data, group_var, ..., var.equal = TRUE) {

  # Get vars
  test_vars <- grab_vars(data, rlang::quos(...))

  # Get group var name
  group_var_str <- rlang::as_label(rlang::quo({{ group_var }}))

  # Drop unused levels (if data is filtered)
  data <- droplevels(data)

  # Get levels
  levels <- data %>%
    dplyr::pull({{ group_var }}) %>%
    unique()

  # Check
  if (length(levels) < 2) {
    stop("Grouping variable must have more than one level", call. = FALSE)
  } else if (length(levels) > 2) {
    warning(glue::glue("{group_var_str} has more than 2 levels, defaulting to first two ",
                       "({levels[1]} and {levels[2]}). ",
                       "Consider filtering your data."), call. = FALSE)
    data <- data %>%
      dplyr::filter({{ group_var }} %in% levels[1:2]) %>%
      droplevels()
  }

  # TO-DO: Check if group_var in test_vars


  # Prepare data
  levels <- levels[1:2]
  data <- dplyr::select(data, {{ group_var }}, !!!test_vars)

  # Main function
  purrr::map_dfr(test_vars, compute_t_test, data, {{ group_var }},
                 levels, var.equal)

}

#
compute_t_test <- function(test_var, data, group_var, levels, var.equal) {

  # Split data
  x <- data %>%
    dplyr::filter({{ group_var }} == levels[1]) %>%
    pull({{ test_var }})
  y <- data %>%
    dplyr::filter({{ group_var }} == levels[2]) %>%
    pull({{ test_var }})

  # Test
  tt <- t.test(x, y, var.equal = var.equal)

  # Get names
  level_names <- levels %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_sub(1, 10)
  M_str <- paste("M", level_names, sep = "_")
  SD_str <- paste("SD", level_names, sep = "_")
  test_var_str <- rlang::as_label(rlang::quo({{ test_var }}))

  tibble::tibble(
    Variable = test_var_str,
    !!M_str[1] := mean(x, na.rm = TRUE),
    !!SD_str[1] := sd(x, na.rm = TRUE),
    !!M_str[2] := mean(y, na.rm = TRUE),
    !!SD_str[2] := sd(y, na.rm = TRUE),
    Delta_M = mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE),
    t = tt$statistic,
    df = tt$parameter,
    p = tt$p.value,
    d_pooled = cohens_d(x, y)
  )
}


cohens_d <- function(x, y, pooled_sd = TRUE, na.rm = TRUE) {

  nx <- length(!is.na(x))
  ny <- length(!is.na(y))
  mx <- mean(x, na.rm = na.rm)
  my <- mean(y, na.rm = na.rm)
  varx <- var(x, na.rm = na.rm)
  vary <- var(y, na.rm = na.rm)

  if (pooled_sd) {
    s <- sqrt(((nx-1)*varx + (ny-1)*vary) / (nx + ny - 2))
  } else {
    s <- sqrt((varx + vary) / 2)
  }

  (mx - my) / s
}
