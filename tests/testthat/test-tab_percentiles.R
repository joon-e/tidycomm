# Main functions

test_that("tab_percentiles calculates correct percentiles", {
  data <- WoJ
  result <- data %>% tab_percentiles(autonomy_emphasis)
  expect_equal(result$p10, quantile(data$autonomy_emphasis, 0.1, na.rm = TRUE))
})

test_that("tab_percentiles calculates custom percentile levels correctly", {
  data <- WoJ
  custom_levels <- c(0.25, 0.75)
  result <- data %>%  tab_percentiles(autonomy_emphasis, levels = custom_levels)

  expect_equal(result$p25, quantile(data$autonomy_emphasis, 0.25, na.rm = TRUE))
  expect_equal(result$p75, quantile(data$autonomy_emphasis, 0.75, na.rm = TRUE))
})

test_that("tab_percentiles stops when no numeric variables found", {
  data <- WoJ
    expect_error(data %>% tab_percentiles(country))
})

test_that("tab_percentiles describes all numeric variables when no variables are passed", {
  data <- WoJ
  result <- data %>% tab_percentiles()

  # Extract numeric variable names from the original data
  numeric_var_names <- data %>%
    dplyr::select(where(is.numeric)) %>%
    names() %>%
    sort()

  # Extract variable names from the result
  result_var_names <- result %>%
    dplyr::pull(Variable) %>%
    unique() %>%
    sort()

  # Check if all numeric variables are present in the result
  expect_equal(numeric_var_names, result_var_names)
})

test_that("tab_percentiles works with tidyselect helpers", {
  # Expect error when non-numeric variables are selected
  expect_error(WoJ %>% tab_percentiles(tidyselect::starts_with("temp_")))
})

test_that("tab_percentiles removes NAs in variables correctly", {
  result_with_na <- snscomments %>%
    tab_percentiles(medium_evaluation) %>%
    dplyr::pull()

  result_without_na <- snscomments %>%
    dplyr::filter(!is.na(medium_evaluation)) %>%
    tab_percentiles(medium_evaluation) %>%
    dplyr::pull()

  expect_equal(result_with_na, result_without_na)
})
