context("Partial Correlation")

# Main functions

test_that("correlate(partial=TRUE) returns tibble", {

  t <- WoJ %>% correlate(ethics_1, ethics_2, ethics_3,
                         partial = TRUE)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(3, 7))
})

test_that("correlate(partial=TRUE) returns tibble for other methods", {

  t1 <- WoJ %>% correlate(ethics_1, ethics_2, ethics_3,
                          partial = TRUE,
                          method = "spearman")
  t2 <- WoJ %>% correlate(ethics_1, ethics_2, ethics_3,
                          partial = TRUE,
                          method = "kendall")

  expect_true(tibble::is_tibble(t1))
  expect_true("rho" %in% names(t1))
  expect_equal(dim(t1), c(3, 7))

  expect_true(tibble::is_tibble(t2))
  expect_true("tau" %in% names(t2))
  expect_equal(dim(t2), c(3, 7))
})

test_that("correlate(partial=TRUE) calculates correct results for Pearson correlations", {
  result <- WoJ %>% correlate(ethics_1, ethics_2, ethics_3,
                              partial = TRUE)
  partial_cor_numerator <- cor(WoJ$ethics_1, WoJ$ethics_2, method = "pearson") -
                 cor(WoJ$ethics_1, WoJ$ethics_3, method = "pearson") *
                 cor(WoJ$ethics_2, WoJ$ethics_3, method = "pearson")
  partial_cor_denumerator <- sqrt((1-(cor(WoJ$ethics_1, WoJ$ethics_3, method = "pearson")^2))*(1-(cor(WoJ$ethics_3, WoJ$ethics_2, method = "pearson")^2)))
  partial_cor <- partial_cor_numerator / partial_cor_denumerator
  expect_equal(round(result$r[1], digits = 3), round(partial_cor, digits = 3))
})

test_that("correlate(partial=TRUE) works with tidyselect helpers", {
  expect_error(WoJ %>% correlate(tidyselect::starts_with("ethics"),
                                 partial = TRUE),
               "The computation cannot be performed due to an excessive number of variables provided. Please provide exactly three variables.")
  t1 <- WoJ %>% correlate(tidyselect::num_range("ethics_", 1:3), partial = TRUE)
  expect_equal(dim(t1), c(3, 7))
})

## Possible errors

test_that("Error is thrown when number of provided variables is less than three", {
  expect_error(WoJ %>% correlate(ethics_1, ethics_2,
                                 partial = TRUE),
               "The computation cannot be performed because there are not enough variables provided. Please provide exactly three variables.")
})

test_that("Error is thrown when number of provided variables is more than three", {
  expect_error(WoJ %>% correlate(ethics_1, ethics_2, ethics_3, ethics_4,
                                 partial = TRUE),
               "The computation cannot be performed due to an excessive number of variables provided. Please provide exactly three variables.")
})

test_that("Error is thrown when method is not one of 'pearson', 'kendall' or 'spearman'", {
  expect_error(WoJ %>% correlate(ethics_1, ethics_2, ethics_3,
                                 partial = TRUE,
                                 method = "wrong"),
               "Method must be one of \"pearson\", \"kendall\" or \"spearman\".")
})

test_that("Error is thrown when non-numeric variables are passed", {
  expect_error(fbposts %>% correlate(type, n_pictures, pop_elite,
                                     partial = TRUE))
})
