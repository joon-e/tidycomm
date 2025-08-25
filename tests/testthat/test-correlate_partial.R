# Main functions

test_that("correlate(partial = work_experience) returns tibble", {

  t <- WoJ %>% correlate(ethics_1, ethics_2,
                         partial = work_experience)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(1, 7))
})

test_that("correlate(partial = work_experience) returns tibble for other methods", {

  t1 <- WoJ %>% correlate(ethics_1, ethics_2,
                          partial = work_experience,
                          method = "spearman")
  t2 <- WoJ %>% correlate(ethics_1, ethics_2,
                          partial = work_experience,
                          method = "kendall")

  expect_true(tibble::is_tibble(t1))
  expect_true("rho" %in% names(t1))
  expect_equal(dim(t1), c(1, 7))

  expect_true(tibble::is_tibble(t2))
  expect_true("tau" %in% names(t2))
  expect_equal(dim(t2), c(1, 7))
})

test_that("correlate(partial = ethics_3) calculates correct results for Pearson correlations", {
  result <- WoJ %>% correlate(ethics_1, ethics_2,
                              partial = ethics_3)
  partial_cor_numerator <- cor(WoJ$ethics_1, WoJ$ethics_2, method = "pearson") -
    cor(WoJ$ethics_1, WoJ$ethics_3, method = "pearson") *
    cor(WoJ$ethics_2, WoJ$ethics_3, method = "pearson")
  partial_cor_denumerator <- sqrt((1-(cor(WoJ$ethics_1, WoJ$ethics_3, method = "pearson")^2))*(1-(cor(WoJ$ethics_3, WoJ$ethics_2, method = "pearson")^2)))
  partial_cor <- partial_cor_numerator / partial_cor_denumerator
  expect_equal(round(result$r[1], digits = 3), round(partial_cor, digits = 3))
})

## Possible errors

test_that("Error is thrown when number of provided variables is less than three", {
  expect_error(WoJ %>% correlate(ethics_1,
                                 partial = ethics_3),
               "The computation cannot be performed because there are not enough variables provided. Please provide exactly three variables for a partial correlation.")
})

test_that("Error is thrown when number of provided variables is more than three", {
  expect_error(WoJ %>% correlate(ethics_1, ethics_2, ethics_3, ethics_4,
                                 partial = work_experience),
               "The computation cannot be performed due to an excessive number of variables provided. Please provide exactly three variables for a partial correlation.")
})

test_that("Error is thrown when method is not one of 'pearson', 'kendall' or 'spearman'", {
  expect_error(WoJ %>% correlate(ethics_1, ethics_2,
                                 partial = work_experience,
                                 method = "wrong"),
               "Method must be one of \"pearson\", \"kendall\" or \"spearman\".")
})

test_that("Error is thrown when non-numeric variables are passed", {
  expect_error(fbposts %>% correlate(type, n_pictures, pop_elite,
                                     partial = work_experience))
})
