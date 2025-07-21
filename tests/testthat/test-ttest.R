# Main functions

test_that("t_test returns tibble", {

  t1 <- t_test(WoJ, temp_contract, autonomy_selection, autonomy_emphasis)
  t2 <- t_test(WoJ, autonomy_emphasis, mu = 4.00)

  expect_true(tibble::is_tibble(t1))
  expect_equal(dim(t1), c(2, 12))

  expect_true(tibble::is_tibble(t2))
  expect_equal(dim(t2), c(1, 9))
})

test_that("t_test returns tibble when no variables are specified", {

  t <- t_test(WoJ, temp_contract)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(11, 12))
})

test_that("t_test returns tibble with user-defined levels", {

  t <- t_test(WoJ, employment, levels = c("Full-time", "Part-time"))

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(11, 12))
})

test_that("one-sample t_test does not interfere", {

  expect_error(t_test(WoJ, temp_contract, autonomy_emphasis, mu = 4.00))
  expect_error(t_test(WoJ, temp_contract, mu = 4.00))
})

test_that("t_test works with spaces in column names", {
  t1 <- WoJ %>%
    dplyr::select(temp_contract, `autonomy selection` = autonomy_selection) %>%
    t_test(temp_contract)

  expect_equal(dim(t1), c(1, 12))
})


# Helper functions

test_that("cohen's d returns correct results", {

  x <- WoJ %>%
    dplyr::filter(temp_contract == "Permanent") %>%
    dplyr::pull(autonomy_selection)
  y <- WoJ %>%
    dplyr::filter(temp_contract == "Temporary") %>%
    dplyr::pull(autonomy_selection)

  expect_equal(cohens_d(x, y), 0.277, tolerance = .006)
  expect_equal(cohens_d(x, y, pooled_sd = FALSE), 0.250, tolerance = .0005)
})
