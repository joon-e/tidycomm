# Main functions

test_that("correlation test returns tibble", {

  t <- correlation_test(c("ethics_1", "ethics_2"), WoJ, "pearson")

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(1, 6))
})

test_that("correlate returns tibble", {

  t <- correlate(WoJ, ethics_1, ethics_2, ethics_3)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(3, 6))
})

test_that("correlate returns for other methods", {

  tt <- tibble::tibble(x = 1:10, y = c(2:5, 1, 6:10))

  t1 <- correlate(tt, method = "spearman")
  t2 <- correlate(tt, method = "kendall")

  expect_true(tibble::is_tibble(t1))
  expect_true("rho" %in% names(t1))
  expect_equal(dim(t1), c(1, 6))

  expect_true(tibble::is_tibble(t2))
  expect_true("tau" %in% names(t2))
  expect_equal(dim(t2), c(1, 6))
})

test_that("correlate returns tibble when no variables are specified", {

  t <- correlate(WoJ)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(55, 6))
})

test_that("correlate returns tibble with tidyselect helpers", {

  t <- correlate(WoJ, tidyselect::starts_with("ethics"))

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(6, 6))
})

## Possible errors

test_that("correlate produces warning when non-numeric variables are passed", {
  expect_warning(correlate(fbposts, type, n_pictures))
})

test_that("correlate stops when non-defined method is passed", {
  expect_error(correlate(fbposts, method = "correlate"))
})

# Correlation matrix

test_that("correlation matrix returns tibble", {

  t1 <- correlate(WoJ) %>%
    to_correlation_matrix()

  t2 <- correlate(WoJ, tidyselect::starts_with("ethics")) %>%
    to_correlation_matrix()

  expect_true(tibble::is_tibble(t1))
  expect_true(tibble::is_tibble(t2))

  expect_equal(dim(t1)[1], dim(t1)[2] - 1)
  expect_equal(dim(t2)[1], dim(t2)[2] - 1)
})
