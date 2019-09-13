context("Correlation")

test_that("correlation test returns tibble", {

  t <- correlation_test(c("ethics_1", "ethics_2"), WoJ, "pearson")

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(1, 5))
})

test_that("correlate returns tibble", {

  t <- correlate(WoJ, ethics_1, ethics_2, ethics_3)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(3, 5))
})

test_that("correlate returns tibble when no variables are specified", {

  t <- correlate(WoJ)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(55, 5))
})

test_that("correlate returns tibble with tidyselect helpers", {

  t <- correlate(WoJ, tidyselect::starts_with("ethics"))

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(6, 5))
})

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
