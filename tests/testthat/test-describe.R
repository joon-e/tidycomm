# Main function

test_that("describe returns tibble", {

  t <- describe(WoJ, ethics_1, ethics_2, ethics_3)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(3, 15))
})

test_that("describe works without specifying variables", {

  t <- describe(WoJ)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(11, 15))
})

test_that("describe works with tidyselect helpers", {

  t <- describe(WoJ, tidyselect::starts_with("ethics"))

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(4, 15))
})

# Describe_cat function

test_that("describe returns tibble", {

  t <- describe_cat(WoJ, reach, employment)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(2, 6))
})

test_that("describe works without specifying variables", {

  t <- describe_cat(WoJ)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(4, 6))
})

test_that("describe works with tidyselect helpers", {

  t <- describe_cat(WoJ, tidyselect::contains("country"))

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(1, 6))
})


# Skewness & Kurtosis helpers

test_that("skewness returns correct results", {
  x1 <- c(0:10)
  x2 <- c(0:10, 50)
  x3 <- c(rep(1, 5), 1:10)

  expect_equal(skewness(x1), 0, tolerance = .0005)
  expect_equal(skewness(x2), 2.717, tolerance = .0005)
  expect_equal(skewness(x3), 0.569, tolerance = .0005)
})

test_that("kurtosis returns correct results", {
  x1 <- c(0:10)
  x2 <- c(0:10, 50)
  x3 <- c(rep(1, 5), 1:10)

  expect_equal(kurtosis(x1), 1.780, tolerance = .0005)
  expect_equal(kurtosis(x2), 9.028, tolerance = .0005)
  expect_equal(kurtosis(x3), 1.852, tolerance = .0005)
})
