# Main functions

test_that("regress returns tibble for any input", {
  tt <- tibble::tibble(x = 1:10,
                       y = c(2:5, 1, 6:10))
  expected_columns <- c("Variable", "B", "SE B", "LL", "UL", "beta", "t", "p")

  t1 <- regress(tt, y, x)
  expect_true(tibble::is_tibble(t1))
  expect_true(all(expected_columns %in% names(t1)))
  expect_equal(dim(t1), c(2, 8))

  t2 <- regress(tt, y, x, check_independenterrors = TRUE)
  expect_true(tibble::is_tibble(t2))
  expect_true(all(expected_columns %in% names(t2)))
  expect_equal(dim(t2), c(2, 8))

  t3 <- regress(tt, y, x, check_independenterrors = TRUE)
  expect_true(tibble::is_tibble(t3))
  expect_true(all(expected_columns %in% names(t3)))
  expect_equal(dim(t3), c(2, 8))

  t4 <- regress(tt, y, x, check_homoscedasticity = TRUE)
  expect_true(tibble::is_tibble(t4))
  expect_true(all(expected_columns %in% names(t4)))
  expect_equal(dim(t4), c(2, 8))
})

test_that("regress returns tibble also when requesting multicoll./VIF", {
  tt <- tibble::tibble(x1 = 1:10,
                       x2 = c(40, seq(90, 10, -10)),
                       y = c(2:5, 1, 6:10))
  expected_columns <- c("Variable", "B", "SE B", "LL", "UL", "beta", "t", "p", "VIF", "TOL")

  t1 <- regress(tt, y, x1, x2, check_multicollinearity = TRUE)
  expect_true(tibble::is_tibble(t1))
  expect_true(all(expected_columns %in% names(t1)))
  expect_equal(dim(t1), c(3, 10))

  t2 <- regress(tt, y, x1, x2,
                check_independenterrors = TRUE,
                check_multicollinearity = TRUE,
                check_homoscedasticity = TRUE)
  expect_true(tibble::is_tibble(t2))
  expect_true(all(expected_columns %in% names(t2)))
  expect_equal(dim(t2), c(3, 10))
})

test_that("regress returns tibble with tidyselect helpers", {
  tt <- tibble::tibble(x1 = 1:10,
                       x2 = c(40, seq(90, 10, -10)),
                       y = c(2:5, 1, 6:10))

  t <- regress(tt, y, tidyselect::starts_with("x"))

  expect_true(tibble::is_tibble(t))
  expect_true(all(c("Variable", "B", "SE B", "LL", "UL", "beta", "t", "p") %in% names(t)))
  expect_equal(dim(t), c(3, 8))
})


test_that("regress can handle factors", {
  tt <- tibble::tibble(x1 = 1:10,
                       x2 = as.factor(c(1, 1, 1, 2, 2, 1, 2, 1, 1, 2)),
                       y = c(2:5, 1, 6:10))

  t <- regress(tt, y, tidyselect::starts_with("x"))

  expect_true(tibble::is_tibble(t))
  expect_true(all(c("Variable", "B", "SE B", "LL", "UL", "beta", "t", "p") %in% names(t)))
  expect_equal(dim(t), c(3, 8))


  expect_warning(regress(tt,
                         y,
                         tidyselect::starts_with("x"),
                         check_multicollinearity = TRUE))
})


## Possible errors
test_that("regress produces warning when data is grouped", {
  expect_warning(regress(WoJ %>% dplyr::group_by(employment),
                         autonomy_selection,
                         ethics_1))
})

test_that("regress produces error when (in)dependent variables are missing", {
  expect_error(regress(fbposts, n_pictures))
})

test_that("regress produces error when non-numeric variables are passed", {
  tt <- tibble::tibble(x = as.character(1:10),
                       y = c(2:5, 1, 6:10))
  expect_error(regress(tt, y, x))

  tt <- tibble::tibble(x = 1:10,
                       y = as.character(c(2:5, 1, 6:10)))
  expect_error(regress(tt, y, x))
})
