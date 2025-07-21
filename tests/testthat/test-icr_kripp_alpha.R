# Main functions

test_that("Krippendorff's Alpha returns correct result with numeric data", {

  icr_mat <- matrix(c(c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
                      c(1, 2, 3 ,3 ,2 , 2, 4, 1, 2, 5, NA, 3),
                      c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, NA),
                      c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA)),
                    nrow = 12,
                    byrow = FALSE)

  expect_equal(icr_kripp_alpha(icr_mat), 0.743, tolerance = .0006)
  expect_equal(icr_kripp_alpha(icr_mat, var_level = "ordinal"), 0.815, tolerance = .0005)
  expect_equal(icr_kripp_alpha(icr_mat, var_level = "interval"), 0.849, tolerance = .0005)
  expect_equal(icr_kripp_alpha(icr_mat, var_level = "ratio"), 0.797, tolerance = .0006)
})

test_that("Krippendorff's Alpha returns correct result with nominal numeric data", {

  icr_mat <- matrix(c(c(6, 99, 3, 3, 99, 6, 4, 6, 99, NA, NA, NA),
                      c(6, 99, 3 ,3 ,99 , 99, 4, 6, 99, 5, NA, 3),
                      c(NA, 3, 3, 3, 99, 3, 4, 99, 99, 5, 6, NA),
                      c(6, 99, 3, 3, 99, 4, 4, 6, 99, 5, 6, NA)),
                    nrow = 12,
                    byrow = FALSE)

  expect_equal(icr_kripp_alpha(icr_mat), 0.743, tolerance = .0006)
})

test_that("Krippendorff's Alpha returns correct result with nominal string data", {

  icr_mat <- matrix(c(c("one", "two", "three", "three", "two", "one", "four", "one", "two", NA, NA, NA),
                      c("one", "two", "three" ,"three" ,"two" , "two", "four", "one", "two", "five", NA, "three"),
                      c(NA, "three", "three", "three", "two", "three", "four", "two", "two", "five", "one", NA),
                      c("one", "two", "three", "three", "two", "four", "four", "one", "two", "five", "one", NA)),
                    nrow = 12,
                    byrow = FALSE)

  expect_equal(icr_kripp_alpha(icr_mat), 0.743, tolerance = .0006)
})

test_that("Krippendorff's Alüha returns 1 for variables with one category", {

  icr_mat <- matrix(rep(1, 30), nrow = 10)

  expect_equal(icr_kripp_alpha(icr_mat), 1)

})

# Helper functions

test_that("Count value in unit works", {

  unit1 <- c(1, 2, 1, 4, 5, 1, 2, 5)
  unit2 <- c("one", "two", "one", "four", "five", "one", "two", "five")
  tab1 <- table(unit1)
  tab2 <- table(unit2)

  expect_equal(count_value_in_unit(1, tab1), 3)
  expect_equal(count_value_in_unit(9, tab1), 0)
  expect_equal(count_value_in_unit("one", tab2), 3)
  expect_equal(count_value_in_unit("nine", tab2), 0)

})

test_that("Values in unit works", {

  unit1 <- c(1, 2, 1, 4, 5, 1, 2, 5)
  unit2 <- c("one", "two", "one", "four", "five", "one", "two", "five")

  expect_equal(values_in_unit(unit1, c(1, 2, 9)), c(3, 2, 0))
  expect_equal(values_in_unit(unit2, c("one", "two", "nine")), c(3, 2, 0))

})

test_that("Delta-square function returns correct results", {

  vum <- matrix(c(3, 0, 0, 0, 0, 1, 0, 3, 0, 0, 2, 0,
                  0, 3, 0, 0, 4, 1, 0, 1, 4, 0, 0, 0,
                  0, 1, 4, 4, 0, 1, 0, 0, 0, 0, 0, 1,
                  0, 0, 0, 0, 0, 1, 4, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0),
                nrow = 5, byrow = TRUE)
  vum <- vum[, apply(vum, 2, sum) > 1]

  expect_equal(delta_sq(1, 1, var_level = "nominal"), 0)
  expect_equal(delta_sq(1, 2, var_level = "nominal"), 1)
  expect_equal(delta_sq(1, 1, vum, var_level = "ordinal"), 0)
  expect_equal(delta_sq(1, 2, vum, var_level = "ordinal"), 121)
  expect_equal(delta_sq(1, 1, var_level = "interval"), 0)
  expect_equal(delta_sq(1, 4, var_level = "interval"), 9)
  expect_equal(delta_sq(1, 1, var_level = "ratio"), 0)
  expect_equal(delta_sq(1, 4, var_level = "ratio"), .36)

})

test_that("Numerator function returns correct results", {

  vum <- matrix(c(3, 0, 0, 0, 0, 1, 0, 3, 0, 0, 2, 0,
                  0, 3, 0, 0, 4, 1, 0, 1, 4, 0, 0, 0,
                  0, 1, 4, 4, 0, 1, 0, 0, 0, 0, 0, 1,
                  0, 0, 0, 0, 0, 1, 4, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0),
                nrow = 5, byrow = TRUE)
  vum <- vum[, apply(vum, 2, sum) > 1]

  expect_equal(kalpha_num_values(c(3, 0, 0, 0, 0), 1:5, vum, "nominal"), 0)
  expect_equal(kalpha_num_values(c(1, 1, 1, 1, 0), 1:5, vum, "nominal"), 2)
  expect_equal(kalpha_num_values(c(1, 1, 1, 1, 0), 1:5, vum, "interval"), 6.667, tolerance = .0005)
})

test_that("Denominator function returns correct results", {

  vum <- matrix(c(3, 0, 0, 0, 0, 1, 0, 3, 0, 0, 2, 0,
                  0, 3, 0, 0, 4, 1, 0, 1, 4, 0, 0, 0,
                  0, 1, 4, 4, 0, 1, 0, 0, 0, 0, 0, 1,
                  0, 0, 0, 0, 0, 1, 4, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0),
                nrow = 5, byrow = TRUE)
  vum <- vum[, apply(vum, 2, sum) > 1]

  expect_equal(kalpha_denom(vum, 1:5, "nominal"), 608)
  expect_equal(kalpha_denom(vum, 1:5, "interval"), 2240)
})
