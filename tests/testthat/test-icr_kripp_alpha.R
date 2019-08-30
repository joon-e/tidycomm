context("ICR: Krippendorff's Alpha")

# Main functions

test_that("Krippendorff's Alpha works with numeric data", {

  icr_mat <- matrix(c(c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
                      c(1, 2, 3 ,3 ,2 , 2, 4, 1, 2, 5, NA, 3),
                      c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, NA),
                      c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA)),
                    nrow = 12,
                    byrow = FALSE)

  expect_equal(icr_kripp_alpha(icr_mat), 0.743, tolerance = .001)
  expect_equal(icr_kripp_alpha(icr_mat, var_level = "ordinal"), 0.815, tolerance = .001)
  expect_equal(icr_kripp_alpha(icr_mat, var_level = "interval"), 0.849, tolerance = .001)
  expect_equal(icr_kripp_alpha(icr_mat, var_level = "ratio"), 0.797, tolerance = .001)
})

test_that("Krippendorff's Alpha works with nominal numeric data", {

  icr_mat <- matrix(c(c(6, 99, 3, 3, 99, 6, 4, 6, 99, NA, NA, NA),
                      c(6, 99, 3 ,3 ,99 , 99, 4, 6, 99, 5, NA, 3),
                      c(NA, 3, 3, 3, 99, 3, 4, 99, 99, 5, 6, NA),
                      c(6, 99, 3, 3, 99, 4, 4, 6, 99, 5, 6, NA)),
                    nrow = 12,
                    byrow = FALSE)

  expect_equal(icr_kripp_alpha(icr_mat), 0.743, tolerance = .001)
})

test_that("Krippendorff's Alpha works with nominal string data", {

  icr_mat <- matrix(c(c("one", "two", "three", "three", "two", "one", "four", "one", "two", NA, NA, NA),
                      c("one", "two", "three" ,"three" ,"two" , "two", "four", "one", "two", "five", NA, "three"),
                      c(NA, "three", "three", "three", "two", "three", "four", "two", "two", "five", "one", NA),
                      c("one", "two", "three", "three", "two", "four", "four", "one", "two", "five", "one", NA)),
                    nrow = 12,
                    byrow = FALSE)

  expect_equal(icr_kripp_alpha(icr_mat), 0.743, tolerance = .001)
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
