context("ICR: Agreement")

test_that("Check equal works", {
  expect_true(check_equal(c(1, 1, 1, 1, 1)))
  expect_false(check_equal(c(0, 1, 1, 1, 1)))
  expect_false(check_equal(c(1, 2, 3, 4, 5)))
  expect_true(check_equal(c("one", "one", "one", "one", "one")))
  expect_false(check_equal(c("zero", "one", "one", "one", "one")))
})

test_that("Agreement computes correct result", {
  m1 <- matrix(c(1, 2, 4, 1, 2, 1, 1, 1, 2,
                 1, 2, 3, 1, 2, 1, 1, 2, 2,
                 1, 2, 3, 1, 2, 2, 1, 1, 2),
               ncol = 3)

  m2 <- matrix(c("one", "two", "four", "one", "two", "one", "one", "one", "two",
                 "one", "two", "three", "one", "two", "one", "one", "two", "two",
                 "one", "two", "three", "one", "two", "two", "one", "one", "two"),
               ncol = 3)

  expect_equal(icr_agreement(m1), 2/3)
  expect_equal(icr_agreement(m2), 2/3)
})
