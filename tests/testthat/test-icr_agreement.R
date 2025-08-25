test_that("Check equal works", {
  expect_true(check_equal(c(1, 1, 1, 1, 1)))
  expect_false(check_equal(c(0, 1, 1, 1, 1)))
  expect_false(check_equal(c(1, 2, 3, 4, 5)))
  expect_true(check_equal(c("one", "one", "one", "one", "one")))
  expect_false(check_equal(c("zero", "one", "one", "one", "one")))
})

test_that("Check equal works with tolerances", {
  expect_false(check_equal(c(0.9, 1, 1.1), tol = 0.1))
  expect_true(check_equal(c(0.9, 1, 1.1), tol = 0.2))
})

test_that("Agreement computes correct result", {
  m1n <- matrix(c(1, 2, 4, 1, 2, 1, 1, 1, 2,
                  1, 2, 3, 1, 2, 2, 1, 1, 2),
                ncol = 2)

  m1s <- matrix(c("one", "two", "four", "one", "two", "one", "one", "one", "two",
                  "one", "two", "three", "one", "two", "two", "one", "one", "two"),
                ncol = 2)

  m2n <- matrix(c(1, 2, 4, 1, 2, 1, 1, 1, 2,
                  1, 2, 3, 1, 2, 1, 1, 2, 2,
                  1, 2, 3, 1, 2, 2, 1, 1, 2),
                ncol = 3)

  m2s <- matrix(c("one", "two", "four", "one", "two", "one", "one", "one", "two",
                  "one", "two", "three", "one", "two", "one", "one", "two", "two",
                  "one", "two", "three", "one", "two", "two", "one", "one", "two"),
                ncol = 3)
  expect_equal(icr_agreement(m1n), 0.778, tolerance = .0005)
  expect_equal(icr_agreement(m1s), 0.778, tolerance = .0005)
  expect_equal(icr_agreement(m2n), 2/3, tolerance = .0005)
  expect_equal(icr_agreement(m2s), 2/3, tolerance = .0005)
})

test_that("Holsti computes correct result", {
  m1n <- matrix(c(1, 2, 4, 1, 2, 1, 1, 1, 2,
                  1, 2, 3, 1, 2, 2, 1, 1, 2),
                ncol = 2)

  m1s <- matrix(c("one", "two", "four", "one", "two", "one", "one", "one", "two",
                  "one", "two", "three", "one", "two", "two", "one", "one", "two"),
                ncol = 2)

  m2n <- matrix(c(1, 2, 4, 1, 2, 1, 1, 1, 2,
                  1, 2, 3, 1, 2, 1, 1, 2, 2,
                  1, 2, 3, 1, 2, 2, 1, 1, 2),
                ncol = 3)

  m2s <- matrix(c("one", "two", "four", "one", "two", "one", "one", "one", "two",
                  "one", "two", "three", "one", "two", "one", "one", "two", "two",
                  "one", "two", "three", "one", "two", "two", "one", "one", "two"),
                ncol = 3)

  expect_equal(icr_holstis_CR(m1n), 0.778, tolerance = .0005)
  expect_equal(icr_holstis_CR(m1s), 0.778, tolerance = .0005)
  expect_equal(icr_holstis_CR(m2n), 0.778, tolerance = .0005)
  expect_equal(icr_holstis_CR(m2s), 0.778, tolerance = .0005)
})
