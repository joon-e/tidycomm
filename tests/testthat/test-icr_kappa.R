# Cohen's Kappa

test_that("Cohen's Kappa computes correct result", {
  m1n <- matrix(c(1, 2, 4, 1, 2, 1, 1, 1, 2,
                  1, 2, 3, 1, 2, 2, 1, 1, 2),
                ncol = 2)

  m1s <- matrix(c("one", "two", "four", "one", "two", "one", "one", "one", "two",
                  "one", "two", "three", "one", "two", "two", "one", "one", "two"),
                ncol = 2)

  expect_equal(icr_cohens_kappa(m1n), 0.633, tolerance = .0006)
  expect_equal(icr_cohens_kappa(m1s), 0.633, tolerance = .0006)
})


# Fleiss' Kappa

test_that("Cohen's Kappa computes correct result", {
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

  expect_equal(icr_fleiss_kappa(m1n), 0.625, tolerance = .0005)
  expect_equal(icr_fleiss_kappa(m1s), 0.625, tolerance = .0005)
  expect_equal(icr_fleiss_kappa(m2n), 0.627, tolerance = .0005)
  expect_equal(icr_fleiss_kappa(m2s), 0.627, tolerance = .0005)
})


# Brennan & Prediger's Kappa

test_that("Brennan & Prediger's Kappa computes correct result", {
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

  expect_equal(icr_brennan_prediger(m1n), 0.704, tolerance = .0005)
  expect_equal(icr_brennan_prediger(m1s), 0.704, tolerance = .0005)
  expect_equal(icr_brennan_prediger(m2n), 0.644, tolerance = .0007)
  expect_equal(icr_brennan_prediger(m2s), 0.644, tolerance = .0007)
})
