context("ICR: Kappas")

# Cohen's Kappa

test_that("Cohen's Kappa computes correct result", {
  m1 <- matrix(c(1, 2, 4, 1, 2, 1, 1, 1, 2,
                 1, 2, 3, 1, 2, 2, 1, 1, 2),
               ncol = 2)

  m2 <- matrix(c("one", "two", "four", "one", "two", "one", "one", "one", "two",
                 "one", "two", "three", "one", "two", "two", "one", "one", "two"),
               ncol = 2)

  expect_equal(icr_cohens_kappa(m1), 0.633, tolerance = .0005)
  expect_equal(icr_cohens_kappa(m2), 0.633, tolerance = .0005)
})
