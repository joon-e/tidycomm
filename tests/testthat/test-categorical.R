# Main functions

test_that("tab_frequencies returns tibble", {

  t1 <- tab_frequencies(WoJ, employment)
  t2 <- tab_frequencies(WoJ, employment, country)

  expect_true(tibble::is_tibble(t1))
  expect_true(tibble::is_tibble(t2))

  expect_equal(dim(t1), c(3,5))
  expect_equal(dim(t2), c(15, 6))
})

test_that("crosstab returns tibble", {

  t1 <- crosstab(WoJ, reach, employment)
  t2 <- crosstab(WoJ, reach, employment, add_total = TRUE,
                 percentages = TRUE, chi_square = TRUE)

  expect_true(tibble::is_tibble(t1))
  expect_true(tibble::is_tibble(t2))

  expect_equal(dim(t1), c(3,5))
  expect_equal(dim(t2), c(3, 6))
})

# Helper functions

test_that("Cramers V returns correct result", {

  chi2 <- chisq.test(WoJ$reach, WoJ$employment)

  expect_equal(cramer_V(chi2), .082, tolerance = .005)

})
