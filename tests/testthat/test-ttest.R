context("t-Test")

# Main functions

test_that("compute_t_test returns tibble", {

  t <- compute_t_test(autonomy_selection, WoJ, temp_contract,
                      levels = c("Permanent", "Temporary"),
                      var.equal = TRUE, paired = FALSE, pooled_sd = TRUE)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(1, 10))
})

test_that("t_test returns tibble", {

  t <- t_test(WoJ, temp_contract, autonomy_selection, autonomy_emphasis)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(2, 10))
})

test_that("t_test returns tibble when no variables are specified", {

  t <- t_test(WoJ, temp_contract)

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(11, 10))
})

test_that("t_test returns tibble with user-defined levels", {

  t <- t_test(WoJ, employment, levels = c("Full-time", "Part-time"))

  expect_true(tibble::is_tibble(t))
  expect_equal(dim(t), c(11, 10))
})


# Helper functions

test_that("cohen's d returns correct results", {

  x <- WoJ %>%
    dplyr::filter(temp_contract == "Permanent") %>%
    dplyr::pull(autonomy_selection)
  y <- WoJ %>%
    dplyr::filter(temp_contract == "Temporary") %>%
    dplyr::pull(autonomy_selection)

  expect_equal(cohens_d(x, y), 0.277, tolerance = .0005)
  expect_equal(cohens_d(x, y, pooled_sd = FALSE), 0.250, tolerance = .0005)
})

