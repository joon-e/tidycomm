context("Unianova")

# Main functions

test_that("compute_aov returns tibble", {

  t1 <- compute_aov(autonomy_selection, WoJ, employment,
                   descriptives = FALSE, post_hoc = FALSE)
  t2 <- compute_aov(autonomy_selection, WoJ, employment,
                    descriptives = TRUE, post_hoc = FALSE)
  t3 <- compute_aov(autonomy_selection, WoJ, employment,
                    descriptives = TRUE, post_hoc = TRUE)

  expect_true(tibble::is_tibble(t1))
  expect_true(tibble::is_tibble(t2))
  expect_true(tibble::is_tibble(t3))

  expect_equal(dim(t1), c(1, 6))
  expect_equal(dim(t2), c(1, 12))
  expect_equal(dim(t3), c(1, 13))
})

test_that("unianova returns tibble", {

  t1 <- unianova(WoJ, employment, autonomy_selection, autonomy_emphasis)
  t2 <- unianova(WoJ, employment, autonomy_selection, autonomy_emphasis,
                 descriptives = TRUE)
  t3 <- unianova(WoJ, employment, autonomy_selection, autonomy_emphasis,
                 post_hoc = TRUE)

  expect_true(tibble::is_tibble(t1))
  expect_true(tibble::is_tibble(t2))
  expect_true(tibble::is_tibble(t3))

  expect_equal(dim(t1), c(2, 6))
  expect_equal(dim(t2), c(2, 12))
  expect_equal(dim(t3), c(2, 7))
})

test_that("t_test returns tibble when no variables are specified", {

  t1 <- unianova(WoJ, employment)
  t2 <- unianova(WoJ, employment, descriptives = TRUE)
  t3 <- unianova(WoJ, employment, post_hoc = TRUE)

  expect_true(tibble::is_tibble(t1))
  expect_true(tibble::is_tibble(t2))
  expect_true(tibble::is_tibble(t3))

  expect_equal(dim(t1), c(11, 6))
  expect_equal(dim(t2), c(11, 12))
  expect_equal(dim(t3), c(11, 7))
})
