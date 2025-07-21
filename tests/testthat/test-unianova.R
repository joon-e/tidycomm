# Main functions

test_that("unianova returns tibble", {

  t1 <- unianova(WoJ, employment, autonomy_selection, autonomy_emphasis)
  t2 <- unianova(WoJ, employment, autonomy_selection, autonomy_emphasis,
                 descriptives = TRUE)
  t3 <- unianova(WoJ, employment, autonomy_selection, autonomy_emphasis,
                 post_hoc = TRUE)

  expect_true(tibble::is_tibble(t1))
  expect_true(tibble::is_tibble(t2))
  expect_true(tibble::is_tibble(t3))

  expect_equal(dim(t1), c(2, 9))
  expect_equal(dim(t2), c(2, 15))
  expect_equal(dim(t3), c(2, 10))
})

test_that("t_test returns tibble when no variables are specified", {

  t1 <- unianova(WoJ, employment)
  t2 <- unianova(WoJ, employment, descriptives = TRUE)
  t3 <- unianova(WoJ, employment, post_hoc = TRUE)

  expect_true(tibble::is_tibble(t1))
  expect_true(tibble::is_tibble(t2))
  expect_true(tibble::is_tibble(t3))

  expect_equal(dim(t1), c(11, 9))
  expect_equal(dim(t2), c(11, 15))
  expect_equal(dim(t3), c(11, 10))
})


test_that("unianova works with spaces in column names", {
  t1 <- WoJ %>%
    dplyr::select(employment, `autonomy selection` = autonomy_selection) %>%
    unianova(employment)

  expect_equal(dim(t1), c(1, 8))
})
