test_that("describe returns tibble", {
  expect_true(tibble::is_tibble(describe(mtcars)))
})
