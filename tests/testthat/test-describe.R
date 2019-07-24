test_that("describe returns tibble", {
  expect_true(tibble::is_tibble(describe(mtcars)))
})

test_that("describe_groups returns tibble", {
  expect_true(tibble::is_tibble(describe_groups(mtcars, mpg, cyl, am)))
})
