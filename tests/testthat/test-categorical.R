test_that("tab_frequencies returns tibble", {
  expect_true(tibble::is_tibble(tab_frequencies(iris, Species)))
})

test_that("crosstab returns tibble", {
  expect_true(tibble::is_tibble(crosstab(mtcars, vs, am)))
})
