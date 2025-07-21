test_that("compute_reliability returns tibble", {
  rel <- WoJ %>%
    compute_reliability(ethics_1, ethics_2, ethics_3, ethics_4)
  rel_ci <- WoJ %>%
    compute_reliability(ethics_1, ethics_2, ethics_3, ethics_4,
                        interval.type = "ml")

  expect_true(tibble::is_tibble(rel))
  expect_true(tibble::is_tibble(rel_ci))
})

test_that("get_reliability picks all available indices", {
  df <- WoJ %>%
    add_index(trust_politics,
              tidyselect::starts_with('trust')) %>%
    add_index(ethical_flexbility,
              tidyselect::starts_with('ethics'))

  rel <- get_reliability(df)

  expect_equal(nrow(rel), 2)
})

test_that("get_reliability produces error if no index variables are found", {

  expect_error(get_reliability(WoJ))

})

test_that("get_reliability outputs progess if specified", {
  df <- WoJ %>%
    add_index(trust_politics,
              tidyselect::starts_with('trust')) %>%
    add_index(ethical_flexbility,
              tidyselect::starts_with('ethics'))

  expect_message(get_reliability(df, progress = TRUE))
})
