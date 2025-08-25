test_that("add_index adds variable", {

  WoJ_i <- WoJ %>%
    add_index(trust_politics,
              trust_parliament,
              trust_government,
              trust_parties,
              trust_politicians)

  WoJ_is <- WoJ %>%
    add_index(trust_politics,
              trust_parliament,
              trust_government,
              trust_parties,
              trust_politicians,
              type = "sum")

    expect_equal(ncol(WoJ) + 1, ncol(WoJ_i))
    expect_equal(ncol(WoJ) + 1, ncol(WoJ_is))
})

test_that("add_index adds attribute 'index_of'", {

  WoJ_i <- WoJ %>%
    add_index(trust_politics,
              trust_parliament,
              trust_government,
              trust_parties,
              trust_politicians)

  expect_true(!is.null(attr(WoJ_i$trust_politics, "index_of")))
})

test_that("add_index works with tidyselect", {

  WoJ_i <- WoJ %>%
    add_index(trust_politics,
              tidyselect::starts_with('trust'))

  expect_equal(ncol(WoJ) + 1, ncol(WoJ_i))
})

test_that("add_index works with factors", {

  expect_error(add_index(WoJ, new_index, country, ethics_1),
               "All variables for index computation must be numeric")
  expect_equal(ncol(add_index(WoJ, new_index, country, ethics_1, cast.numeric = TRUE)),
               ncol(WoJ) + 1)


})
