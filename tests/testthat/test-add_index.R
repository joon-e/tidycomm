context("Add index")

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
