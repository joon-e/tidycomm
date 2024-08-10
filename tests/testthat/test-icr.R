context("ICR: Main functions")

test_that("unit-coder matrices are generated", {

  ucm_string <- unit_coder_matrix(fbposts, post_id, coder_id, type)
  ucm_int <- unit_coder_matrix(fbposts, post_id, coder_id, pop_elite)

  expect_equal(dim(ucm_string), c(45, 6))
  expect_equal(dim(ucm_int), c(45, 6))
})

test_that("compute_icr returns tibble", {

  t_string <- compute_icr(type, fbposts, post_id, coder_id)
  t_int <- compute_icr(pop_elite, fbposts, post_id, coder_id)

  expect_true(tibble::is_tibble(t_string))
  expect_true(tibble::is_tibble(t_int))

  expect_equal(dim(t_string), c(1, 8))
  expect_equal(dim(t_int), c(1, 8))
})

test_that("test_icr works with specified test variables", {

  t1v <- test_icr(fbposts, post_id, coder_id, pop_elite,
                  fleiss_kappa = TRUE, brennan_prediger = TRUE)
  t3v <- test_icr(fbposts, post_id, coder_id, pop_elite, pop_people, pop_othering)

  expect_true(tibble::is_tibble(t1v))
  expect_true(tibble::is_tibble(t3v))

  expect_equal(dim(t1v), c(1, 10))
  expect_equal(dim(t3v), c(3, 8))
})

test_that("test_icr works tests all variables if no test variables are specified", {

  t <- test_icr(fbposts, post_id, coder_id)

  expect_true(tibble::is_tibble(t))

  expect_equal(dim(t), c(5, 8))
})

test_that("test_icr produces warning with missing data", {

  fbposts_NA <- fbposts
  fbposts_NA[4, 4] <- NA

  expect_warning(test_icr(fbposts_NA, post_id, coder_id))

})

test_that("test_icr removes missing data if specified", {

  fbposts_NA <- fbposts
  fbposts_NA[4, 4] <- NA

  t <- test_icr(fbposts_NA, post_id, coder_id, n_pictures, na.omit = TRUE)

  expect_equal(t$n_Units, 44)
})

test_that("test_icr works with tidyselect", {
  t1 <- fbposts %>%
    test_icr(post_id, coder_id, tidyselect::starts_with("pop"))

  t2 <- fbposts %>%
    test_icr(post_id, coder_id, -type)

  expect_equal(dim(t1), c(3, 8))
  expect_equal(dim(t2), c(4, 8))
})

test_that("test_icr prints a specific error message when using empty ucm", {
  data <- tibble(
    unit = c(1, 1, 2, 2, 3, 3),
    coder = c('a', 'b', 'a', 'c', 'b', 'c'),
    code = rnorm(6)
  )

  expect_error(data %>% test_icr(unit, coder, na.omit = T),
               "Empty units-coders matrix detected")

})

test_that("test_icr prints a specific error message when forgetting to specifiy unit_var and coder_var", {
  data <- tibble(
    unit = c(1, 1, 2, 2, 3, 3),
    coder = c('a', 'b', 'a', 'c', 'b', 'c'),
    code = rnorm(6)
  )

  expect_error(data %>% test_icr(code),
               "Please provide both a variable with unit identifiers and a variable with coder identifiers.")

})

test_that("test_icr works with grouped data", {
  data <- data.frame(
    "group" = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5),
    "R1"    = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0),
    "R2"    = c(1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    "R3"    = c(1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0))

  data <- data %>%
    dplyr::mutate(post_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(R1:R3, names_to = "coder_id", values_to = "code") %>%
    dplyr::group_by(group)

  t1 <- data %>%
    test_icr(post_id, coder_id, code)

  expect_equal(dim(t1), c(5, 9))
})

test_that("check_disagreements identifies disagreements correctly", {
  data <- tibble(
    unit = c(1, 1, 2, 2, 3, 3, 4, 4),
    coder = c('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b'),
    code = c('1', '1', '0', '1', '1', '0', '0', '0')
    )

  result <- test_icr(data, unit, coder, code, check_disagreements = TRUE)
  expect_true("code_a" %in% names(result),
              info = "Output should include a 'code_a' column when check_disagreements is TRUE.")
  expect_equal(nrow(result), 2,
               info = "Result should have a row for each coder disagreement (here: 2) when check_disagreements is TRUE.")
})

test_that("check_pairs computes pair-wise reliabilities correctly with only 2 coders", {

  data <- tibble(
    unit = c(1, 1, 2, 2, 3, 3, 4, 4),
    coder = c('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b'),
    code = c('1', '1', '0', '1', '1', '0', '0', '0')
  )

  result <- test_icr(data, unit, coder, code, check_pairs = TRUE)

  expect_named(result, c("Variable", "n_Units", "n_Coders", "Coder_Pair", "n_Categories", "Level", "Agreement", "Holstis_CR", "Krippendorffs_Alpha"),
               info = "Output should include expected columns.")
  expect_equal(nrow(result), 1,
               info = "Output should have one row for each unique pair of coders. Here: 1")
})

test_that("check_pairs computes pair-wise reliabilities correctly with only 3 coders", {

  data <- tibble(
    unit = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
    coder = c('a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c'),
    code = c('1', '1', '1', '0', '1', '1', '1', '0', '0', '0', '0', '0')
  )

  result <- test_icr(data, unit, coder, code, check_pairs = TRUE)

  expect_named(result, c("Variable", "n_Units", "n_Coders", "Coder_Pair", "n_Categories", "Level", "Agreement", "Holstis_CR", "Krippendorffs_Alpha"),
               info = "Output should include expected columns.")
  expect_equal(nrow(result), 3,
               info = "Output should have one row for each unique pair of coders. Here: 3")
})

test_that("test_icr prints a specific error message when check_pairs and check_disagreements are both set to TRUE", {
  expect_error(
    test_icr(data, unit, coder, code, check_disagreements = TRUE, check_pairs = TRUE),
    "The parameters 'check_pairs' and 'check_disagreements' are mutually exclusive",
    fixed = TRUE
  )
})
