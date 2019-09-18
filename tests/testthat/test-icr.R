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
