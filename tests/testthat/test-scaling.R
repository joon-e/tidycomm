context("Scaling")

test_that("scaling return values work", {

  # reverse_scale
  expect_warning(reverse_scale(WoJ, autonomy_emphasis))
  expect_true(tibble::is_tibble(suppressWarnings(reverse_scale(WoJ,
                                                               autonomy_emphasis))))
  WoJ_rev <- WoJ %>% reverse_scale(autonomy_emphasis,
                                   lower_end = 1,
                                   upper_end = 5)
  expect_true(tibble::is_tibble(WoJ_rev))
  expect_equal(dim(WoJ_rev),
               dim(WoJ) + c(0, 1))
  expect_true("autonomy_emphasis_rev" %in% names(WoJ_rev))
  expect_true("ae_rev" %in%
                names(reverse_scale(WoJ,
                                    autonomy_emphasis,
                                    name = "ae_rev",
                                    lower_end = 1,
                                    upper_end = 5)))


  # z_scale
  WoJ_z <- WoJ %>% z_scale(autonomy_emphasis)
  expect_true(tibble::is_tibble(WoJ_z))
  expect_equal(dim(WoJ_z),
               dim(WoJ) + c(0, 1))
  expect_true("autonomy_emphasis_z" %in% names(WoJ_z))
  expect_true("ae_z" %in%
                names(z_scale(WoJ, autonomy_emphasis, name = "ae_z")))

  # center_scale
  WoJ_centered <- WoJ %>% center_scale(autonomy_emphasis)
  expect_true(tibble::is_tibble(WoJ_centered))
  expect_equal(dim(WoJ_centered),
               dim(WoJ) + c(0, 1))
  expect_true("autonomy_emphasis_centered" %in% names(WoJ_centered))
  expect_true("ae_center" %in%
                names(center_scale(WoJ, autonomy_emphasis, name = "ae_center")))

  # minmax_scale
  WoJ_changed <- WoJ %>% minmax_scale(autonomy_emphasis)
  expect_true(tibble::is_tibble(WoJ_changed))
  expect_equal(dim(WoJ_changed),
               dim(WoJ) + c(0, 1))
  expect_true("autonomy_emphasis_0to1" %in% names(WoJ_changed))
  expect_true("ae_change" %in%
                names(minmax_scale(WoJ, autonomy_emphasis, name = "ae_change")))
  expect_true("autonomy_emphasis_neg2to2" %in%
                names(minmax_scale(WoJ,
                                   autonomy_emphasis,
                                   change_to_min = -2,
                                   change_to_max = +2)))
})

test_that("scaling can handle false inputs", {

  #reverse_scale
  check <- tibble::tibble(a = c(1, 2, 3, NA),
                          b = forcats::as_factor(c(1, 1, 1, 2)),
                          c = c("a", "b", "cde", NA_character_))
  expect_equal(sum(is.na(check$a)),
               sum(is.na(reverse_scale(check, a,
                                       lower_end = 1,
                                       upper_end = 3)$a_rev)))
  expect_error(reverse_scale(check, b))
  expect_error(reverse_scale(check, c))

  #z_scale
  check <- tibble::tibble(a = c(1, 2, 3, NA),
                          b = forcats::as_factor(c(1, 1, 1, 2)),
                          c = c("a", "b", "cde", NA_character_))
  expect_equal(sum(is.na(check$a)),
               sum(is.na(z_scale(check, a)$a_z)))
  expect_error(z_scale(check, b))
  expect_error(z_scale(check, c))

  #center_scale
  check <- tibble::tibble(a = c(1, 2, 3, NA),
                          b = forcats::as_factor(c(1, 1, 1, 2)),
                          c = c("a", "b", "cde", NA_character_))
  expect_equal(sum(is.na(check$a)),
               sum(is.na(center_scale(check, a)$a_centered)))
  expect_error(center_scale(check, b))
  expect_error(center_scale(check, c))

  #minmax_scale
  check <- tibble::tibble(a = c(1, 2, 3, NA),
                          b = forcats::as_factor(c(1, 1, 1, 2)),
                          c = c("a", "b", "cde", NA_character_))
  expect_equal(sum(is.na(check$a)),
               sum(is.na(minmax_scale(check, a)$a_0to1)))
  expect_error(minmax_scale(check, b))
  expect_error(minmax_scale(check, c))
})

test_that("scaling returns correct scales", {

  #reverse_scale
  expect_equal(WoJ %>%
                 dplyr::filter(!is.na(autonomy_emphasis)) %>%
                 tab_frequencies(autonomy_emphasis) %>%
                 dplyr::pull(n),
               suppressWarnings(
                 WoJ %>%
                   dplyr::filter(!is.na(autonomy_emphasis)) %>%
                   reverse_scale(autonomy_emphasis) %>%
                   tab_frequencies(autonomy_emphasis_rev) %>%
                   dplyr::pull(n) %>%
                   rev()
               ))
  t <- tibble(s = c(-2, 2, 1, 0, 0, -2, NA, -1))
  expect_equal(t$s,
               t %>%
                 reverse_scale(s, lower_end = -2, upper_end = 2) %>%
                 reverse_scale(s_rev, lower_end = 2, upper_end = -2) %>%
                 dplyr::pull(s_rev_rev))

  #z_scale
  check <- WoJ %>% z_scale(autonomy_emphasis)
  expect_equal(check %>%
                 correlate(autonomy_emphasis, autonomy_emphasis_z) %>%
                 dplyr::pull(r),
               c(cor = 1))
  expect_equal(check %>%
                 dplyr::pull(autonomy_emphasis_z) %>%
                 mean(na.rm = TRUE) %>%
                 round(),
               0)
  expect_equal(check %>%
                 dplyr::pull(autonomy_emphasis_z) %>%
                 sd(na.rm = TRUE) %>%
                 round(),
               1)

  #center_scale
  check <- WoJ %>% center_scale(autonomy_emphasis)
  expect_equal(check %>%
                 correlate(autonomy_emphasis, autonomy_emphasis_centered) %>%
                 dplyr::pull(r),
               c(cor = 1))
  expect_equal(check %>%
                 dplyr::pull(autonomy_emphasis_centered) %>%
                 mean(na.rm = TRUE) %>%
                 round(),
               0)

  #minmax_scale
  check <- WoJ %>%
    z_scale(autonomy_emphasis) %>%
    minmax_scale(autonomy_emphasis, change_to_min = 1, change_to_max = 10) %>%
    z_scale(autonomy_emphasis_1to10)
  expect_equal(min(check$autonomy_emphasis_1to10, na.rm = TRUE), 1)
  expect_equal(max(check$autonomy_emphasis_1to10, na.rm = TRUE), 10)
  expect_equal(check$autonomy_emphasis_z,
               check$autonomy_emphasis_1to10_z)
})
