context("Visualizations")

test_that("implemented visualize() calls return ggplot2 (gg)", {
  expect_s3_class(visualize(describe(WoJ, autonomy_selection)),
                  "gg")

  expect_s3_class(visualize(describe_cat(WoJ, reach, employment)),
                  "gg")
  expect_s3_class(visualize(describe_cat(WoJ, reach, employment),
                            stacked = FALSE),
                  "gg")

  expect_s3_class(visualize(tab_frequencies(WoJ, employment)),
                  "gg")
  expect_s3_class(visualize(tab_frequencies(WoJ, employment, reach)),
                  "gg")

  expect_s3_class(visualize(crosstab(WoJ, reach, employment)),
                  "gg")
  expect_s3_class(visualize(crosstab(WoJ, reach, employment, percentages = T)),
                  "gg")
  expect_error(visualize(crosstab(WoJ, reach, employment, ethics_1)))

  expect_s3_class(visualize(t_test(WoJ, temp_contract, autonomy_selection)),
                  "gg")
  expect_s3_class(visualize(t_test(WoJ, temp_contract,
                                   autonomy_selection, autonomy_emphasis)),
                  "gg")

  expect_s3_class(visualize(unianova(WoJ, employment)),
                  "gg")
  expect_s3_class(visualize(unianova(WoJ, employment, descriptives = T)),
                  "gg")
  expect_s3_class(visualize(unianova(WoJ, employment, post_hoc = T)),
                  "gg")

  expect_s3_class(visualize(correlate(WoJ, ethics_1, ethics_2)),
                  "gg")
  expect_s3_class(visualize(correlate(WoJ, ethics_1, ethics_2, ethics_3)),
                  "gg")
  expect_s3_class(visualize(correlate(WoJ,
                                      autonomy_selection,
                                      autonomy_emphasis,
                                      work_experience,
                                      partial = TRUE)),
                  "gg")
  expect_s3_class(visualize(to_correlation_matrix(correlate(WoJ,
                                                            ethics_1,
                                                            ethics_2,
                                                            ethics_3))),
                  "gg")

  r <- WoJ %>% regress(autonomy_selection, temp_contract, work_experience, ethics_2)
  expect_s3_class(visualize(r), "gg")
  expect_s3_class(visualize(r, "lm"), "gg")
  expect_warning(v <- visualize(r, "scatter"))
  expect_s3_class(v, "gg")
  expect_s3_class(visualize(r, "resfit"), "gg")
  expect_s3_class(visualize(r, "pp"), "gg")
  expect_s3_class(visualize(r, "qq"), "gg")
  expect_s3_class(visualize(r, "scaloc"), "gg")
  expect_s3_class(visualize(r, "reslev"), "gg")
})
