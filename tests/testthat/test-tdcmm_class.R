test_that("S3 class tdcmm is assigned to relevant outputs", {

  # Indices
  expect_s3_class(add_index(WoJ,
                            ethical_flexibility,
                            ethics_1, ethics_2, ethics_3, ethics_4),
                  "tdcmm")
  expect_s3_class(add_index(WoJ,
                            ethical_flexibility,
                            ethics_1, ethics_2, ethics_3, ethics_4,
                            type = "sum"),
                  "tdcmm")

  expect_s3_class(get_reliability(add_index(WoJ,
                                            ethical_flexibility,
                                            ethics_1, ethics_2, ethics_3)),
                  "tdcmm")
  expect_s3_class(get_reliability(add_index(WoJ,
                                            ethical_flexibility,
                                            ethics_1, ethics_2, ethics_3,
                                            type = "sum")),
                  "tdcmm")

  # Crosstabs
  expect_s3_class(crosstab(WoJ, reach, employment), "tdcmm")
  expect_s3_class(crosstab(WoJ, reach, employment, chi_square = TRUE), "tdcmm")

  # Frequency tables
  expect_s3_class(tab_frequencies(WoJ, employment, country), "tdcmm")

  # Correlations
  expect_s3_class(correlate(WoJ, work_experience, autonomy_selection), "tdcmm")
  expect_s3_class(correlate(WoJ, work_experience, autonomy_selection,
                            method = "kendall"), "tdcmm")
  expect_s3_class(to_correlation_matrix(correlate(WoJ,
                                                  work_experience,
                                                  autonomy_selection)),
                  "tdcmm")
  expect_s3_class(to_correlation_matrix(correlate(WoJ,
                                                  work_experience,
                                                  autonomy_selection,
                                                  method = "kendall")),
                  "tdcmm")

  # Description
  expect_s3_class(describe(WoJ, autonomy_emphasis), "tdcmm")
  expect_s3_class(describe_cat(WoJ, reach), "tdcmm")

  # ICR
  expect_s3_class(test_icr(fbposts,
                           post_id, coder_id,
                           pop_elite, pop_othering),
                  "tdcmm")
  expect_s3_class(test_icr(fbposts,
                           post_id, coder_id,
                           levels = c(n_pictures = "ordinal"),
                           fleiss_kappa = TRUE),
                  "tdcmm")

  # t Test
  expect_s3_class(t_test(WoJ, temp_contract, autonomy_selection),
                  "tdcmm")
  expect_s3_class(t_test(WoJ,
                         temp_contract, autonomy_selection, autonomy_emphasis),
                  "tdcmm")

  # uni anova
  expect_s3_class(unianova(WoJ, employment,
                           autonomy_selection, autonomy_emphasis),
                  "tdcmm")
  expect_s3_class(unianova(WoJ, employment),
                  "tdcmm")
  expect_s3_class(unianova(WoJ, employment,
                           descriptives = TRUE, post_hoc = TRUE),
                  "tdcmm")

  # regression
  expect_s3_class(regress(WoJ, autonomy_selection, ethics_1), "tdcmm")
  expect_s3_class(regress(WoJ, autonomy_selection,
                          work_experience, trust_government), "tdcmm")
  expect_s3_class(regress(WoJ, autonomy_selection,
                          work_experience, trust_government,
                          check_multicollinearity = TRUE), "tdcmm")
})

test_that("correct subclasses are assigned to outputs", {

  # Crosstabs with Chi2
  expect_s3_class(crosstab(WoJ, reach, employment, chi_square = TRUE),
                  "tdcmm_chi2")

  # Correlation matrices with correlations
  expect_s3_class(to_correlation_matrix(correlate(WoJ,
                                                  work_experience,
                                                  autonomy_selection)),
                  "tdcmm_cormatrix")

  # t tests
  expect_s3_class(t_test(WoJ, temp_contract, autonomy_selection),
                  "tdcmm_ttest")

  # uni anova
  expect_s3_class(unianova(WoJ, employment,
                           autonomy_selection, autonomy_emphasis),
                  "tdcmm_uniaov")

  # regression
  expect_s3_class(regress(WoJ, autonomy_selection, ethics_1), "tdcmm_lm")
})

test_that("tdcmm provides model accessors", {

  # Outputs without models
  expect_warning(model(crosstab(WoJ, reach, employment)))

  # Crosstabs with Chi2
  expect_s3_class(model(crosstab(WoJ, reach, employment, chi_square = TRUE)),
                  "htest")

  # Correlation matrices with correlations
  expect_s3_class(model(to_correlation_matrix(correlate(WoJ,
                                                        work_experience,
                                                        autonomy_selection))),
                  "tdcmm")

  # t tests with t.test
  expect_s3_class(model(t_test(WoJ, temp_contract, autonomy_selection)),
                  "htest")
  expect_length(model(t_test(WoJ, temp_contract,
                             autonomy_selection, autonomy_emphasis)),
                2)
  expect_s3_class(model(t_test(WoJ, temp_contract,
                               autonomy_selection, autonomy_emphasis))[[1]],
                  "htest")
  expect_s3_class(model(t_test(WoJ, temp_contract,
                               autonomy_selection, autonomy_emphasis))[[2]],
                  "htest")

  # uni anova with aov/lm
  expect_s3_class(model(unianova(WoJ, employment, autonomy_selection)),
                  "aov")
  expect_s3_class(model(unianova(WoJ, employment, autonomy_selection,
                                 descriptives = TRUE, post_hoc = TRUE)),
                  "aov")
  expect_length(model(unianova(WoJ, employment,
                               autonomy_selection, autonomy_emphasis)),
                2)
  expect_s3_class(model(unianova(WoJ, employment,
                                 autonomy_selection, autonomy_emphasis))[[1]],
                  "aov")
  expect_s3_class(model(unianova(WoJ, employment,
                                 autonomy_selection, autonomy_emphasis))[[2]],
                  "aov")

  # regression with lm
  expect_s3_class(model(regress(WoJ, autonomy_selection, ethics_1)),
                  "lm")
})
