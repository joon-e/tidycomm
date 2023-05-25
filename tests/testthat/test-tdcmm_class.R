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

  # Describe
  expect_s3_class(describe(WoJ, autonomy_emphasis),
                  "tdcmm_dscrb")
  expect_s3_class(describe_cat(WoJ, reach),
                  "tdcmm_dscrb")

  # Categorical
  expect_s3_class(tab_frequencies(WoJ, employment, country),
                  "tdcmm_ctgrcl")
  expect_s3_class(crosstab(WoJ, reach, employment), "tdcmm_ctgrcl")
  expect_s3_class(crosstab(WoJ, reach, employment, chi_square = TRUE),
                  "tdcmm_ctgrcl")

  # Correlation
  expect_s3_class(correlate(WoJ, work_experience, autonomy_selection),
                  "tdcmm_crrltn")
  expect_s3_class(to_correlation_matrix(correlate(WoJ,
                                                  work_experience,
                                                  autonomy_selection)),
                  "tdcmm_crrltn")

  # t tests
  expect_s3_class(t_test(WoJ, temp_contract, autonomy_selection),
                  "tdcmm_ttst")

  # uni anova
  expect_s3_class(unianova(WoJ, employment,
                           autonomy_selection, autonomy_emphasis),
                  "tdcmm_nnv")

  # regression
  expect_s3_class(regress(WoJ, autonomy_selection, ethics_1), "tdcmm_rgrssn")
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

test_that("tdcmm contains adequate func names and param lists", {
  t <- describe(WoJ, autonomy_selection)
  expect_equal(attr(t, "func"), "describe")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(describe)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- describe_cat(WoJ, reach)
  expect_equal(attr(t, "func"), "describe_cat")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(describe_cat)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- tab_frequencies(WoJ, employment)
  expect_equal(attr(t, "func"), "tab_frequencies")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(tab_frequencies)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- crosstab(WoJ, reach, employment)
  expect_equal(attr(t, "func"), "crosstab")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(crosstab)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- crosstab(WoJ, reach, employment, chi_square = TRUE)
  expect_equal(attr(t, "func"), "crosstab")
  expect_type(attr(t, "params"), "list")

  t <- correlate(WoJ)
  expect_equal(attr(t, "func"), "correlate")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(correlate)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- to_correlation_matrix(t)
  expect_equal(attr(t, "func"), "to_correlation_matrix")
  expect_type(attr(t, "params"), "list")

  # to_correlation_matrix uses its corresponding correlate params
  expect_equal(length(formals(to_correlation_matrix)) - 1, # reduced by piping data argument
               0)
  expect_equal(length(attr(t, "params")),
               length(attr(model(t), "params")))

  t <- add_index(WoJ, ethical_flexibility, ethics_1, ethics_2, ethics_3)
  expect_equal(attr(t, "func"), "add_index")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(add_index)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- get_reliability(t)
  expect_equal(attr(t, "func"), "get_reliability")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(get_reliability)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- t_test(WoJ, temp_contract)
  expect_equal(attr(t, "func"), "t_test")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(t_test)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- unianova(WoJ, employment)
  expect_equal(attr(t, "func"), "unianova")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(unianova)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- test_icr(fbposts, post_id, coder_id, pop_elite, pop_othering)
  expect_equal(attr(t, "func"), "test_icr")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(test_icr)) - 1, # reduced by piping data argument
               length(attr(t, "params")))

  t <- regress(WoJ, autonomy_selection, ethics_1)
  expect_equal(attr(t, "func"), "regress")
  expect_type(attr(t, "params"), "list")
  expect_equal(length(formals(regress)) - 1, # reduced by piping data argument
               length(attr(t, "params")))
})
