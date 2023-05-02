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
})

test_that("correct subclasses are assigned to outputs", {

  # Crosstabs with Chi2
  expect_s3_class(crosstab(WoJ, reach, employment, chi_square = TRUE),
                  "tdcmm_chi2")
})

test_that("tdcmm provides model accessors", {

  # Outputs without models
  expect_warning(model(crosstab(WoJ, reach, employment)))

  # Crosstabs with Chi2
  expect_s3_class(model(crosstab(WoJ, reach, employment, chi_square = TRUE)),
                  "htest")
})
