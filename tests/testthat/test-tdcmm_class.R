test_that("S3 class tdcmm is assigned to relevant outputs", {

  # Crosstabs
  expect_s3_class(crosstab(WoJ, reach, employment), "tdcmm")
  expect_s3_class(crosstab(WoJ, reach, employment, chi_square = TRUE), "tdcmm")
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
