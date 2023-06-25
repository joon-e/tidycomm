test_that("designs", {
  expected_return <- c("main_color_1",
                       "main_colors",
                       "main_contrast_1",
                       "main_contrasts",
                       "main_size",
                       "comparison_linetype",
                       "comparison_color",
                       "comparison_size",
                       "theme")

  expect_equal(names(design_lmu()),
               expected_return)

  expect_equal(names(design_gray()),
               expected_return)

  expect_equal(names(design_grey()),
               expected_return)
})
