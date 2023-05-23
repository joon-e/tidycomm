context("Visualizations")

test_that("implemented visualize() calls return ggplot2", {
  expect_s3_class(visualize(describe(WoJ, autonomy_selection)),
                  "ggplot")
})
