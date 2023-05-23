context("Visualizations")

test_that("implemented visualize() calls return ggplot2", {
  expect_s3_class(visualize(describe(WoJ, autonomy_selection)),
                  "ggplot")

  expect_s3_class(visualize(describe_cat(WoJ, reach, employment)),
                  "ggplot")
  expect_s3_class(visualize(describe_cat(WoJ, reach, employment),
                            stacked = FALSE),
                  "ggplot")

  expect_s3_class(visualize(tab_frequencies(WoJ, employment)),
                  "ggplot")
  expect_s3_class(visualize(tab_frequencies(WoJ, employment, reach)),
                  "ggplot")
})
