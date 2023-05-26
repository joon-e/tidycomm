test_that("formatters work", {

  p_vals <- c(0.1, 0.12, 0.05, 0.04999, 0.05001, 0.001, 0.00051)
  p_strings <- c("p = 0.100", "p = 0.120", "p = 0.050", "p = 0.050",
                 "p = 0.050", "p = 0.001", "p < 0.001")

  expect_equal(format_pvalue(p_vals), p_strings)

  other_vals <- c(1.12345, 100.12345, 0.50001, 0.49999)

  expect_equal(format_value(other_vals, 2),
               c("1.12", "100.12", "0.50", "0.50"))

  expect_equal(format_value(other_vals, 3),
               c("1.123", "100.123", "0.500", "0.500"))
})
