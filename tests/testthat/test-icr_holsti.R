context("ICR: Holsti")

test_that("Holsti computes correct result", {
  m1 <- matrix(c(1, 2, 4, 1, 2, 1, 1, 1, 2,
                 1, 2, 3, 1, 2, 1, 1, 2, 2,
                 1, 2, 3, 1, 2, 2, 1, 1, 2),
               ncol = 3)

  m2 <- matrix(c("one", "two", "four", "one", "two", "one", "one", "one", "two",
                 "one", "two", "three", "one", "two", "one", "one", "two", "two",
                 "one", "two", "three", "one", "two", "two", "one", "one", "two"),
               ncol = 3)

  expect_equal(icr_holsti(m1), 0.778, tolerance = .0005)
  expect_equal(icr_holsti(m2), 0.778, tolerance = .0005)
})
