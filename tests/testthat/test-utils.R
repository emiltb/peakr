library(peakr)

context("Peak finding tools")

test_that("find_peak returns correct values", {
  df <- tibble::tibble(x = 1:5, y = c(1,2,3,2,1))
  expect_equal(find_peak(df$x, df$y, 2), 3)
})
