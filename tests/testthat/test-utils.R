library(peakr)

context("utils work properly")

test_that("add_if_unique adds/removes row", {
  df <- data.frame(x = c(1,2), y = c(3,4))
  row_in_df <- data.frame(x = 1, y = 3)
  row_not_in_df <- data.frame(x = 3, y = 5)
  expect_equal(add_if_unique(df, row_not_in_df, x, y), rbind(df, row_not_in_df))
  expect_equal(add_if_unique(df, row_in_df, x, y), data.frame(x = 2, y = 4))
})
