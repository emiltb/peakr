library(peakr)

context("utils work properly")

test_that("find_peak returns correct values", {
  df <- tibble::tibble(x = 1:5, y = c(1,2,3,2,1))
  expect_equal(find_peak(df$x, df$y, 2), 3)
  expect_warning(find_peak(df$x, df$y, 1), "Index out of bounds")
})


test_that("find_gradient works", {
  df <- tibble::tibble(x = 1:5, y = c(1,2,3,4,5))
  expect_gt(find_gradient(df$x, df$y, 3), 0)
  expect_lt(find_gradient(df$x, rev(df$y), 3), 0)
  expect_warning(find_gradient(df$x, df$y, 1), "Index out of bounds")
})


test_that("add_if_unique adds/removes row", {
  df <- data.frame(x = c(1,2), y = c(3,4))
  row_in_df <- data.frame(x = 1, y = 3)
  row_not_in_df <- data.frame(x = 3, y = 5)
  expect_equal(add_if_unique(df, row_not_in_df), rbind(df, row_not_in_df))
  expect_equal(add_if_unique(df, row_in_df), data.frame(x = 2, y = 4))
})
