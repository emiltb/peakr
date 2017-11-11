
#' Returns the index of the nearest peak
#'
#' Calculates the gradient at the given index and then travels along the line in the upwards direction until a peak is found
#'
#' @param x Vector of x-values
#' @param y Vector of y-values
#' @param ind Index to start peak search
#'
#' @return integer index of peak
#'

find_peak <- function(x, y, ind) {
  grad <- find_gradient(x, y, ind)
  if (is.na(grad)) return(NA)
  step <- if (grad > 0) 1 else -1

  repeat{
    ind <- ind + step
    if(y[ind + step] < y[ind]){ break }
  }

  return(ind)
}


#' Local gradient at given index
#'
#' @param x Vector of x-values
#' @param y Vector of y-values
#' @param ind Index to calculate the gradient at
#'
#' @return numeric
#'

find_gradient <- function(x, y, ind) {
  if (ind == 1 | ind == length(x)) {
    warning("Index out of bounds")
    return(NA)
  }

  (y[ind + 1] - y[ind - 1]) / (x[ind + 1] - x[ind - 1])
}

#' Return a standard dataframe with two columns (x and y)
#'
#' @param df Dataframe to be converted
#' @param x Column containing x-coordinates
#' @param y Column containing y-coordinates
#'
#' @return A standard tibble with the given data in two columns (x and y)
#'
#' @examples
#' library(tidyverse)
#' df <- tibble(x1 = seq(0, 5, 0.1), y1 = sin(x))
#' generic_df(df, x1, y1)
#'

generic_df <- function(df, x, y) {
  x = rlang::enquo(x)
  y = rlang::enquo(y)

  tibble::tibble(x = dplyr::pull(df, !!x), y = dplyr::pull(df, !!y))
}
