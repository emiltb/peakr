
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


#' Add row to dataframe, or remove it if it already exists
#'
#' @param original The input dataframe
#' @param append A dataframe containing the same columns as `original` and 1 row
#'
#' @return If the row in `append` exists in `original` it will be removed, otherwise it will be added to the dataframe that is returned.
#'
add_if_unique <- function(original, append, x, y) {
  x <- dplyr::enquo(x)
  y <- dplyr::enquo(y)
  joined <- rbind(original, append)

  if (nrow(joined) == nrow(dplyr::distinct(joined))) {
    ret <- rbind(original, append) %>% dplyr::arrange(!! x)
  } else {
    ret <- suppressMessages(dplyr::anti_join(original, append))

  }
  ret
}
