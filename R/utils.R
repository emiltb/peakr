
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

generic_df <- function(df, x, y) {
  x = rlang::enquo(x)
  y = rlang::enquo(y)

  tibble::tibble(x = dplyr::pull(df, !!x), y = dplyr::pull(df, !!y))
}


#' Add row to dataframe, or remove it if it already exists
#'
#' @param original The input dataframe
#' @param append A dataframe containing the same columns as `original` and 1 row
#'
#' @return If the row in `append` exists in `original` it will be removed, otherwise it will be added to the dataframe that is returned.
#'
add_if_unique <- function(original, append) {
  joined <- rbind(original, append)

  if (nrow(joined) == nrow(dplyr::distinct(joined))) {
    ret <- rbind(original, append) %>% dplyr::arrange(x)
  } else {
    ret <- dplyr::anti_join(original, append, by = c("x", "y"))
  }
  ret
}
