
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
