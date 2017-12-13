#' \code{peakr} package
#'
#' Interactive tools for analysis of small datasets
#'
#' See the README on
#' \href{https://cran.r-project.org/package=peakr/README.html}{CRAN}
#' or \href{https://github.com/emiltb/peakr#readme}{GitHub}
#'
#' @docType package
#' @name peakr
#' @importFrom dplyr %>%
#' @importFrom rlang !! :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
