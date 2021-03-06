% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pick.R
\name{plot_pick}
\alias{plot_pick}
\title{Plot a dataset contain defined peaks}
\usage{
plot_pick(data, x, y)
}
\arguments{
\item{data}{A tibble containing the data to pick peaks in}

\item{x}{Column containing the x-values}

\item{y}{Column containing the y-values}
}
\value{
A ggplot
}
\description{
Plot a dataset contain defined peaks
}
\examples{
library(tibble)
library(dplyr)

tibble(x1 = seq(0.1, 9, 0.01), y1 = sin(x1)) \%>\%
  add_pick(c(148,776)) \%>\%
  plot_pick(x1, y1)
}
