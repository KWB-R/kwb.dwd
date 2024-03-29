% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_monthly_data_for_berlin.R
\name{load_precipitation_berlin}
\alias{load_precipitation_berlin}
\title{Load monthly precipitation for Berlin from DWD}
\usage{
load_precipitation_berlin(from = NULL, to = NULL)
}
\arguments{
\item{from}{optional. First month to be considered, as yyyymm string}

\item{to}{optional. Last month to be considered, as yyyymm string}
}
\value{
data frame with columns \code{file} (name of file downloaded from
  DWD), \code{year} (year number as integer), \code{month number as integer},
  \code{mean} (mean value), \code{sd} (standard deviation), \code{min}
  (minimum value), \code{max} (maximum value) of precipitation
  calculated for Berlin, Germany
}
\description{
This function loads monthly precipitation for Berlin, Germany,
from Deutscher Wetterdienst (DWD).
}
