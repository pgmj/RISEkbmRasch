% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RISEkbmRasch.R
\name{RIestThetasOLD2}
\alias{RIestThetasOLD2}
\title{Person location estimation with parallel processing}
\usage{
RIestThetasOLD2(
  dfin,
  itemParams,
  model = "PCM",
  method = "WL",
  cpu = 4,
  theta_range = c(-7, 7)
)
}
\arguments{
\item{dfin}{Dataframe with response data only (no demographics etc), items as columns}

\item{itemParams}{Optional item (threshold) location matrix}

\item{model}{Rasch model to use (use \code{NULL} for dichotomous data)}

\item{method}{Estimation method (defaults to \code{"WL"})}

\item{cpu}{Number of CPUs/cores to utilize (default is 4)}

\item{theta_range}{Range of theta (person location) values}
}
\description{
Yields about 2-3x speed increase when using 4-8 CPU cores.
Requires \code{library(furrr)}
}
\details{
NOTE: Does not yet work with dichotomous data

Outputs a vector of person locations, one for each row in the dataframe.

Uses thetaEst function from catR package to estimate person locations
(thetas) for a dataframe with item data as columns and persons as rows.
Defaults to use WL estimation (lower bias than ML, see Warm, 1989) and PCM.
See ?thetaEst for options available.
}
