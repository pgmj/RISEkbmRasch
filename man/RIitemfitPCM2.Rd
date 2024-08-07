% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RISEkbmRasch.R
\name{RIitemfitPCM2}
\alias{RIitemfitPCM2}
\title{Create table with Rasch PCM model item fit values for each item.}
\usage{
RIitemfitPCM2(
  dfin,
  samplesize = 200,
  nsamples = 8,
  cpu = 4,
  zstd_min = -1.96,
  zstd_max = 1.96,
  msq_min = 0.7,
  msq_max = 1.3,
  fontsize = 15,
  fontfamily = "Lato",
  output = "table",
  tbl_width = 65,
  method = "conditional"
)
}
\arguments{
\item{dfin}{Dataframe with item data only}

\item{samplesize}{Desired sample size in multisampling (recommended range 200-500)}

\item{nsamples}{Desired number of samples (recommended range 8-50)}

\item{cpu}{Number of CPU cores to utilize (default = 4)}

\item{zstd_min}{Lower cutoff level for ZSTD}

\item{zstd_max}{Upper cutoff level for ZSTD}

\item{msq_min}{Lower cutoff level for MSQ}

\item{msq_max}{Upper cutoff level for MSQ}

\item{fontsize}{Set fontsize for table}

\item{fontfamily}{Set font family for table}

\item{output}{Defaults to output a table. Optional "dataframe" or "quarto"}

\item{tbl_width}{Set table width in percent}

\item{method}{Defaults to "conditional". Optional "unconditional"}
}
\description{
Special version of \code{RIitemfitPCM()} that utilizes multiple CPU cores to improve
performance. Requires \code{library(doParallel)}. To find how many cores you
have on your computer, use \code{parallel::detectCores()}, but remember to keep
some cores free.
}
\details{
See documentation for \code{RIitemfitPCM()} for more complete information.
}
