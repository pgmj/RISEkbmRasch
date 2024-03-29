% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RISEkbmRasch.R
\name{RIdifThreshFigLR}
\alias{RIdifThreshFigLR}
\title{DIF PCM analysis with panel figure output for item thresholds}
\usage{
RIdifThreshFigLR(dfin, dif.var)
}
\arguments{
\item{dfin}{Dataframe with item data only}

\item{dif.var}{DIF variable}
}
\description{
Makes use of the eRm package function \code{LRtest()}. Outputs a panel of figures
with item threshold locations and 95\% confidence intervals.
}
\details{
DIF variables need to be factors with the same length as the number of rows
in the dataset.

sample usage: RIdifTableE(df, dif.age)
}
