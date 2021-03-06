% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcOuterPerformances.R
\name{calcOuterPerformance}
\alias{calcOuterPerformance}
\title{Calculates Outer Performance for a given Iteration}
\usage{
calcOuterPerformance(tuning.resampled, out.res.i, ...)
}
\arguments{
\item{tuning.resampled}{[\code{\link[mlr]{ResampleResult}} | \code{\link{ResampleOverfitResult}}]
Make sure to run \code{resample(..., extract = getTuneResult, keep.pred = TRUE)}.}

\item{out.res.i}{[\code{integer(1)}]}

\item{...}{Arguments passed to specific methods}
}
\value{
\code{data.table}
}
\description{
Calculates the performance on the outer test set for a specific nested resampling iteration with a tune wrapper
}
