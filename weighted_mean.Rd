% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_mediation.R
\name{weighted_mean}
\alias{weighted_mean}
\title{Calculate a weighted mean.}
\usage{
weighted_mean(x, weights, multiplier = NULL, na.rm = TRUE)
}
\arguments{
\item{x}{numeric
The the data set}

\item{weights}{numeric
The weights}

\item{multiplier}{An additional vector to multiply
Default : \code{NULL}}

\item{na.rm}{Whether to remove NA values.
Default: \code{TRUE}}
}
\value{
numeric
The weighted mean of the data.
}
\description{
\code{weighted_mean} calculates a weighted mean, given a vector.
}
