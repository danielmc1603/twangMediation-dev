% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.mediation.R
\name{print.mediation}
\alias{print.mediation}
\title{Default print statement for \code{mediation} class}
\usage{
\method{print}{mediation}(x, ...)
}
\arguments{
\item{x}{A \code{mediation} object.}

\item{...}{Additional arguments.}
}
\value{
Default print statement.
}
\description{
Default print statement for \code{mediation} class
}
\examples{
data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "ps",
                      ps_n.trees=1500,
                      ps_shrinkage=0.01,
                      ps_stop.method=c("es.max")
                      )

print(fit.es.max)
}
\seealso{
\code{\link{wgtmed}} for in put.
}
