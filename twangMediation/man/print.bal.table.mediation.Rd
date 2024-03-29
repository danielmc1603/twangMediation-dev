% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.bal.table.mediation.R
\name{print.bal.table.mediation}
\alias{print.bal.table.mediation}
\title{Default print statement for \code{mediation} class}
\usage{
\method{print}{bal.table.mediation}(x, ...)
}
\arguments{
\item{x}{A \code{bal.table.mediation} object.}

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

bal.table.mediation(fit.es.max)
}
\seealso{
\code{\link{bal.table.mediation}}, \code{\link{wgtmed}}
}
