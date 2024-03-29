% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wgtmed.R
\name{wgtmed}
\alias{wgtmed}
\title{Weighted mediation analysis.}
\usage{
wgtmed(
  formula.med,
  data,
  a_treatment,
  y_outcome = NULL,
  total_effect_wts = NULL,
  total_effect_ps = NULL,
  total_effect_stop_rule = NULL,
  method = "ps",
  sampw = NULL,
  ps_n.trees = 10000,
  ps_interaction.depth = 3,
  ps_shrinkage = 0.01,
  ps_bag.fraction = 1,
  ps_n.minobsinnode = 10,
  ps_perm.test.iters = 0,
  ps_verbose = FALSE,
  ps_stop.method = c("ks.mean", "ks.max"),
  ps_version = "gbm",
  ps_ks.exact = NULL,
  ps_n.keep = 1,
  ps_n.grid = 25,
  ps_cv.folds = 10,
  ps_keep.data = FALSE
)
}
\arguments{
\item{formula.med}{A object of class \link{formula} relating the mediatior(s)
to the covariates (potential confounding variables).}

\item{data}{A dataset of class \link{data.frame} that includes the treatment indicator, mediator(s), and covariates.}

\item{a_treatment}{The (character) name of the treatment variable, which must be
dichotomous (0, 1).}

\item{y_outcome}{The (character) name of the outcome variable, y. If this is not provided, then
no effects will be calculated and a warning will be raised. Default : \code{NULL}.}

\item{total_effect_wts}{A vector of total effect weights, which if left \code{NULL}
then total_effect_ps must be supplied. Default : \code{NULL}.}

\item{total_effect_ps}{A ps object that contains the total effect weights,}

\item{total_effect_stop_rule}{The stopping rule (\code{ks.mean}, \code{ks.max}, \code{es.mean}, \code{es.max}) for the total effect weights, which
only needs to be specified if total_effect_ps is provided. Default : \code{NULL}.}

\item{method}{The method for getting weights ("ps", "logistic", or "crossval"). Default : \code{"ps"}.}

\item{sampw}{Optional sampling weights Default : \code{NULL}.}

\item{ps_n.trees}{Number of gbm iterations passed on to \link{gbm}. Default: 10000.}

\item{ps_interaction.depth}{A positive integer denoting the tree depth used in
gradient boosting. Default: 3.}

\item{ps_shrinkage}{A numeric value between 0 and 1 denoting the learning rate.
See \link{gbm} for more details. Default: 0.01.}

\item{ps_bag.fraction}{A numeric value between 0 and 1 denoting the fraction of
the observations randomly selected in each iteration of the gradient
boosting algorithm to propose the next tree. See \link{gbm} for
more details. Default: 1.0.}

\item{ps_n.minobsinnode}{An integer specifying the minimum number of observations
in the terminal nodes of the trees used in the gradient boosting.  See \link{gbm} for
more details. Default: 10.}

\item{ps_perm.test.iters}{A non-negative integer giving the number of iterations
of the permutation test for the KS statistic. If \code{perm.test.iters=0}
then the function returns an analytic approximation to the p-value. Setting
\code{perm.test.iters=200} will yield precision to within 3\% if the true
p-value is 0.05. Use \code{perm.test.iters=500} to be within 2\%. Default: 0.}

\item{ps_verbose}{If \code{TRUE}, lots of information will be printed to monitor the
the progress of the fitting. Default: \code{FALSE}.}

\item{ps_stop.method}{A method or methods of measuring and summarizing balance across pretreatment
variables. Current options are \code{ks.mean}, \code{ks.max}, \code{es.mean}, and \code{es.max}. \code{ks} refers to the
Kolmogorov-Smirnov statistic and es refers to standardized effect size. These are summarized
across the pretreatment variables by either the maximum (\code{.max}) or the mean (\code{.mean}).
Default: \code{c("ks.mean", "ks.max")}.}

\item{ps_version}{"gbm", "xgboost", or "legacy", indicating which version of the twang package to use.
\itemize{
\item \code{"gbm"} uses gradient boosting from the \code{\link[gbm]{gbm}} package.
\item \code{"xgboost"} uses gradient boosting from the \code{\link[xgboost]{xgboost}} package.
\item \code{"legacy"} uses the prior implementation of the \code{ps} function.
}}

\item{ps_ks.exact}{\code{NULL} or a logical indicating whether the
Kolmogorov-Smirnov p-value should be based on an approximation of exact
distribution from an unweighted two-sample Kolmogorov-Smirnov test. If
\code{NULL}, the approximation based on the exact distribution is computed
if the product of the effective sample sizes is less than 10,000.
Otherwise, an approximation based on the asymptotic distribution is used.
\strong{Warning:} setting \code{ks.exact = TRUE} will add substantial
computation time for larger sample sizes. Default: \code{NULL}.}

\item{ps_n.keep}{A numeric variable indicating the algorithm should only
consider every \code{n.keep}-th iteration of the propensity score model and
optimize balance over this set instead of all iterations. Default : 1.}

\item{ps_n.grid}{A numeric variable that sets the grid size for an initial
search of the region most likely to minimize the \code{stop.method}. A
value of \code{n.grid=50} uses a 50 point grid from \code{1:n.trees}. It
finds the minimum, say at grid point 35. It then looks for the actual
minimum between grid points 34 and 36.If specified with \code{n.keep>1}, \code{n.grid}
corresponds to a grid of points on the kept iterations as defined by \code{n.keep}. Default: 25.}

\item{ps_cv.folds}{A numeric variable that sets the number of cross-validation folds if
using method='crossval'. Default: 10.}

\item{ps_keep.data}{A logical variable that determines if the dataset should be saved
in the resulting \code{ps} model objects. Default: \code{FALSE}.}
}
\value{
mediation object
The \code{mediation} object includes the following:
\itemize{
\item \code{model_a} The model A \code{ps()} results.
\item \code{model_m1} The model M1 \code{ps()} results.
\item \code{model_m0} The model M0 \code{ps()} results.
\item \code{data} The data set used to compute models
\item \code{stopping_methods} The stopping methods passed to \code{stop.method}.
\item \code{datestamp} The date when the analysis was run.
\item For each \code{stop.method}, a list with the following:
\itemize{
\item \code{TE} The total effect.
\item \code{NDE_0} The natural direct effect, holding the mediator constant at 0.
\item \code{NIE_1} The natural indirect effect, holding the exposure constant at 1.
\item \code{NDE_1} The natural direct effect, holding the mediator constant at 1.
\item \code{NIE_0} The natural indirect effect, holding the exposure constant at 0.
\item \code{expected_treatment0_mediator0} E(Y(0, M(0)))
\item \code{expected_treatment1_mediator1} E(Y(1, M(1)))
\item \code{expected_treatment1_mediator0} E(Y(1, M(0)))
\item \code{expected_treatment0_mediator1} E(Y(0, M(1)))
}
}
}
\description{
Estimate causal mediation mechanism of a treatment
using propensity score weighting.
}
\details{
For users comfortable with \link{ps}, any options prefaced with
\code{ps_} are passed directly to the \code{ps()} function.
Model A is used to estimate Pr(A=1 | X) where X is the vector of background covariates specified in \code{formula.med}. If \code{method} equals \code{"ps"} model A is fit using the \link{twang} \code{ps} function with estimand= \code{"ATE"}. If \code{method} equals \code{"logistic"} then model A is fit using logistic regression. If \code{method} equals \code{"crossval"} then \link{gbm} using cross-validation is used to estimate model A. Because X might include variables not used to estimate the user-provided total effect weights, model A is fit rather than using the user-provided total effect weights to derive Pr(A | X). If the user uses the same set of variables to estimate their provided total effect weights as they enter in the wgtmed function to estimate the cross-world weights and the user uses the same estimation method and arguments as specified in the wgtmed function, then the estimated model A will match the model the user used to obtain the provided total effect weights.
}
\examples{
data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

head(tMdat)

## The tMdat data contains the following variables:
##   w1, w2, w3 -- Simulatad covariates
##   A   -- Simulated dichotomous exposure indicator
##   M   -- Simulated discrete mediator (11 values)
##   Y   -- Simulated continuous outcome
##   te.wgt -- Estimated inverse probability weight, estimated using 
##             GBM via the twang ps function

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

fit.es.max
}
\seealso{
\code{\link[twang]{ps}}
}
\keyword{models}
\keyword{multivariate}
