#' Describe the effects
#' 
#' Describe the effects, and calculate standard
#' errors and confidence intervals
#' 
#' @param x An object
#' @param ... list, optional
#'   Additional arguments.
#' @return {Effects, standard errors and confidence intervals of an object}
#' @examples 
#' data("tMdat")
#' 
#' ## tMdat is small simulated data set included in twangMediation for 
#' ## demonstrating the functions. See ?tMdat for details
#' 
#' fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
#'                       data = tMdat,
#'                       a_treatment = "A",
#'                       y_outcome = "Y",
#'                       total_effect_wts = tMdat$te.wgt,
#'                       method = "ps",
#'                       ps_n.trees=1500,
#'                       ps_shrinkage=0.01,
#'                       ps_stop.method=c("es.max")
#'                       )
#' 
#' desc.effects(fit.es.max)
#' @seealso \code{\link{desc.effects.mediation}}, \code{\link{wgtmed}}
#' @export
desc.effects <- function (x, ...) {
   UseMethod("desc.effects", x)
}
