#' Describe the effects
#' 
#' Describe the effects, and calculate standard
#' errors and confidence intervals
#' 
#' @param x An object
#' @param ... list, optional
#'   Additional arguments.
#' @examples 
#' \dontrun{
#' # describe the effects of cig_med, a mediation object
#' desc.effects(cig_med)
#' }
#' @return {Effects, standard errors and confidence intervals of an object}
#' @seealso \code{\link{desc.effects.mediation}}, \code{\link{wgtmed}}
#' @export
desc.effects <- function (x, ...) {
   UseMethod("desc.effects", x)
}
