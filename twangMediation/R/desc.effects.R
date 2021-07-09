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
#' #' data(NSDUH_female)
#' TEps <- ps(formula = lgb_flag ~ age + race + educ + income + employ,
#' data=NSDUH_female, verbose=F, n.trees=6000, estimand="ATE", stop.method="ks.mean")
#' # Obtain mediaiton estimates of interest
#' cig_med <- wgtmed(formula.med = cig15 ~ age + race + educ + income + employ,
#'    a_treatment="lgb_flag",
#'    y_outcome="cigmon",
#'    data=NSDUH_female,
#'    method="ps",
#'    total_effect_ps=TEps,
#'    total_effect_stop_rule="ks.mean",
#'    ps_version="gbm",
#'    ps_n.trees=6000,
#'    ps_interaction.depth=3,
#'    ps_shrinkage=0.01,
#'    ps_stop.method="ks.mean",
#'    ps_verbose=FALSE)
#' # describe the effects, standard errors, and confidence intervals
#' desc.effects(cig_med)
#' }
#' @return {Effects, standard errors and confidence intervals of an object}
#' @export
desc.effects <- function (x, ...) {
   UseMethod("desc.effects", x)
}
