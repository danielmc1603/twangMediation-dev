#' Default print statement for `mediation` class
#'
#' @param x A `mediation` object.
#' @param ... Additional arguments.
#' @return Default print statement.
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
#' print(fit.es.max)
#' @seealso \code{\link{wgtmed}} for in put.
#' @method print mediation
#' @export
print.mediation <- function(x, ...)
{
  # Grab the effects, if they exist
  effects_logical <- grepl('_effects', names(x))
  if (any(effects_logical)) {
    estimates_table <- x[effects_logical]
  } else {
    estimates_table <- NULL
  }

    ps_tables  <- lapply(x$dx.wts, function(x){tmp <- x$summary.tab
                                               tmp$iter <- NULL
                                               rownames(tmp) <- tmp$type
                                               tmp$type <- NULL
                                               return(tmp)})
  
  # Get balance tables for NIE_1 and NIE_0
  # to check that weights for the counterfactual 
  # mediator distributions yeild distributions of 
  # mediators that match the target
  mediator_distribution_check <- bal.table.mediation(x)[c("check_counterfactual_nie_1","check_counterfactual_nie_0")]

  print(list(estimates_table = estimates_table, covariate_balance_tables = ps_tables[1:5], mediator_distribution = mediator_distribution_check))
}
