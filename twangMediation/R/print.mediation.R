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
  print(list(estimates_table = estimates_table))
  
  ps_tables  <- lapply(x$dx.wts, function(x){tmp <- x$summary.tab
  tmp$iter <- NULL
  rownames(tmp) <- tmp$type
  tmp$type <- NULL
  return(tmp)})
  
  # add the row name for the 2nd row in total effect ps_tables
  rownames(ps_tables[[1]])[2] <- rownames(ps_tables[[2]])[2]

  ###### Add weight-related notes####
  for (i in 1:5){
    if(names(ps_tables)[[i]]=="TE") {
      cat("Note: Balance for Covariates for Total Effects -- \n \"treat\" treatment group weighted by w11 weights, \n \"ctrl\" control group weighted by w00 weights \n")
    }else{ps_tables[[i]]<- ps_tables[[i]][2,] # delete the "unw" rows
    }
    if(names(ps_tables)[[i]]=="NIE1") {
      cat("Note: Balance for Covariates for NIE1 -- \n \"treat\" treatment group weighted by w11 weights, \n \"ctrl\" treatment group weighted by w10 weights \n")
    }
    if(names(ps_tables)[[i]]=="NDE0") {
      cat("Note: Balance for Covariates for NDE0 -- \n \"treat\" treatment group weighted by w10 weights, \n \"ctrl\" control group weighted by w00 weights \n")
    }
    if(names(ps_tables)[[i]]=="NIE0") {
      cat("Note: Balance for Covariates for NIE0 -- \n \"treat\" group group weighted by w01 weights, \n \"ctrl\" control group weighted by w00 weights \n")
    }
    if(names(ps_tables)[[i]]=="NDE1") {
      cat("Note: Balance for Covariates for NDE1 -- \n \"treat\" treatment group weighted by w11 weights, \n \"ctrl\" control group weighted by w01 weights \n")
    }
    cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    print(round(ps_tables[[i]],digits=3))
    cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
  }
  ##### End of code for weight-related notes##  
  
  # Get balance tables for NIE_1 and NIE_0
  # to check that weights for the counterfactual 
  # mediator distributions yeild distributions of 
  # mediators that match the target
  mediator_distribution_check <- bal.table.mediation(x)[c("check_counterfactual_nie_1","check_counterfactual_nie_0")]
  
  print(list(mediator_distribution = mediator_distribution_check))
}
