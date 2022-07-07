#' Displays a useful description of a `mediation` object.
#'
#' @param object A `mediation` object 
#' @param ... Additional arguments.
#' @return \item{ps_tables}{Table of observations' propensity scores}
#' @return \item{mediator_distribution_check}{balance tables for NIE_1 and NIE_0}
#' @examples 
#' data("tMdat")
#' 
#' ## tMdat is small simulated data set included in twangMediation for 
#' ## demonstrating the functions. See ?tMdat for details
#' 
#' ## The tMdat data contains the following variables
#' ## See ?tMdat for details
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
#' summary(fit.es.max)
#' @seealso \code{\link{wgtmed}}
#' @method summary mediation
#' @export
summary.mediation	<- 
  function(object,...) 
  {
    # Get confidence intervals of effects 
    if(!is.null(object$y_outcome)) {
      desc_effects  <- desc.effects.mediation(object)
    }
    else {
      desc_effects  <- NULL
    }
    cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    for(i in 1:length(desc_effects)) {
      cat(paste("95% Confidence Intervals for Effect Estimates:",names(desc_effects)[i],"\n"))
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
      print(round(desc_effects[[i]],digits=3))
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    }	
    
    ## Get the ESS for each of the weighted means used in the effect estimation
    ## Helper function used because I need ESS for for sets of weights and possibly  multiple stopping rules
    get_ess <- function(wgt, x){
      ww <- attr(x, wgt)
      if(!is.null(attr(x,"sampw"))) {
         ww <- ww*attr(x,"sampw")
      }
      .ess <- apply(ww, 2, function(w){tmp <- na.omit(w)
      return(sum(tmp)^2/sum(tmp^2))})
      return(.ess) 
    }
    wnames <- c("w_00", "w_11", "w_10", "w_01")
    wess <- sapply(wnames, get_ess, x=object)
    wess <- matrix(wess, ncol=4)
    ntx <- sum(object$data[,object$a_treat])
    nctrl <- sum(1-object$data[,object$a_treat])
    ess_table <- rbind(c(nctrl, ntx, ntx, nctrl), wess)
    colnames(ess_table) <- c("E[Y(0, M(0))]", "E[Y(1, M(1))]", "E[Y(1, M(0))]", "E[Y(0, M(1))]") 
    rownames(ess_table) <- c("Sample Size", object$stopping_methods)
    cat("ESS for Total Effect and Cross-World Weights for estimating four population means used\nto estimate the total effect and the natural direct and indirect effects\n")
    if(!is.null(attr(object,"sampw"))) {
        methodnms <- object$stopping_methods
        if(length(methodnms)==1) { 
		methods <- paste0("\"",methodnms,"\" reflects")
	} 
	if(length(methodnms)==2) {
		methods <- character()
        	for(i in methodnms) {
        		methods <- c(methods,paste0("\"",i,"\""))
		}
		methods <- paste(paste(methods,collapse=" and "),"relfect")
	}
	if(length(methodnms)>2) {
		methods <- character()
        	for(i in methodnms[1:(length(methodnms)-1)]) {
        		methods <- c(methods,paste0("\"",i,"\""))
		}
		methods <- paste0(paste(methods,collapse=", "),", and \"",methodnms[length(methodnms)],"\" reflect")
	}
      cat(paste0("Note. Results for ",methods," weighting by both \nthe sampling weights and total-effect/cross-world weights.\n"))
    }
    cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    print(round(ess_table, digits=3))
    cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    
    ps_tables  <- lapply(object$dx.wts, function(x){tmp <- x$summary.tab
    tmp$iter <- NULL
    rownames(tmp) <- tmp$type
    tmp$type <- NULL
    return(tmp)})
    
    # add a row name to the 2nd row in the balance table for total effect
    rownames(ps_tables[[1]])[2] <- object$method

    for(i in 1:(length(ps_tables)-2)) {
      cat(paste("Balance Summary Tables:",names(ps_tables)[i],"\n"))
      if(names(ps_tables)[[i]]=="TE") {
        if(!is.null(attr(object,"sampw"))) {
          cat(paste0("Note: Balance for Covariates for Total Effects -- \n \"treat\" treatment group weighted by w11 weights, \n \"ctrl\" control group weighted by w00 weights \n \"unw\" reflects weighting with sampling weights only \n \"",rownames(ps_tables[["TE"]])[2],"\" reflects weighting by both the sampling weights and total-effect weights \n"))
        } else {
          cat("Note: Balance for Covariates for Total Effects -- \n \"treat\" treatment group weighted by w11 weights, \n \"ctrl\" control group weighted by w00 weights \n")
        }
      }else{ps_tables[[i]]<- ps_tables[[i]][-1,]# delete the "unw" rows
      } 
    if(names(ps_tables)[[i]]=="NIE1") {
      if(!is.null(attr(object,"sampw"))) {
        cat("Note: Balance for Covariates for NIE1 -- \n \"treat\" treatment group weighted by w11 weights, \n \"ctrl\" treatment group weighted by w10 weights \n Results reflect weighting by both the sampling weights and total-effect/cross-world weights \n")
      } else {
        cat("Note: Balance for Covariates for NIE1 -- \n \"treat\" treatment group weighted by w11 weights, \n \"ctrl\" treatment group weighted by w10 weights \n")
      }
    }
    if(names(ps_tables)[[i]]=="NDE0") {
      if(!is.null(attr(object,"sampw"))) {
        cat("Note: Balance for Covariates for NDE0 -- \n \"treat\" treatment group weighted by w10 weights, \n \"ctrl\" control group weighted by w00 weights \n Results reflect weighting by both the sampling weights and total-effect/cross-world weights \n")
      } else {
        cat("Note: Balance for Covariates for NDE0 -- \n \"treat\" treatment group weighted by w10 weights, \n \"ctrl\" control group weighted by w00 weights \n")
      }
    }
    if(names(ps_tables)[[i]]=="NIE0") {
      if(!is.null(attr(object,"sampw"))) {
        cat("Note: Balance for Covariates for NIE0 -- \n \"treat\" control group weighted by w01 weights, \n \"ctrl\" control group weighted by w00 weights \n Results reflect weighting by both the sampling weights and total-effect/cross-world weights \n")
      } else {
        cat("Note: Balance for Covariates for NIE0 -- \n \"treat\" control group weighted by w01 weights, \n \"ctrl\" control group weighted by w00 weights \n")
      }
    }
    if(names(ps_tables)[[i]]=="NDE1") {
      if(!is.null(attr(object,"sampw"))) {
        cat("Note: Balance for Covariates for NDE1 -- \n \"treat\" treatment group weighted by w11 weights, \n \"ctrl\" control group weighted by w01 weights \n Results reflect weighting by both the sampling weights and total-effect/cross-world weights \n")
      } else {
        cat("Note: Balance for Covariates for NDE1 -- \n \"treat\" treatment group weighted by w11 weights, \n \"ctrl\" control group weighted by w01 weights \n")
      }
    }
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
      print(round(ps_tables[[i]],digits=3))
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    }
    
    # Get balance tables for NIE_1 and NIE_0
    # to check that weights for the counterfactual 
    # mediator distributions yield distributions of 
    # mediators that match the target
    mediator_distribution_check <- bal.table.mediation(object)[c("check_counterfactual_nie_1","check_counterfactual_nie_0")]
    #    for(i in 1:length(mediator_distribution_check)) {
    #      cat(paste("Mediator Distribution Check:",names(mediator_distribution_check)[[i]],"\n"))
    #      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    #      print(mediator_distribution_check[[i]])
    #      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    #    }
    invisible(list(results_table = desc_effects, ess_table = ess_table, balance_summary_tables = ps_tables, mediator_distribution  = mediator_distribution_check ))
  }
