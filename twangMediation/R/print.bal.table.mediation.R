#' Default print statement for `mediation` class
#'
#' @param x A `bal.table.mediation` object.
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
#' bal.table.mediation(fit.es.max)
#' @seealso \code{\link{bal.table.mediation}}, \code{\link{wgtmed}}
#' @method print bal.table.mediation
#' @export
print.bal.table.mediation <- function(x,...){
        
        if(inherits(x,"bal.table.mediation")==FALSE){stop("x must be of class bal.table.mediation")}

        methodnms <- names(x$NIE1)
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

        
        cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
        for(i in 1:5){
                if(names(x)[[i]]=="TE") {
                        if(attr(x,"sampw")==TRUE) {
                        	cat(paste0("Note: Balance for Covariates for Total Effects -- \n \"tx\" treatment group weighted by w11 weights, \n \"ct\" control group weighted by w00 weights \n \"unw\" reflects weighting with sampling weights only \n \"",names(x[["TE"]])[2],"\" reflects weighting by both the sampling weights and total-effect weights \n"))
			} else {
				cat("Note: Balance for Covariates for Total Effects -- \n \"tx\" treatment group weighted by w11 weights, \n \"ct\" control group weighted by w00 weights \n")
			}
                }
                if(names(x)[[i]]=="NIE1") {
                       if(attr(x,"sampw")==TRUE) {
                        	cat(paste0("Note: Balance for Covariates for NIE1 -- \n \"tx\" treatment group weighted by w11 weights, \n \"ct\" treatment group weighted by w10 weights \n Results reflect weighting by both the sampling weights and total-effect/cross-world weights \n"))
			} else {
                        	cat("Note: Balance for Covariates for NIE1 -- \n \"tx\" treatment group weighted by w11 weights, \n \"ct\" treatment group weighted by w10 weights \n")
			}
                }
                if(names(x)[[i]]=="NDE0") {
                       if(attr(x,"sampw")==TRUE) {
                        	cat(paste0("Note: Balance for Covariates for NDE0 -- \n \"tx\" treatment group weighted by w10 weights, \n \"ct\" control group weighted by w00 weights \n Results reflect weighting by both the sampling weights and total-effect/cross-world weights \n"))
			} else {
				cat("Note: Balance for Covariates for NDE0 -- \n \"tx\" treatment group weighted by w10 weights, \n \"ct\" control group weighted by w00 weights \n")
			}
                }
                if(names(x)[[i]]=="NIE0") {
                       if(attr(x,"sampw")==TRUE) {
	                        cat(paste0("Note: Balance for Covariates for NIE0 -- \n \"tx\" control group weighted by w01 weights, \n \"ct\" control group weighted by w00 weights \n Results reflect weighting by both the sampling weights and total-effect/cross-world weights \n"))
        		} else {
                        cat("Note: Balance for Covariates for NIE0 -- \n \"tx\" control group weighted by w01 weights, \n \"ct\" control group weighted by w00 weights \n")
        		}
	        }
                if(names(x)[[i]]=="NDE1") {
                       if(attr(x,"sampw")==TRUE) {
                        	cat(paste0("Note: Balance for Covariates for NDE1 -- \n \"tx\" treatment group weighted by w11 weights, \n \"ct\" control group weighted by w01 weights \n Results reflect weighting by both the sampling weights and total-effect/cross-world weights \n"))
    		       } else {
	           		cat("Note: Balance for Covariates for NDE1 -- \n \"tx\" treatment group weighted by w11 weights, \n \"ct\" control group weighted by w01 weights \n")
    		   	}
		}
                cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
                cat(paste0("$",names(x)[i]," \n"))
                for(j in 1:length(x[[i]])){
                        cat(paste0("\n$",names(x[[i]])[j]," \n"))
                        print(round(x[[i]][[j]],digits=3))
                }
                cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
        }
        
        for(i in 6:7) {
                cat(paste("Mediator Distribution Check:",names(x)[i],"\n"))
		if(attr(x,"sampw")==TRUE) {
		  cat(paste0("\"unw\" reflects weighting with sampling weights only \n",methods," weighting by both the sampling weights and total-effect/cross-world weights \n"))
		}
                cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
                print(round(x[[i]], digits=3))
                cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
        }
        
        
        
        if("details" %in% names(x)){
                # Print the note
                cat(attr(x$details,"note"))
                
                nn <- names(x$details)
                for(i in nn){
                        cat(paste0("\n$",i," \n"))
                        print(x$details[[i]])
                }
        }
}
