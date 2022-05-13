globalVariables("A") # This gets rid of a warning related to variable "A"
#' Plot the `mediation` object.
#'
#' @param x weighted_mediation object
#' @param subset Used to restrict which of the `stop.method`s will be used
#'   in the figure. For example `subset = c(1,3)` would indicate that the
#'   first and third `stop.method`s (in alphabetical order of those specified
#'   in the original call to the mediation function) should be included in the
#'   figure. If x$method = `logistic` or `crossval`, there is no need to subset 
#'   as there is only one method used. 
#' @param color If `color = FALSE`, figures will be gray scale. Default: `TRUE`.
#' @param ... Additional arguments.
#' @return Distribution plots of NIE1 (distribution of mediator for treatment
#'     sample weighted to match distribution of mediator under control for the population)
#'     and NIE0 (distribution of mediator for control sample weighted to match 
#'     distribution of mediator under treatment for the population) for each mediator.
#'     For continuous mediators, distributions are plotted with density curves and 
#'     for categorical (factor) mediators, distributions are plotted with barplots. .
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
#' plot(fit.es.max)
#' @seealso \code{\link{wgtmed}} for function input
#' @method plot mediation
#' @export
plot.mediation <- function(x,
                           subset = NULL,
                           color = TRUE,
                            ...) {
  
  # return error if subset doesn't reflect available stopping rules
  if (!is.null(subset) & any(!subset %in% 1:length(x$stopping_methods))) {
    stop(paste0("The `subset` argument must be NULL or only include integers for each of the available (n=",length(x$stopping_methods),") stopping methods."))}

  # we want the mediator and the treatment variables
  mediators <- x$data[,x$mediator_names, drop = F]
  treatment <- x$data[[x$a_treatment]]
  
  # indicate which stopping rules to plot 
  if(is.null(subset)) {
    whichmethods <- 1:length(x$stopping_methods)
  } else {
    whichmethods <- subset
  }

  oldpar <- par(no.readonly = TRUE)   
  on.exit(suppressWarnings(par(oldpar)), add=TRUE)

  # we separate out the mediators that are factors and those that aren't
  mediator_is_factor <- x$mediator_names %in% names(Filter(is.factor, x$data[,x$mediator_names, drop=F]))
  
  ##check if any mediators binary and make factors 
  for(i in 1:length(x$mediator_names)) {
    if(length(unique(x$data[,x$mediator_names[i]]))<=5 & mediator_is_factor[i]==FALSE) {
      mediator_is_factor[i] <- TRUE
      warning(paste("Mediator",x$mediator_names[i],"is being treated like a factor to plot distributions
       with barplots instead of with density curves."))
	}
  }

  mediators_factors <- x$mediator_names[mediator_is_factor == T, drop = T]
  mediators_numbers <- x$mediator_names[mediator_is_factor == F, drop = T]
  
  ## Create a function for generating the plots so it can be replicated for NIE1 and NIE0

  genplot <- function(which_nie){

     if(which_nie == 1){
        # Plot to check that the weighted counterfactual density of mediator from the treatment sample 
        # matches the estimated population density of the mediator under control or M(0)  
        # The population sample is control and its weight is w_00
        # The counterfactual is M(0) from the treatment sample and its weight is w_10
        
        # get the actual weights from the mediation object
        w_pop <- attr(x, 'w_00')
        w_cfac <- attr(x, 'w_10')

	  ##multiply by sampling wts
        sampw <- attr(x,"sampw")
        if(!is.null(sampw)) {
		w_pop <- w_pop*sampw
            w_cfac <- w_cfac*sampw
         }
      
        # finally, create indicators for treatment and control
        cfac <- which(treatment == 1)
        pop  <- which(treatment == 0)
     
     }else{
        # Plot to check that the weighted counterfactual density of mediator from the control sample 
        # matches the estimated population density of the mediator under treatment or M(1)  
        # The population sample is treatment and its weight is w_11
        # The counterfactual is M(1) from the treatment sample and its weight is w_01
  
        # get the actual weights from the mediation object
        # w_pop is the population wait -- now f
        w_pop <- attr(x, 'w_11')
        w_cfac <- attr(x, 'w_01')

	  ##multiply by sampling wts
        sampw <- attr(x,"sampw")
        if(!is.null(sampw)) {
		w_pop <- w_pop*sampw
            w_cfac <- w_cfac*sampw
         }
   
        # finally, create indicators for treatment and control
        cfac <- which(treatment == 0)
        pop  <- which(treatment == 1)
        }
        
     if(which_nie==1){
        ptitle <- "NIE1: Distribution of Mediator for Treatment Sample Weighted to Match \n Distribution of Mediator under Control for the Population"
     }else{
        ptitle <- "NIE0: Distribution of Mediator for Control Sample Weighted to Match \n Distribution of Mediator under Treatment for the Population"
     }
  
  if(color) {
    cols <- c("#478BB8", "#B87447","#ff8c00")
  } else { cols <- c("black","gray80","gray100") }
  stripBgCol <- ifelse(color, "#ffe5cc", "transparent")

  factor_plot <- NULL
  if (any((mediator_is_factor == T))) {
    factor_frames <- list()
    for (i in whichmethods) {
      for (m in mediators_factors) {
        
        method <- x$stopping_methods[i]
        weights <- ifelse(!is.na(w_cfac[,i]), w_cfac[,i], w_pop[,i])
        
        weights[cfac] <- (weights[cfac] / sum(weights[cfac]))
        weights[pop] <- (weights[pop] / sum(weights[pop]))
        
        a1 <- aggregate(weights[cfac], by = list(m = factor(mediators[cfac, m])), FUN = sum)
        a0 <- aggregate(weights[pop], by = list(m = factor(mediators[pop, m])), FUN = sum)

        a1[x$a_treatment] <- 1; a0[x$a_treatment] <- 0 

        
        combined <- rbind(a1, a0)
        combined['mediator'] <- m
        combined['method'] <- method
        
        factor_frames[[paste(method, m, sep='')]] <- combined
      }
    }
    factor_data <- do.call(rbind, factor_frames)
    trt <- factor_data[,x$a_treatment]
    factor_plot <- vector("list",length(mediators_factors))
    pos <- 0
    for(mm in mediators_factors) {
      pos <- pos+1   
      factor_plot[[pos]] <- lattice::barchart(x ~ factor(m) | factor(method) + factor(mediator),
                                     groups = factor(trt, levels = c(0,1), labels = c('population', 'counterfactual')),
                                     data = factor_data[factor_data$mediator==mm,],                                   
                                     origin = 0, 
                                     par.settings = list(superpose.polygon = list(col = cols),strip.background = list(col=stripBgCol)),
                                     auto.key = TRUE,
                                     ylab = "Proportion",
                                     xlab = "Weighted Mediator",
                                     main = list(label=ptitle,cex=.8),
                                     horiz = FALSE,
                                     cex.main=0.6)
  }
  }
  
  number_plot <- NULL
  if (any((mediator_is_factor == F))) {
    number_frames <- list()
    for (i in whichmethods) {
      for (m in mediators_numbers) {
        
        method <- x$stopping_methods[i]
        weights <- ifelse(!is.na(w_cfac[,i]), w_cfac[,i], w_pop[,i])
        
        weights[cfac] <- (weights[cfac] / sum(weights[cfac]))
        weights[pop] <- (weights[pop] / sum(weights[pop]))

       ##define vector that is 1 for cfac indices and 0 otherwise so that A=1 is always the counterfactual
       cfacind <- rep(0,length(weights))
       cfacind[cfac] <- 1

        number_frames[[paste(method, m, sep='')]] <- data.frame('m' = mediators[,m],
                                                                'mediator' = m,
                                                                'A' = cfacind,
                                                                'weights' = weights,
                                                                'method' = method)
      }
    }
    number_data <- do.call(rbind, number_frames)
    number_plot <- vector("list",length(mediators_numbers))
    pos <- 0
    for(mm in mediators_numbers) {
      pos <- pos+1
      number_plot[[pos]] <- lattice::densityplot(~m | factor(method) + factor(mediator),
                                        groups = factor(A, levels = c(0,1), labels = c('population', 'counterfactual')),
                                        data = number_data[number_data$mediator==mm,],
                                        weights = weights,
                                        plot.points = FALSE,
                                        origin = 0,
                                        par.settings = list(superpose.line = list(lwd = 3, col = cols),strip.background = list(col=stripBgCol)),
                                        auto.key = TRUE,
                                        ylab = "Density",
                                        xlab = "Weighted Mediator",
                                        main = list(label=ptitle,cex=.8),
                                        cex.main=0.55)
    }
  }
  
  if (!is.null(factor_plot) && !is.null(number_plot)) {
    for(i in 1:length(factor_plot)) {
      plot(factor_plot[[i]])
      cc <- par()$ask
      par(ask=TRUE)
    }
      for(i in 1:length(number_plot)) {
      plot(number_plot[[i]])
      cc <- par()$ask
      par(ask=TRUE)
    }
       par(ask=cc)
  } else if (!is.null(factor_plot)) {
    for(i in 1:length(factor_plot)) {
      plot(factor_plot[[i]])
      cc <- par()$ask
      par(ask=TRUE)
    }
     par(ask=cc)
  } else {
      for(i in 1:length(number_plot)) {
      plot(number_plot[[i]])
      cc <- par()$ask
      par(ask=TRUE)
    }
     par(ask=cc)
  }
} # closes function genplot

  cask <- par()$ask

  # Run the density plots for NIE1
  genplot(which_nie=1)

  # Run the density plots for NIE0
  par(ask=TRUE)
  genplot(which_nie=0)
 
  par(ask=cask)


}
