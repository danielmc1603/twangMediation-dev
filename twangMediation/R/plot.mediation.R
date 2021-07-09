globalVariables("A") # This gets rid of a warning related to variable "A"
#' Plot the `mediation` object.
#'
#' @param x weighted_mediation object
#' @param plots An indicator of which type of plot is desired. The options are
#'   * `"optimize"` A plot of the balance criteria as a function of the GBM
#'     iteration.
#'   * `"boxplot"` Boxplots of the propensity scores for the treatment and
#'     control cases
#'   * `"es"` or `"asmd"` Plots of the absolute value of the standardized mean difference (effect size) of the pre-treatment
#'     variables before and after reweighting
#'   * `"density"` Distribution plots of NIE1 (distribution of mediator for treatment
#'     sample weighted to match distribution of mediator under control for the population)
#'     and NIE0 (distribution of mediator for control sample weighted to match 
#'     distribution of mediator under treatment for the population) for each mediator.
#'     For continuous mediators, distributions are plotted with density curves and 
#'     for categorical (factor) mediators, distributions are plotted with barplots. 
#'   * `"weights"` Histograms of the standardized weights by each stopping rule. Weights
#'     are standardized to sum to 1.  
#' @param subset Used to restrict which of the `stop.method`s will be used
#'   in the figure. For example `subset = c(1,3)` would indicate that the
#'   first and third `stop.method`s (in alphabetical order of those specified
#'   in the original call to the mediation function) should be included in the
#'   figure. If x$method = `logistic` or `crossval`, there is no need to subset 
#'   as there is only one method used. 
#' @param color If `color = FALSE`, figures will be gray scale. Default: `TRUE`.
#' @param model_subset integer
#'   Choose either model A (1), model M0 (2), or model M1 (3) only. Argument is
#'   not relevant for plots = `density' or `weights'.
#' @param ... Additional arguments.
#' @examples
#' \dontrun{
#' data(NSDUH_female)
#' # Calculate total effect weighs
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
#' # Plot cig_med
#' plot(cig_med)
#' }
#' @return The plot of a mediation object, can be different types.
#' @method plot mediation
#' @export
plot.mediation <- function(x,
                           plots = "optimize",
                           subset = NULL,
                           color = TRUE,
                           model_subset = NULL,
                           ...) {
  
  # return error if model subset is not 1, 2 or NULL
  if (!is.null(model_subset) && !(model_subset %in% c(1, 2, 3))) {
    stop("The `model_subset` must be NULL, 1, 2, or 3.")
  }
  
  # return error if ask for any plots other than available 
  if (!plots %in% c("optimize","boxplot","es","asmd","density","weights")) {
     stop("The `plots` options must be `optimize`,`boxplot`,`asmd` (or `es`), `density`, or `weights`.")
  }
  # return error if ask for plots="optimize" or 1 for method!=ps
  if (x$method!="ps" & (plots=="optimize" || plots==1)) { 
   stop("The `optimize` plot is only available for method='ps'.")}
 
  # return error if subset doesn't reflect available stopping rules
  if (!is.null(subset) & any(!subset %in% 1:length(x$stopping_methods))) {
    stop(paste0("The `subset` argument must be NULL or only include integers for each of the available (n=",length(x$stopping_methods),") stopping methods."))}

  # we want the mediator and the treatment variables
  mediators <- x$data[,x$mediator_names, drop = F]
  treatment <- x$data[[x$a_treatment]]
  
  if (!plots %in% c('density','weights')) {
    args <- list(plots = plots, subset = subset, color = color)
    if(x$method=="logistic") {
       x$model_a$ps  <- data.frame(logistic=predict(x$model_a,type="response"))
       x$model_m0$ps  <- data.frame(logistic=predict(x$model_m0,type="response"))
       x$model_m1$ps  <- data.frame(logistic=predict(x$model_m1,type="response"))
       model_m_preds <- predict(x$model_m0,type="link")
       x$model_a$w  <- data.frame(logistic=ifelse(x$data[,x$a_treatment]==1,1/x$model_a$ps[,1],1/(1-x$model_a$ps[,1])))
       x$model_m0$w  <- data.frame(logistic=ifelse(x$data[,x$a_treatment]==1,1/exp(model_m_preds),1))
       x$model_m1$w  <- data.frame(logistic=ifelse(x$data[,x$a_treatment]==1,1,exp(model_m_preds)))
	 x$model_a$treat <- x$data[,x$a_treatment]
	 x$model_m0$treat <- x$data[,x$a_treatment]
	 x$model_m1$treat <- x$data[,x$a_treatment]

	x$model_a$desc  <- vector("list",length=2)
	names(x$model_a$desc)  <- c("unw","logistic")
	bala	<- bal.table.mediation(x)
	x$model_a$desc[[1]]$bal.tab$results <- bala$balance_a[grep("unw",rownames(bala$balance_a)),1:8]
	x$model_a$desc[[2]]$bal.tab$results <- bala$balance_a[grep("logistic",rownames(bala$balance_a)),1:8]

	x$model_m0$desc  <- vector("list",length=2)
	names(x$model_m0$desc)  <- c("unw","logistic")
	x$model_m0$desc[[1]]$bal.tab$results <- bala$balance_m0[grep("unw",rownames(bala$balance_m0)),1:8]
	x$model_m0$desc[[2]]$bal.tab$results <- bala$balance_m0[grep("logistic",rownames(bala$balance_m0)),1:8]

	x$model_m1$desc  <- vector("list",length=2)
	names(x$model_m1$desc)  <- c("unw","logistic")
	x$model_m1$desc[[1]]$bal.tab$results <- bala$balance_m1[grep("unw",rownames(bala$balance_m1)),1:8]
	x$model_m1$desc[[2]]$bal.tab$results <- bala$balance_m1[grep("logistic",rownames(bala$balance_m1)),1:8]
    }
    if(x$method=="crossval") {
       best.iter.a 		<- gbm::gbm.perf(x$model_a, method="cv",plot.it=FALSE)
       best.iter.m 		<- gbm::gbm.perf(x$model_m0, method="cv",plot.it=FALSE)
       x$model_a$ps  <- data.frame(crossval=predict(x$model_a, n.trees=best.iter.a,newdata=x$data,type="response"))
       x$model_m0$ps  <- data.frame(crossval=predict(x$model_m0,n.trees=best.iter.m,newdata=x$data,type="response"))
       x$model_m1$ps  <- data.frame(crossval=predict(x$model_m1,n.trees=best.iter.m,newdata=x$data,type="response"))
       model_m_preds <- predict(x$model_m0, n.trees=best.iter.m, newdata=x$data, type="link")
       x$model_a$w  <- data.frame(crossval=ifelse(x$data[,x$a_treatment]==1,1/x$model_a$ps[,1],1/(1-x$model_a$ps[,1])))
       x$model_m0$w  <- data.frame(crossval=ifelse(x$data[,x$a_treatment]==1,1/exp(model_m_preds),1))
       x$model_m1$w  <- data.frame(crossval=ifelse(x$data[,x$a_treatment]==1,1,exp(model_m_preds)))
	 x$model_a$treat <- x$data[,x$a_treatment]
	 x$model_m0$treat <- x$data[,x$a_treatment]
	 x$model_m1$treat <- x$data[,x$a_treatment]

	x$model_a$desc  <- vector("list",length=2)
	names(x$model_a$desc)  <- c("unw","crossval")
	bala	<- bal.table.mediation(x)
	x$model_a$desc[[1]]$bal.tab$results <- bala$balance_a[grep("unw",rownames(bala$balance_a)),1:8]
	x$model_a$desc[[2]]$bal.tab$results <- bala$balance_a[grep("crossval",rownames(bala$balance_a)),1:8]

	x$model_m0$desc  <- vector("list",length=2)
	names(x$model_m0$desc)  <- c("unw","crossval")
	x$model_m0$desc[[1]]$bal.tab$results <- bala$balance_m0[grep("unw",rownames(bala$balance_m0)),1:8]
	x$model_m0$desc[[2]]$bal.tab$results <- bala$balance_m0[grep("crossval",rownames(bala$balance_m0)),1:8]

	x$model_m1$desc  <- vector("list",length=2)
	names(x$model_m1$desc)  <- c("unw","crossval")
	x$model_m1$desc[[1]]$bal.tab$results <- bala$balance_m1[grep("unw",rownames(bala$balance_m1)),1:8]
	x$model_m1$desc[[2]]$bal.tab$results <- bala$balance_m1[grep("crossval",rownames(bala$balance_m1)),1:8]
    }
    if(x$method=="ps") {
	  ## in model m0 the ps function estimate the probability of 1 - tx but we want 
	  ## to plot the probability of tx since it gives the same information of overlap but is less confusing
	  ## for end users.
	  x$model_m0$ps  <- 1 - x$model_m0$ps
	  }
    model_a <- x$model_a
    model_m0 <- x$model_m0
    model_m1 <- x$model_m1
    model_names <- c('Model A', 'Model M0', 'Model M1')
    
    plot1 <- do.call(diag.plot.color, c(list(model_a), args))
    plot2 <- do.call(diag.plot.color, c(list(model_m0), args))
    plot3 <- do.call(diag.plot.color, c(list(model_m1), args))

    plot1 <- update(plot1, ylab.right = model_names)
    if (is.null(model_subset)) {
      new_plot <- suppressWarnings(c(plot1, plot2))
      new_plot <- suppressWarnings(c(new_plot,plot3))
      new_plot <- update(new_plot, ylab.right = rev(model_names),layout=c(length(plot1$packet.sizes),length(new_plot$packet.sizes)/length(plot1$packet.sizes)))
    } else if (model_subset == 1) {
      new_plot <- update(plot1, ylab.right = model_names[[1]])
    } else if (model_subset == 2) {
      new_plot <- update(plot2, ylab.right = model_names[[2]])
    } else {
      new_plot <- update(plot3, ylab.right = model_names[[3]])
    }
    new_plot$as.table	<- TRUE
    return(new_plot)
  }

  # indicate which stopping rules to plot for density and weights plots
  if(is.null(subset)) {
    whichmethods <- 1:length(x$stopping_methods)
  } else {
    whichmethods <- subset
  }

  if(plots=='density') {
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

     oldpar <- par(no.readonly = TRUE)   
     on.exit(par(oldpar))            
     
     if(which_nie == 1){
        # Plot to check that the weighted counterfactual density of mediator from the treatment sample 
        # matches the estimated population density of the mediator under control or M(0)  
        # The population sample is control and its weight is w_00
        # The counterfactual is M(0) from the treatment sample and its weight is w_10
        
        # get the actual weights from the mediation object
        w_pop <- attr(x, 'w_00')
        w_cfac <- attr(x, 'w_10')
      
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
                                     main = ptitle,
                                     horiz = FALSE,
                                     cex.main=0.85)
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
                                        main = ptitle,
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

  if (plots=='weights') {
  w_00 <- attr(x,'w_00')
  w_11 <- attr(x,'w_11')
  w_01 <- attr(x,'w_01')
  w_10 <- attr(x,'w_10')

  cask <- par()$ask
  for (i in whichmethods) {
    weight_plot <- vector("list",4)
    pos <- 0
    for(w in c('00','11','01','10')) {
      pos <- pos+1
      wt <- get(paste0('w_',w)) 
      weight_plot[[pos]] <- lattice::histogram(wt[,i]/sum(wt[,i],na.rm=TRUE),
                                                xlab='Weight',
                                                main=paste0('w_Y(',substr(w,1,1),",M(",substr(w,2,2),"))",'\n',x$stopping_methods[i]),
                                                col='light blue',
                                                type='density')
    }
      gridExtra::grid.arrange(weight_plot[[1]],weight_plot[[2]],weight_plot[[3]],weight_plot[[4]],ncol=2)
      cc <- par()$ask
      par(ask=TRUE)
  }
  par(ask=cask)
  }



}
