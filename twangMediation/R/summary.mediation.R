#' Displays a useful description of a `mediation` object.
#'
#' @param object A `mediation` object 
#' @param ... Additional arguments.
#'
#' @method summary mediation
#' @export
summary.mediation	<- 
function(object,...) 
{
   # Get confidence intervals of effects 
    if(!is.null(object$y_outcome)) {
      desc_effects  <- twangMediation:::desc.effects.mediation(object)
    }
    else {
      desc_effects  <- NULL
    }
    cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    for(i in length(desc_effects)) {
      cat(paste("95% Confidence Intervals for Effect Estimates:",names(desc_effects)[i],"\n"))
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
      print(round(desc_effects[[i]],digits=3))
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    }	
    
    ## Get the ESS for each of the weighted means used in the effect estimation
    ## Helper function used because I need ESS for for sets of weights and possibly  multiple stopping rules
    get_ess <- function(wgt, x){
       ww <- attr(x, wgt)
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
    cat("ESS for Total Effect and Cross-World Weights\n")
    cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    print(round(ess_table, digits=3))
    cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
   
    # Get summaries of model objects
    if(object$method=="ps") {
      model_a  <- twang:::summary.ps(object$model_a)
      model_m0 <- twang:::summary.ps(object$model_m0)
      model_m1 <- twang:::summary.ps(object$model_m1)
    } 
    if(object$method!="ps") {
      data <- object$data 

      if(object$method=="logistic") {
        model_a_preds <- predict(object$model_a,type="response")
      }
      if(object$method=="crossval") {     
        best.iter <- gbm:::gbm.perf(object$model_a, method="cv",plot.it=FALSE)
        model_a_preds <- predict(object$model_a, n.trees=best.iter, newdata=data, type="response")
      }

      wts_a <- ifelse(data[,object$a_treatment]==1,1/model_a_preds,1/(1-model_a_preds))

      dx_a <- twangMediation:::dx.wts.mediation(wts_a, data = data, 
          vars = object$covariate_names, treat.var = object$a_treatment, x.as.weights = TRUE, 
          estimand = "ATE")       
      dx_a$desc[[1]]["iter"] <- NA
      dx_a$desc[[2]]["iter"] <- NA
      names(dx_a$desc)[2] <- object$method
      model_a <- twang:::summary.ps(dx_a)
 
      data$trt0 <- 1-data[,object$a_treatment]
      if(object$method=="logistic") {
        model_m_preds <- predict(object$model_m0,type="link")
      }      
        else {
        best.iter <- gbm:::gbm.perf(object$model_m0, method="cv",plot.it=FALSE)
        model_m_preds <- predict(object$model_m0, n.trees=best.iter, newdata=data, type="link")
      }
      wts_m0 <- ifelse(data[,object$a_treatment]==0,1,1/exp(model_m_preds))
      dx_m0 <- twangMediation:::dx.wts.mediation(wts_m0, data = data, 
          vars = c(object$mediator_names,object$covariate_names), treat.var = "trt0", x.as.weights = TRUE, 
          estimand = "ATT")
      dx_m0$desc[[1]]["iter"] <- NA
      dx_m0$desc[[2]]["iter"] <- NA
      names(dx_m0$desc)[2] <- object$method
      dx_m0$desc$unw <- twangMediation:::swapTxCtrl(dx_m0$desc$unw)
      dx_m0$desc[[object$method]] <- twangMediation:::swapTxCtrl(dx_m0$desc[[object$method]])
      model_m0 <- twang:::summary.ps(dx_m0)

      wts_m1 <- ifelse(data[,object$a_treatment]==0,exp(model_m_preds),1)
      dx_m1 <- twangMediation:::dx.wts.mediation(wts_m1, data = data, 
        vars =  c(object$mediator_names,object$covariate_names), treat.var = object$a_treatment, x.as.weights = TRUE, 
        estimand = "ATT")
      dx_m1$desc[[1]]["iter"] <- NA
      dx_m1$desc[[2]]["iter"] <- NA
      names(dx_m1$desc)[2] <- object$method
      model_m1 <- twang:::summary.ps(dx_m1)
    }
    ps_tables  <- list(model_a=model_a,model_m0=model_m0,model_m1=model_m1)
    for(i in 1:length(ps_tables)) {
      cat(paste("Balance Summary Tables:",names(ps_tables)[i],"\n"))
      if(names(ps_tables)[[i]]=="model_a") {
        cat("Note: Model A is used for all effects: NDE_0, NDE_1, NIE_0, and NIE_1.\n")
      }
      if(names(ps_tables)[[i]]=="model_m0") {
        cat("Note: Model M0 is used for NDE_0 and NIE_1 effects.\n")
      }
      if(names(ps_tables)[[i]]=="model_m1") {
        cat("Note: Model M1 is used for NDE_1 and NIE_0 effects.\n")
      }
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
      print(round(ps_tables[[i]],digits=3))
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    }
  
    # Get balance tables for NIE_1 and NIE_0
    # to check that weights for the counterfactual 
    # mediator distributions yield distributions of 
    # mediators that match the target
    mediator_distribution_check <- twangMediation:::bal.table.mediation(object)[c("check_counterfactual_nie_1","check_counterfactual_nie_0")]
    for(i in 1:length(mediator_distribution_check)) {
      cat(paste("Mediator Distribution Check:",names(mediator_distribution_check)[[i]],"\n"))
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
      print(mediator_distribution_check[[i]])
      cat(paste(paste(rep('-', 90), collapse = ''), '\n', sep=''))
    }
    invisible(list(results_table = desc_effects, ess_table=ess_table, balance_summary_tables = ps_tables, mediator_distribution  = mediator_distribution_check ))
}

#summary.mediation.R1(fit.logit)
