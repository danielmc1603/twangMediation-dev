#' Default print statement for `mediation` class
#'
#' @param x A `mediation` object.
#' @param ... Additional arguments.
#' 
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

  # Get summaries of ps xs
    if(x$method=="ps") {
      model_a  <- twang:::summary.ps(x$model_a)
      model_m0 <- twang:::summary.ps(x$model_m0)
      model_m1 <- twang:::summary.ps(x$model_m1)
    } 
    if(x$method!="ps") {
      data <- x$data 

      if(x$method=="logistic") {
        model_a_preds <- predict(x$model_a,type="response")
      } else {     
        best.iter <- gbm::gbm.perf(x$model_a, method="cv",plot.it=FALSE)
        model_a_preds <- predict(x$model_a, n.trees=best.iter, newdata=data, type="response")
      }

      wts_a <- ifelse(data[,x$a_treatment]==1,1/model_a_preds,1/(1-model_a_preds))
      dx_a <- twangMediation:::dx.wts.mediation(wts_a, data = data, 
          vars = x$covariate_names, treat.var = x$a_treatment, x.as.weights = TRUE, 
          estimand = "ATE")      
      dx_a$desc[[1]]["iter"] <- NA
      dx_a$desc[[2]]["iter"] <- NA
      names(dx_a$desc)[2] <- x$method
      model_a <- twang:::summary.ps(dx_a)

      data$trt0 <- 1-data[,x$a_treatment]
      if(x$method=="logistic") {
        model_m_preds <- predict(x$model_m0,type="link")
      }      
        else {
        best.iter <- gbm::gbm.perf(x$model_m0, method="cv",plot.it=FALSE)
        model_m_preds <- predict(x$model_m0, n.trees=best.iter, newdata=data, type="link")
      }
      wts_m0 <- ifelse(data[,x$a_treatment]==0,1,1/exp(model_m_preds))
      dx_m0 <- twangMediation:::dx.wts.mediation(wts_m0, data = data, 
          vars = c(x$mediator_names,x$covariate_names), treat.var = "trt0", x.as.weights = TRUE, 
          estimand = "ATT")
      dx_m0$desc[[1]]["iter"] <- NA
      dx_m0$desc[[2]]["iter"] <- NA
      names(dx_m0$desc)[2] <- x$method
      dx_m0$desc$unw <- twangMediation:::swapTxCtrl(dx_m0$desc$unw)
      dx_m0$desc[[x$method]] <- twangMediation:::swapTxCtrl(dx_m0$desc[[x$method]])
      model_m0 <- twang:::summary.ps(dx_m0)

      wts_m1 <- ifelse(data[,x$a_treatment]==0,exp(model_m_preds),1)
      dx_m1 <- twangMediation:::dx.wts.mediation(wts_m1, data = data, 
        vars =  c(x$mediator_names,x$covariate_names), treat.var = x$a_treatment, x.as.weights = TRUE, 
        estimand = "ATT")
      dx_m1$desc[[1]]["iter"] <- NA
      dx_m1$desc[[2]]["iter"] <- NA
      names(dx_m1$desc)[2] <- x$method
      model_m1 <- twang:::summary.ps(dx_m1)
    }

    ps_tables  <- list(model_a=model_a,model_m0=model_m0,model_m1=model_m1)

  # Get balance tables for NIE_1 and NIE_0
  # to check that weights for the counterfactual 
  # mediator distributions yeild distributions of 
  # mediators that match the target
  mediator_distribution_check <- twangMediation:::bal.table.mediation(x)[c("check_counterfactual_nie_1","check_counterfactual_nie_0")]

  print(list(estimates_table = estimates_table, ps_summary_tables = ps_tables, mediator_distribution = mediator_distribution_check))
}
