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

  # Get summaries of ps xs
    if(x$method=="ps") {
      model_a  <- summary(x$model_a)
      model_m0 <- summary(x$model_m0)
      model_m1 <- summary(x$model_m1)
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
      dx_a <- dx.wts.mediation(wts_a, data = data, 
          vars = x$covariate_names, treat.var = x$a_treatment, x.as.weights = TRUE, 
          estimand = "ATE")      
      dx_a$desc[[1]]["iter"] <- NA
      dx_a$desc[[2]]["iter"] <- NA
      names(dx_a$desc)[2] <- x$method
      attr(dx_a, "class") <- "ps" 
      model_a <- summary(dx_a)

      data$trt0 <- 1-data[,x$a_treatment]
      if(x$method=="logistic") {
        model_m_preds <- predict(x$model_m0,type="link")
      }      
        else {
        best.iter <- gbm::gbm.perf(x$model_m0, method="cv",plot.it=FALSE)
        model_m_preds <- predict(x$model_m0, n.trees=best.iter, newdata=data, type="link")
      }
      wts_m0 <- ifelse(data[,x$a_treatment]==0,1,1/exp(model_m_preds))
      dx_m0 <- dx.wts.mediation(wts_m0, data = data, 
          vars = c(x$mediator_names,x$covariate_names), treat.var = "trt0", x.as.weights = TRUE, 
          estimand = "ATT")
      dx_m0$desc[[1]]["iter"] <- NA
      dx_m0$desc[[2]]["iter"] <- NA
      names(dx_m0$desc)[2] <- x$method
      attr(dx_m0, "class") <- "ps" 
      dx_m0$desc$unw <- swapTxCtrl(dx_m0$desc$unw)
      dx_m0$desc[[x$method]] <- swapTxCtrl(dx_m0$desc[[x$method]])
      model_m0 <- summary(dx_m0)

      wts_m1 <- ifelse(data[,x$a_treatment]==0,exp(model_m_preds),1)
      dx_m1 <- dx.wts.mediation(wts_m1, data = data, 
        vars =  c(x$mediator_names,x$covariate_names), treat.var = x$a_treatment, x.as.weights = TRUE, 
        estimand = "ATT")
      dx_m1$desc[[1]]["iter"] <- NA
      dx_m1$desc[[2]]["iter"] <- NA
      names(dx_m1$desc)[2] <- x$method
      attr(dx_m1, "class") <- "ps" 
      model_m1 <- summary(dx_m1)
    }

    ps_tables  <- list(model_a=model_a,model_m0=model_m0,model_m1=model_m1)

  # Get balance tables for NIE_1 and NIE_0
  # to check that weights for the counterfactual 
  # mediator distributions yeild distributions of 
  # mediators that match the target
  mediator_distribution_check <- bal.table.mediation(x)[c("check_counterfactual_nie_1","check_counterfactual_nie_0")]

  print(list(estimates_table = estimates_table, ps_summary_tables = ps_tables, mediator_distribution = mediator_distribution_check))
}
