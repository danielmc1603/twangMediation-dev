globalVariables("type2") # This gets rid of a warning related to variable "type2"
#' Compute the balance table for mediation object.
#'
#' @param x A `mediation` object 
#' @param digits Number of digits to round to. Dafault: 3
#' @param details logical. If `TRUE` covariate balance for the models
#'   used to create the inputs into the weights used in effect estimation
#'   is checked. If `FALSE` the additional balance is not checked.
#' @param plot logical. If `TRUE`, plots of the balance for all covariates
#'   are outputted for each type of effect (NIE0, NIE1, NDE0, NDE1, TE)
#'   for each stopping method. If `FALSE`, no plots are returned.
#' @param ... Additional arguments.
#' @return \item{res}{tables detailing covariate balance across
#'   exposure groups both before and after weighting}
#' @examples
#' data("tMdat")
#' 
#' ## tMdat is small simulated data set included in twangMedRiation for 
#' ## demonstrating the functions. See ?tMdat for details
#' 
#' fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
#'                       data = tMdat,
#'                       a_treatment = "A",
#'                       y_outcome = "Y",
#'                       total_effect_wts = tMdat$te.wgt,
#'                       method = "logistic"
#'                       )
#' 
#' bal.table.mediation(fit.es.max)
#' @seealso \code{\link{print.bal.table.mediation}}, \code{\link{wgtmed}}
#' @export
bal.table.mediation <- 
  function(x,digits=3, details=FALSE,plot=FALSE, ...) 
  {
    
    # we extract what we need from the mediation object
    data <- x$data
    model_a <- x$model_a
    model_m0 <- x$model_m0
    model_m1 <- x$model_m1
    stopping_methods <- x$stopping_methods
    method <- x$method
    w_00 <- attr(x, "w_00")
    w_10 <- attr(x, "w_10")
    w_01 <- attr(x, "w_01")
    w_11 <- attr(x, "w_11")
    sampw <- attr(x,"sampw")
    column_names <- colnames(data)
    m_and_x_names <- c(x$mediator_names,x$covariate_names)
    
    ## TE table
    balance_TE <- twang::bal.table(x$dx.wts$TE, digits=digits)
    balance_TE <- lapply(balance_TE, function(dd){dd$ks.pval <- NULL
    return(dd)})
    names(balance_TE)[2] <- x$method
    
    ## NIE1 table
    balance_NIE1 <- twang::bal.table(x$dx.wts$NIE1, digits=digits)
    balance_NIE1 <- lapply(balance_NIE1, function(dd){dd$ks.pval <- NULL
    return(dd)})
    balance_NIE1$unw <- NULL
    if(method == "ps"){names(balance_NIE1) <- stopping_methods}else{
      names(balance_NIE1) <- method}
    
    ## NDE0 table
    balance_NDE0 <- twang::bal.table(x$dx.wts$NDE0, digits=digits)
    balance_NDE0 <- lapply(balance_NDE0, function(dd){dd$ks.pval <- NULL
    return(dd)})
    balance_NDE0$unw <- NULL
    if(method == "ps"){names(balance_NDE0) <- stopping_methods}else{
      names(balance_NDE0) <- method}
    
    ## NIE0 table
    balance_NIE0 <- twang::bal.table(x$dx.wts$NIE0, digits=digits)
    balance_NIE0 <- lapply(balance_NIE0, function(dd){dd$ks.pval <- NULL
    return(dd)})
    balance_NIE0$unw <- NULL
    if(method == "ps"){names(balance_NIE0) <- stopping_methods}else{
      names(balance_NIE0) <- method}
    
    ## NDE1 table
    balance_NDE1 <- twang::bal.table(x$dx.wts$NDE1, digits=digits)
    balance_NDE1 <- lapply(balance_NDE1, function(dd){dd$ks.pval <- NULL
    return(dd)})
    balance_NDE1$unw <- NULL
    if(method == "ps"){names(balance_NDE1) <- stopping_methods}else{
      names(balance_NDE1) <- method}
    
    
    balance_res <- list(TE=balance_TE, NIE1=balance_NIE1, NDE0=balance_NDE0, NIE0=balance_NIE0, NDE1=balance_NDE1)
    
    # Compare the weighted distribution of M(1)=m weighted to have the distribution of M(0)=m on the entire pop'n 
    # Note: Using ATE here (instead of ATT)
    balance_nie_1 <- twang::bal.table(x$dx.wts$NIE1_m, digits=digits)
    balance_nie_1 <- do.call(rbind, balance_nie_1)
    balance_nie_1["model"] = "NIE_1"
    names(balance_nie_1) <- gsub("tx","cntfact",gsub("ct","target",names(balance_nie_1)))
    
    # Compare the weighted distribution of M(0)=m weighted to have the distribution of M(1)=m on the entire pop'n 
    balance_nie_0 <- twang::bal.table(x$dx.wts$NIE0_m, digits=digits)
    balance_nie_0 <- do.call(rbind, balance_nie_0)
    balance_nie_0["model"] = "NIE_0"
    names(balance_nie_0) <- gsub("tx","target",gsub("ct","cntfact",names(balance_nie_0)))
    balance_nie_0 <- balance_nie_0[,c(3,4,1,2,5:10)]
    balance_nie_0$std.eff.sz <- -1*balance_nie_0$std.eff.sz
    balance_nie_0$stat <- -1*balance_nie_0$stat
    
    # Remove ks.pvalue from tables 
    balance_nie_1 <- balance_nie_1[,-which(colnames(balance_nie_1)=="ks.pval")]
    balance_nie_0 <- balance_nie_0[,-which(colnames(balance_nie_0)=="ks.pval")]
    
    # Remove model from tables 
    balance_nie_1 <- balance_nie_1[,-which(colnames(balance_nie_1)=="model")]
    balance_nie_0 <- balance_nie_0[,-which(colnames(balance_nie_0)=="model")]
    
    balance_res[["check_counterfactual_nie_1"]] <- balance_nie_1 
    balance_res[["check_counterfactual_nie_0"]] <- balance_nie_0
    
    if(!is.null(sampw)) {
	attr(balance_res,"sampw") <- TRUE
    } else {
	attr(balance_res,"sampw") <- FALSE
    }
    attr(balance_res, "class") <- "bal.table.mediation"
    
    if(details){
      ##results if method == ps
      if(x$method=="ps") {
        
        # get the balance table for Model A
        balance_a <- do.call(rbind, bal.table.ps(model_a, digits = digits))
        balance_a['model'] <- 'Model A'
        
        # get the balance table for Model M0
        balance_m0 <- do.call(rbind, bal.table.ps(model_m0, digits = digits))
        balance_m0["model"] <- "Model M0"
        
        # get the balance table for Model M1
        balance_m1 <- do.call(rbind, bal.table.ps(model_m1, digits = digits))
        balance_m1["model"] <- "Model M1"
      }
      
      ##results if method = "logistic" or "crossval"
      if(x$method!="ps") {
        # get the balance table for Model A
        
        if(x$method=="logistic") {
          model_a_preds <- predict(x$model_a,type="response")
        }
        if(x$method=="crossval") {     
          best.iter <- gbm::gbm.perf(x$model_a, method="cv",plot.it=FALSE)
          model_a_preds <- predict(x$model_a, n.trees=best.iter, newdata=data, type="response")
        }
        wts_a <- ifelse(data[,x$a_treatment]==1,1/model_a_preds,1/(1-model_a_preds))
        
        tmp_a <- bal.table.ps(dx.wts.mediation(wts_a, data = data, 
                                               vars = x$covariate_names[!x$covariate_names %in% x$med_interactions], treat.var = x$a_treatment, x.as.weights = TRUE, 
                                               estimand = "ATE",sampw=sampw),digits=digits)
        names(tmp_a)[2] <- x$method
        balance_a <- do.call(rbind, tmp_a)
        balance_a['model'] <- 'Model A'
        
        # get the balance table for Model M0
        data$trt0 <- 1-data[,x$a_treatment]
        if(x$method=="logistic") {
          model_m_preds <- predict(model_m0,type="link")
        }
        else {
          best.iter <- gbm::gbm.perf(model_m0, method="cv",plot.it=FALSE)
          model_m_preds <- predict(model_m0, n.trees=best.iter, newdata=data, type="link")
        }
        wts_m0 <- ifelse(data[,x$a_treatment]==0,1,1/exp(model_m_preds))
        tmp_m0 <- dx.wts.mediation(wts_m0, data = data, 
                                   vars = m_and_x_names, treat.var = "trt0", x.as.weights = TRUE, 
                                   estimand = "ATT",sampw=sampw)
        tmp_m0$desc <- lapply(tmp_m0$desc, swapTxCtrl)
        tmp_m0 <- bal.table.ps(tmp_m0, digits=digits)
        names(tmp_m0)[2] <- x$method
        balance_m0 <- do.call(rbind, tmp_m0)
        balance_m0["model"] <- "Model M0"
        
        # get the balance table for Model M1 
        wts_m1 <- ifelse(data[,x$a_treatment]==0,exp(model_m_preds),1)
        tmp_m1 <- bal.table.ps(dx.wts.mediation(wts_m1, data = data, 
                                                vars = m_and_x_names, treat.var = x$a_treatment, x.as.weights = TRUE, 
                                                estimand = "ATT",sampw=sampw),digits=digits)
        names(tmp_m1)[2] <- x$method
        balance_m1<- do.call(rbind, tmp_m1)
        balance_m1["model"] <- "Model M1"
      } ## ends if logisitic or crossval
      
      # Remove ks.pvalue from all tables 
      balance_a <- balance_a[,-which(colnames(balance_a)=="ks.pval")]
      balance_m0 <- balance_m0[,-which(colnames(balance_m0)=="ks.pval")]
      balance_m1 <- balance_m1[,-which(colnames(balance_m1)=="ks.pval")]
      
      # Remove model from all tables 
      balance_a <- balance_a[,-which(colnames(balance_a)=="model")]
      balance_m0 <- balance_m0[,-which(colnames(balance_m0)=="model")]
      balance_m1 <- balance_m1[,-which(colnames(balance_m1)=="model")]
      
      details  <- list(balance_a = balance_a, balance_m0 = balance_m0,balance_m1 = balance_m1) 
      
      attr(details, "note") <- "**********************************************************\nNotes: \nA. Model A estimates the probability of exposure given \nthe covariates specified in wgtmed. The results are used \nby wgtmed to estimate E[Y(1,M(0))] and E[Y(0,M(1))]. \nThey are not used to estimate the total effect. \nB. Model M0 is used for NDE_0 and NIE_1 effects. \nct.sd is used for the denominator of std.eff.sz. \nC. Model M1 is used for NDE_1 and NIE_0 effects.\ntx.sd is used for the denominator of std.eff.sz. \nSee the bal.table.mediation help file for more information. \n**********************************************************\n"
      if(!is.null(sampw)) {

        methodnms <- names(balance_res$NIE1)
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

 	attr(details, "note") <- paste0("**********************************************************\nNotes: \nA. Model A estimates the probability of exposure given \nthe covariates specified in wgtmed. The results are used \nby wgtmed to estimate E[Y(1,M(0))] and E[Y(0,M(1))]. \nThey are not used to estimate the total effect. \nB. Model M0 is used for NDE_0 and NIE_1 effects. \nct.sd is used for the denominator of std.eff.sz. \nC. Model M1 is used for NDE_1 and NIE_0 effects.\ntx.sd is used for the denominator of std.eff.sz. \nD. \"unw\" reflects weighting with sampling weights \nonly. ",methods," weighting by both \nthe sampling weights and total-effect/cross-world weights.\nSee the bal.table.mediation help file for more information. \n**********************************************************\n")
      }     

      balance_res[["details"]] <- details
      
    } ## Ends if details

     # Return covariate balance plots if plot=TRUE
     if(plot) {

       Allbal <- data.frame()
       for(i in 1:5) {
	  for(j in 1:length(balance_res[[i]])) {
            Allbal <- rbind(Allbal,cbind.data.frame(Effect=names(balance_res)[i],type=names(balance_res[[i]])[j],covariate=rownames(balance_res[[i]][[j]]),balance_res[[i]][[j]]))
          }
       }

       cov_plots <- vector("list",length(stopping_methods))
       for(k in 1:length(stopping_methods)) {
         allbal <- Allbal[Allbal$type %in% c("unw","ps",stopping_methods[k]),]
         allbal$covariate <- factor(allbal$covariate,levels=rownames(balance_res$NDE0[[1]]))
         allbal$Effect <- factor(allbal$Effect,levels=c("NDE0","NDE1","TE","NIE0","NIE1"))
         allbal$type2 <- ifelse(allbal$type=="unw","unweighted","weighted")  

         cov_plots[[k]] <- lattice::dotplot(covariate~std.eff.sz|Effect,data=allbal,
                             groups=type2,
                             panel = function(...) {
                               panel.xyplot(...)
                               panel.abline(v = 0)
                             },
                             par.settings=list(superpose.symbol=list(
                               pch=c(25,24),
                               col=c("red","black"),
                               fill=c("red","transparent"),
                               cex=c(.6,.6))),
                             auto.key=list(columns=2),
                             main=paste0("Balance for Covariates for Each Effect\n",stopping_methods[k]),
                             xlab="Standardized Effect Size",
                             cex.main=.75,
                             cex.lab=.1,
                             layout=c(3,2)
                            )
       }
 
       if(length(cov_plots)==1) {
         plot(cov_plots[[1]]) 
       } else { 
         oldpar <- par(no.readonly = TRUE)   
         on.exit(suppressWarnings(par(oldpar)), add=TRUE)
         cc <- par()$ask
         for(i in 1:length(cov_plots)) {
           plot(cov_plots[[i]])
           par(ask=TRUE)
         }
         par(ask=cc)
      }


     } #end if plot
     
    
    return(balance_res)
    
  } 
