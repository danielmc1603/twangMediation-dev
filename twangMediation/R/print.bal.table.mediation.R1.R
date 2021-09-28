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

if(class(x) != "bal.table.mediation"){stop("x must be of class bal.table.mediation")}

for(i in 1:7){
   cat(paste0("\n$",names(x)[i]," \n"))
   print(x[[i]])
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
