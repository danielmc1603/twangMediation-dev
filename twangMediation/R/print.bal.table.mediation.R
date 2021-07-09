#' Default print statement for `mediation` class
#'
#' @param x A `bal.table.mediation` object.
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
#' # Compute the balance table
#' balance_table<- bal.table.mediation(cig_med)
#' # Now print
#' print(balance_table)
#' }
#' @return Default print statement
#' @method print bal.table.mediation
#' @export
print.bal.table.mediation <- function(x,...){

if(class(x) != "bal.table.mediation"){stop("x must be of class bal.table.mediation")}
  
# Print the note
cat(attr(x,"note"))

nn <- names(x)
for(i in nn){
cat(paste0("\n$",i," \n"))
print(x[[i]])
}
}
