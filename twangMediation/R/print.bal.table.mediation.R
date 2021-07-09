#' Default print statement for `mediation` class
#'
#' @param x A `bal.table.mediation` object.
#' @param ... Additional arguments.
#' @examples
#' \dontrun{
#' print(bal.table.mediation(cig_med))
#' }
#' @return Default print statement
#' @seealso \code{\link{bal.table.mediation}}, \code{\link{wgtmed}}
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
