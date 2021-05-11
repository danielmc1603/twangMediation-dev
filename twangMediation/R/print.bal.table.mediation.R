#' Default print statement for `mediation` class
#'
#' @param object A `bal.table.mediation` object
#' @param ... Additional arguments.
#'
#' @method print bal.table.mediation
#' @export
print.bal.table.mediation <- function(object,...){

if(class(object) != "bal.table.mediation"){stop("object must be of class bal.table.mediation")}
  
# Print the note
cat(attr(object,"note"))

nn <- names(object)
for(i in nn){
cat(paste0("\n$",i," \n"))
print(object[[i]])
}
}
