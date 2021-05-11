print.bal.table.mediation <- function(object,...){

# Print the note
cat(attr(object,"note"))

nn <- names(object)
for(i in nn){
cat(paste0("\n$",i," \n"))
print(object[[i]])
}
}
