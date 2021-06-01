mymapply <- function(Func,...){
  result <- vector()
  max_length <- length(list(...)[[1]])
  
  for(i in 1:length(list(...)))
    max_length <- max(max_length,length(list(...)[[i]]))
 
  newdataframe <- data.frame(list(...)[[1]])
  for (i in 2:length(list(...)))
    newdataframe <- cbind(newdataframe,list(...)[[i]])
  for (j in 1: max_length)
    result <- c(result,Func(newdataframe[j,]))
  return(result)
}

a <- c(1:5) 
b <- c(10:14) 
d <- c(1:5)
mymapply(sum,a,b,d)
class(mymapply(sum,a,b,d))



