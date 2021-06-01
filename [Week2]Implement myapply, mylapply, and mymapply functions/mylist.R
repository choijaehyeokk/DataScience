mylapply <- function(X, Func, ...){
  result <- list()
  if(is.data.frame(X) | is.matrix(X)){
    for(i in 1:ncol(X)){
      for(j in 1:nrow(X)){
        result <- append(result,Func(X[j,i],...))
      }
    }
  }#col=ї­, row=За
  else if(is.vector(X) | is.list(X)){
    for(i in X)
      result <- append(result,Func(i))
  }
  return(result)
}

m1 <- matrix(1:24, nrow = 3, ncol = 8)
mylapply(m1,sum)
class(mylapply(m1,sum))
m2 <- c(1,2,3,4,5,6,10)
mylapply(m2,sum)
class(mylapply(m2,sum))




