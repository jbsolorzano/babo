## cache its inverse
## I set the value of the vector in set<-. 
## Do similar to get the value with get<-. 
##And repeat to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) 
    m<<-inverse
  getInverse<-function() m
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-inverse(data,...)
  x$setInverse(m)
  m
}
