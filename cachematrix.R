## With these two functions we can cache a matrix and its inverse into an environment different to the current
##This following function allow setting and getting the original matrix and its inverse into a list
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
  inv<<-NULL
  }
  get<-function(){x}
  setInve<-function(inve){inv<<-inve}
  getInve<-function(){inv}
  list(set=set,get=get,setInve=setInve,getInve=getInve)
}

## This function firstly verifies if the inverse of a matrix has already been found to give it, if not
##it allows found it so the cached it.
cacheSolve <- function(x, ...) {
  inv<-x$getInve()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  matriz<-x$get()
  inv<-solve(matriz,...)
  x$setInve(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
