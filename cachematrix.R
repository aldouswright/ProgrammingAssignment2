## Stores the inverse of a matrix to prevent unnecessary recomputation

## Creates a matrix representation with getter and setter functions for the data and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(mtx){
    x <<- mtx
    i <<- NULL
  }
  
  get <- function(){
    x
  }
  
  
  setInverse <- function(inv){
    i <<- inv
  }
  
  getInverse <- function(){
    solve(x)
  }
  
  list(set=set, get=get,
       setInverse=setInverse, getInverse=getInverse)
}


## Returns a cached version of the inverse if it exists; else computes and returns a new one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)){
          message("returning cached inverse")
          i
        }
        
        original <- x$get()
        inverse <- solve(original)
        x$setInverse(inverse)
        inverse
}