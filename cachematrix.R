## Since matrix inverse is costly operation, the below two functions makes a way through it, firstly
## making a matrix (function makeCacheMatix) secondly compute and cache the inverse (function cacheSolve) so 
## if the inverse is already computed,
## it skips the computation therby taking cached result otherwise it computes the 
##inverse and cache the inverse computed later on.

## Function for making a Matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) { 
    x <<- y
    inverse <<- NULL
  }
  get <- function() x    # Returns the Matrix
  calcInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       calcInverse = calcInverse,
       getInverse = getInverse)
}


## Function for computing and caching the inverse

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)){
          message("getting cached Result")
          return(inverse)
        }
        data <- x$get()
        inverse <- solve(x, ...)
        x$calcInverse(inverse)
        inverse
}



