## r-prog programming assignment 2
##
## This module defines functions for solving matrices
## optimized for repeated access via caching.
##
## Example usage:
## $ source("cachematrix.R")
## $ m <- matrix(c(1,2,3, 11), nrow = 2, ncol = 2)
## $ cm <- makeCacheMatrix(m)
## $ cm$getinverse() #returns NULL
## $ cacheSolve(cm) #returns inverse
## $ cm$getinverse() #returns cached inverse
##
## Notes:
## * Current implementation will cache results of first
## invocation of `cacheSolve`. If subsequent calls to
## `cacheSolve`, are made with different extra parameters,
## incorrect results could be returned.

## Creates a cache matrix, an object suitable
## for use with `cacheSolve`.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

## Finds the inverse of provided cache matrix. Will return
## cached results for the cache matrix if some are defined.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
