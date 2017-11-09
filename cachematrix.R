## These two functions are used to cache the inverse of a matrix
## Matrix inversion is usually a costly computation
## There may be some benefit to caching the inverse of a matrix rather than computing it repeatedly
## Created the following functions makeCacheMatrix and cacheSolve to compute the results
## Assume that the matrix supplied is always invertible

## This function build up a matrix which is able to cache its inverse

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function caculated the inverse of the marix created by the first function
## it first checks to see if the mean has already been calculated
## If so, it gets the mean from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
     message("getting cached data")
     return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
}


## This function caculated the inverse of the marix created by the first function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
     message("getting cached data")
     return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
}
