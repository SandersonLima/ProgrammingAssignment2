## This script computes the matrix inversion using a "caching approach", where 
## the inverse matrix is cached rather than computing it repeatedly. In order to
## do this, we use a pair of functions: first one to create the "cache matrix" 
## (function makeCacheMatrix) and a second one (function cacheSolve)to computes 
## the inverse of the special "matrix" returned by "makeCacheMatrix".
##
## Example:
##    Matrix <- matrix(1:4, nrow=2, ncol=2)
##    cacheMatrix <- makeCacheMatrix(Matrix)
##    cacheSolve(cacheMatrix)
##
##    cacheMatrix$set(Matrix)      # Change the matrix being cached.
##    Matrix <- cacheMatrix$get()  # Returns the matrix being cached.


## This function creates a special "matrix" object ("x") that can cache its 
## inverse (with this approach, we need calculates the inverse only once).
makeCacheMatrix <- function(x = matrix()) {
       cachedInverse <- NULL
       set <- function(y) {
              x <<- y
              cachedInverse <<- NULL
       }
       get <- function() x
       setInverse <- function(inverse) cachedInverse <<- inverse
       getInverse <- function() cachedInverse
       list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## "makeCacheMatrix" function. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       invFunc <- x$getInverse()
       if(!is.null(invFunc)) {
              message("getting cached data")
              return(invFunc)
       }
       data <- x$get()
       invFunc <- solve(data, ...)
       x$setInverse(invFunc)
       invFunc
}
