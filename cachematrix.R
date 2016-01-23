# These are two R functions that creates a "cached Matrix object" to get a matrix 
# and calculate its inverse and cache this result for future uses (it means that 
# the Matrix Inverse is calculated just once, if the Matrix don`t change).
#
# Steps to use these functions (just an example):
#    1- Creating an object "CacheMatrix":
#       > Matrix.Cached <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
#  
#    2- Calculating the inverse of the Matrix and caching the result:
#       > cacheSolve(Matrix.Cached)
#    
#    3- Setting new values to the cached matrix:
#       > Matrix.Cached$set(matrix(4:1, nrow=2, ncol=2)) 
#
#    4- Using the function get() to see the new Matrix cached:
#       > Matrix.Cached$get()  
#    
#    5- Calculating the new inverse of the Matrix (changed in step 3):
#       > cacheSolve(Matrix.Cached)
#    
#    6- Using the function getinv() to see the cached "Inverse Matrix":
#       > Matrix.Cached$getinv()  
#
#    7- Verifying if cacheSolve() returns the cached "Inverse Matrix":
#       > cacheSolve(Matrix.Cached)


# ---------------------------makeCacheMatrix()------------------------------------
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(X = matrix()) {
       cached.Matrix <- NULL # Initialize the cached "Inverse Matrix" to NULL
       set <- function (Y) {
              X <<- Y  # Set the Matrix to be inversed
              cached.Matrix <<- NULL # Reset the cached "Inverse Matrix" to NULL
       }
       get <- function() X  # Get the Matrix to be inversed
       setinv <- function(M.inv) cached.Matrix <<- M.inv # Set "Inverse Matrix"
       getinv <- function() cached.Matrix # Get the cached "Inverse Matrix"
       list(set=set, get=get, setinv=setinv, getinv=getinv) # Returns a list
}


# -------------------------------cacheSolve()-------------------------------------
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(X, ...) {
       cached.Matrix <- X$getinv() # Get the cached "Inverse Matrix"
       if (!is.null(cached.Matrix)){  # Verify if cached "Inverse Matrix" is !NULL  
              message("Getting cached Matrix")
              return (cached.Matrix) # Returns the cached "Inverse Matrix" 
       }
       cached.Matrix <- solve(X$get()) # Calculates the "Inverse Matrix" 
       X$setinv(cached.Matrix) # The inverse is stored into the cache
       cached.Matrix # Returns the "Inverse Matrix" calculated here
}
