# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 
# This function iterates the calculation of inverse matrixes

makeCacheMatrix <- function(x = matrix()) {
   inv = NULL
   set = function(y) {
      x <<- y
      inv <<- NULL
   }
   get = function() x
   set.inv = function(inverse) inv <<- inverse
   get.inv = function() inv
   list(set=set, get=get, set.inv=set.inv, get.inv=get.inv)
}

# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the mean of the data and sets the value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inverse = x$get.inv()
    if (!is.null(inv)){
        message("getting cache")
        return(inv)
    }
    mat.data = x$get()
    inverse = solve(mat.data, ...)
    x$set.inv(inverse)
    return(inverse)
}
