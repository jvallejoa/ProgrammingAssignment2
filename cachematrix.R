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
