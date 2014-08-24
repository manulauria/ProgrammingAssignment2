## The two functions below are to support the creation
## and use of an object which stores a matrix and its
## inverse.
## The goal is to save on time when repeated inverses
## are done on the same matrix.
## This is achieved by solving the inverse of the matrix
## ONLY the first time it is requested. The inverse so
## obtained is stored, and for subsequent inversions, the
## stored/cached value of the inverse is returned.
## At the cost of some extra space, we save on the time
## for doing an inverse.

## The function makeCacheMatrix does the following
## - creates an object with 
##   2 data values which store 
##     - the matrix - x
##     - its inverse - inv
##   4 functions to
##     - get and set the matrix
##     - get and set the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverted) inv <<- inverted
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve does the following
## - looks for the saved/cached inverse of the matrix
## - if there is no saved/cached value, it calls the
##   solve function to calculate the inverse.
##   Before returning this newly solved inverse, the
##   value of the inverse is stored in the cache for
##   future calls to this function

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}

