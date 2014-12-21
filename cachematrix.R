
## This function creates a special "matrix" object that can cache its inverse
## Returns list with 4 functions:
## set(y) - change matrix walue
## get() - get matrix value
## setsolve() and getsolve() - accessors for cached value. 
##                             Don't use them directly.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix
## Arguments:
## x - matrix, returned by makeCacheMatrix() function
## ... - additional arguments for solve() function

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cache data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
