## This source file contains two functions, makeCacheMatrix() and 
## cacheSolve(), that work together to cache the inverse to a matrix.
## This permits a matrix inverse to be computed just once; subsequent
## invocations will retrieve the inverse from a cache.

## Usage:
##  Let mat represent an invertible matrix.
##  Then invmat <- makeCacheMatrix(mat) creates a "matrix" - a list
##  containing a function to set and get the value of the matrix,
##  and set and get the value of the inverse.
##  The first call to cacheSolve(invmat) then uses solve() to
##  return the inverse. However, subsequent calls to cacheSolve(invmat)
##  retrieves the inverse from the cache.

##  makeCacheMatrix takes a matrix x, and returns a list of functions that:
##  1) set the value of the matrix
##  2) get the value of the matrix
##  3) set the value of the inverse
##  4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##  cacheSolve takes a list that was created by makeCacheMatrix(), and
##  either finds the inverse (using solve()) [and stores it in the cache]
##  or retrieves the already-cached inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
