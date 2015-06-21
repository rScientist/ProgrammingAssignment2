# These functions assist in caching the inv of a matrix rther than
# computing it repeatedly, because it can take alot of processing power to compute.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# This returns the matrix inversion. IT makes sure the inversion has been
# computed.  And then saves the result into the cache. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("loading memory from cache...")
        return(inv)
    }
    src <- x$get()
    inv <- solve(src)
    x$setinv(inv)
    inv
}
