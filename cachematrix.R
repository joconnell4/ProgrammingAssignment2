## This script can be used to invert large matrix objects;
##It saves time and memory by cacheing the result after calculation
##If a cache exitst, it will use this value rather than recalculating
##the result.
##

## The first function creates a special matrix that can cache 
##its inversre

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

# ## This function will compute the inverse of a matrix; If the inverse 
# ##already was calculated and hasn't changed, then the answer isn't 
# ##recalculated but rather is retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
