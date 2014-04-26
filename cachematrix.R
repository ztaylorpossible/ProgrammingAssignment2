## This is a pair of functions to make repeated inversions of a
## matrix speedy using a cache when using identical matricies

## Creates a special form of a matrix that can use a cache for inversion

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <- NULL  # New matrix inputed so can't use cache
    }
    get <- function() x
    setinvert <- function(invert) i <<- invert
    getinvert <- function() i
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## Inverts the matrix, using a cached solution if possible

cacheSolve <- function(x, ...) {
    i <- x$getinvert()
    if(!is.null(i)) {
        return(i)
    }
    data <- x$get()
    i <- solve(x, ...)
    x$setinvert(i)
    i
}
