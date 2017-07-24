## Both functions enables to cache a matrix and its inverse,
## so that we can look it up when we need it, instead of 
## computing it again.

## The function makeCacheMatrix creates a "special" object that 
## stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The cacheSolve function calculates the inverse of the special 
## "matrix" created above. If this inverse has already been
## calculated, the function displays the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
