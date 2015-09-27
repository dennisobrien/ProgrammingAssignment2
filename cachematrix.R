## Cache the results of a matrix inversion.
## This wraps a matrix in a list with a few getter/setter methods.
## Example:
##      Object construction:
## > x <- makeCacheMatrix(matrix(rnorm(5*5), 5, 5))
##      Get the wrapped matrix:
## > x$get()
##      Get the inverse matrix:
## > y <- cacheSolve(x)
##      Reset the matrix and invalidate the cache.  This should be the only way to modify the contents of the matrix.
## > x$set(matrix(rnorm(5*5), 5, 5))


## Wrap the matrix in a list data structure with the following methods.
##     `get` - Get the matrix data.
##     `set` - Set the matrix data and invalidate the cache.
##     `set_inverse` - set the inverse matrix. To be called by `cacheSolve`
##     `get_inverse` - retrieve the cached inverse matrix or NULL if it has not been computed.  To be called by `cacheSolve`.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) m <<- inverse
    get_inverse <- function() m
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Solve the matrix inversion and cache the result in the data structure.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}
