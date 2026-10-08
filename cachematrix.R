## These two functions work together to cache the inverse of a matrix.
## Computing a matrix inverse can be slow, so once it has been calculated
## it is stored and reused instead of being recomputed.

## makeCacheMatrix creates a special "matrix" object: a list of functions
## that set/get the matrix and set/get its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL    # matrix changed, so clear the old inverse
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the special "matrix" made by
## makeCacheMatrix. If the inverse is already cached, it returns that;
## otherwise it computes it with solve(), caches it, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
