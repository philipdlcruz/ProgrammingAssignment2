## These are functions that cache the inverse of a matrix. And matrices that
## are supplied as input should always be always invertible.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setmatinv <- function(matinv) mi <<- matinv
        getmatinv <- function() mi
        list(set = set, get = get, setmatinv = setmatinv, getmatinv = getmatinv)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        mi <- x$getmatinv()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setmatinv(mi)
        mi
}
