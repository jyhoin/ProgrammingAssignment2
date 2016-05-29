## Assignment: Caching the Inverse of a Matrix
## Functions that cache the inverse of a matrix.

## Store special matrix object

makeCacheMatrix <- function(x = matrix()) {
		 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## function computes the inverse of the special matrix

cacheSolve <- function(x, ...) {
m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'