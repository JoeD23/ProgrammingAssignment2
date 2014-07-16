## Cache the inverse of a matrix to avoid repeatedly having 
## to compute the inverse.  This is done using 2 functions

## Creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x<<- y
                m<<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m<<- solve
        getinverse <- function() m
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## Computes the inverse of the special "matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}        
