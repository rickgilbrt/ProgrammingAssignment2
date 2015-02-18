## This pair of functions handles caching and retrieving of the inverse of a given matrix
## in order to reduce computing time that would be incurred by repeat calls to compute
## the inverse.  The inverse is not computed until the first call of cacheSolve().
## Susequent calls to cacheSolve for a given matrix simply retrieve the inverse from the cache.

## makeCacheMatrix - creates a special "matrix" that is really a list containing a function to
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        
        }
        get <- function() { x }
        setInv <- function(inv) {m <<- inv}
        getInv <- function() m
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}        
## cacheSolve(x,...) computes the inverse of the special matrix created by
## the function above.  First, however, it checks to see if the inverse has
## already been computed and cached.  If so, it gets the inverse from the cache
## and skips the computation.  If not, it computes the inverse of the matrix, 
## stores that value of the cache via the setInv function, and returns the value
## of the inverse.

cacheSolve <- function(x, ...) {
        ## Check for a cached value of the inverse...
        m <- x$getInv()
        
        if(!is.null(m)) {
                message ("Your lucky day! Getting cached inverse")
                return(m)
        }
        message("No cached value. Working ...")
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}
