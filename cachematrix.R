## Caching the Inverse of a Matrix
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It contains a list of functions to manipulate the matrix and the inverse of the matrix.
## set() - set the value of the matrix
## get() - get the value of the vector
## setinv() - set the value of the inverse matrix
## getinv() - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinv <- function(imatrix) inv <<- imatrix
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix.
## If the inverse has already been calculated then it should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting inverse of matrix data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
