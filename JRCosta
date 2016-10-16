## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setup <- function(y) {
          x <<- y
          inv <<- NULL
        }
        getup <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(setup = setup,
             getup = getup,
             setupInverse = setupInverse,
             getupInverse = getupInverse)
}


## This function should compute the inverse of the matrix created by makeCacheMatrix. 
## should retrive inverse if matrix doesn't change 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getupInverse()
        if (!is.null(inv)) { 
                message("getiing cached data")
                return (inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setupInverse(inv)
        inv
}
