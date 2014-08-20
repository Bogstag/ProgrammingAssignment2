## Matrix inversion is usually a costly computation and their may be some benefit to caching.
## Tested with:
## a <- makeCacheMatrix(matrix(c(2,9,3,6,4,7,7,3,5,3,2,10,7,8,9,6,6,6,10,6,2,9,2,2,4), nrow=5, ncol=5))

## The first function creates a special matrix, which is really a list containing a function to
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) { # sets a matrix to the special 'matrix' object
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x # function to retrieve the matrix from the special 'matrix' object
        # sets or gets the inverse of the matrix if cacheSolve has been called
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse() # tries to retrieve the inverse from makeCacheMatrix
        if(!is.null(m)) { # if inverse is present in makeCacheMatrix print message and
                message("getting cached data")
                return(m) # returns the inverse
        }
        data <- x$getMatrix() # assigns the matrix from makeCacheMatrix to data
        m <- solve(data, ...)
        x$setInverse(m)
        m # Return the inverse
}