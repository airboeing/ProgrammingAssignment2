# Matrix inversion can be potentially time-consuming. The
# following two functions are used to cache the inverse of 
# a matrix rather than compute it repeatedly

## Usage:
## > a <- makeCacheMatrix( matrix(c(1,2,4,5), nrow = 2, ncol = 2) );
## > a$get()
##       [,1]  [,2]
## [1,]     1     4
## [2,]     2     5

## > cacheSolve(a)
##            [,1]       [,2]
## [1,] -1.6666667  1.3333333
## [2,]  0.6666667 -0.3333333

## Retrieving from the cache in the second run
## > cacheSolve(a)
## getting cached inverse data.
##            [,1]       [,2]
## [1,] -1.6666667  1.3333333
## [2,]  0.6666667 -0.3333333


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

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


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

