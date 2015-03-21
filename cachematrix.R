## Code for the Programming Assignment 2: Lexical Scoping of the rpro-012 R Programming course
## Coursera link: https://class.coursera.org/rprog-012/

## These R functions cache the results of inverting a matrix, in order to save on potentially
## costly computations (avoids repeated computations).

## makeCacheMatrix creates a special "matrix" object that caches its inverse
## and creates a set of functions to set and retrieve its original data
## and inverted data.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" object from the above function.
## If the inverse was already calculated previously (and the matrix wasn't changed),
## the function instead retrieves the inverted data from the cache.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
