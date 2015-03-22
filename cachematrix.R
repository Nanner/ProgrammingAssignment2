## Code for the Programming Assignment 2: Lexical Scoping of the rpro-012 R Programming course
## Coursera link: https://class.coursera.org/rprog-012/
## By: Diogo Santos - 21/03/2015


#### Thank you for taking the time to review my code! ####


## These R functions cache the results of inverting a matrix, in order to save on potentially
## costly computations (avoids repeated computations).
## Here are some useful links for this assignment:
## Test data - https://class.coursera.org/rprog-012/forum/thread?thread_id=189
## Assignment tips, mostly to do with scoping - https://class.coursera.org/rprog-012/forum/thread?thread_id=229

## makeCacheMatrix creates a special "matrix" object that caches its inverse
## and creates a list of functions to set and retrieve its original and
## inverted data.
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the cache with a NULL value
        i <- NULL
        
        ## The set function "sets" the matrix to one given as a parameter
        ## The cache is also initialized as NULL. This method is particularly
        ## useful for cases when we run makeCacheMatrix without parameters.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## The get function simply returns the matrix data (not inverted)
        get <- function() x
        
        ## The setinverse function saves the inverse matrix data passed
        ## as a parameter in the cache
        setinverse <- function(inverse) i <<- inverse
        
        ## The getinverse returns the cached inverse matrix data (or null
        ## if nothing is cached)
        getinverse <- function() i
        
        ## Create a list with the previously created functions, and return it
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" object from the above function.
## If the inverse was already calculated previously (and the matrix wasn't changed),
## the function instead retrieves the inverted data from the cache.
cacheSolve <- function(x, ...) {
        ## First, try to see if there's already cached inverse matrix data
        i <- x$getinverse()
        if(!is.null(i)) {
                ## Return the cached data, if it exists
                message("getting cached data")
                return(i)
        }
        
        ## If no cached data exists, get the matrix data and compute its inverse
        data <- x$get()
        i <- solve(data, ...)
        ## Save the computed inverse data to the cache and return it
        x$setinverse(i)
        i
}
