## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix "x" that can cache its inverse.
## It returns a list of four functions.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse<-function(solve) m<<- solve
        getinverse<-function() m
        list(set=set, get=get,
             setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## The function calculates the inverse of the matrix. 
## If the inverse has been calculated, the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
