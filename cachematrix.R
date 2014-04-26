## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly
## This R script contains two functions 1 that will operate over a cachable matrix by providing set,get functions and the 2nd will produce the inverse of the matrix in cache
## Author R. Brooks; 2014-04-26



## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
        set <- function(y) {
		x <<- y
	 	inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve 
        getinverse <- function() inverse 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function will create the inverse or retrieve the cached inverse of a matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
       inverse 
}
