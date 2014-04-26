## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly
## This R script contains two functions 1 that will operate over a cachable matrix by providing set,get functions and the 2nd will produce the inverse of the matrix in cache
## Author R. Brooks; 2014-04-26



## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## MUST have external matrix defined called the_matrix for this to work properly - tested using the_matrix <- matrix(c(1,2,3,4), 2, 2) and 
## foo <- makeCacheMatrix(the_matrix)

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
        set <- function(y) {
		the_matrix <<- y
	 	inverse <<- NULL
        }
        get <- function() the_matrix
        setInverse <- function(solve) inverse <<- solve 
        getInverse <- function() inverse 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function will create the inverse or retrieve the cached inverse of a matrix tested using cacheSolve(foo) the returned result from cache or created new result in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached inverse data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
       i
}
