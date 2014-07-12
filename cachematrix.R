## In this assignment, we take advantage of the scoping rules of R
## language to cache potentially time consuming computations of 
## matrix inverse
##
## Example Usage:
## m <- makeCacheMatrix() # initalize an empty cache matrix
## data <-matrix(1:4,nrow=2,ncol=2) # this is the matrix data 
## m$set(data) # set the value of the matrix
## m$get() # access the value of the matrix
## cacheSolve(m) # solve for the inverse of the matrix
## m$getinverse() # get the cached value of the matrix

## Yuwei Cui, Jul 11, 2014


## This function creates a special "matrix" object that can cache 
## its inverse.
## It is really a list of functions to 
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL # cached value for x inverse
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the matrix inverse
  setinverse <- function(inverse) xinv <<- inverse
  
  ## get the value of the matrix inverse
  getinverse <- function() xinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## access the current cached value of x-inverse
  xinv <- x$getinverse()
  if(!is.null(xinv)) {
    ## if xinv is not null, return the cached value
    message("getting cached data")
    return(xinv)
  }
  ## we will have to calculate x inverse since the cached
  ## value is null
  
  ## access the matrix data
  data <- x$get()
  ## calculate the inverse of matrix
  xinv <- solve(data, ...)
  ## set the cached matrix inverse
  x$setinverse(xinv)
  xinv
}
