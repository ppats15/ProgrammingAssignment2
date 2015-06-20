## Following code optimizes costly computation of matrix inversion by caching 
## inverse matrix. First time, it computes matrix inverse and caches it. 
## In subsequent, inverse matrix computation cached value is returned for same
## inout matrix. 


## makeCacheMatrix(x) creates a "special" matrix object, with list of functions 
## that helps caching matrix values. It defines four helper functions for 
## setting and getting input matrix  and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize stored variable for inverse Matrix Value to NULL. 
  # This is used as a cache store. 
  invMatrix <- NULL
  
  # Set new stored value of input matrix, which will be used for inversion.
  set <- function(newInputMatrix) {
    x <<- newInputMatrix
    invMatrix <<- NULL
  }
  
  # Return stored value of input matrix
  get <- function() x
  
  # Store a value of matrix inverse (output) for caching.
  setInverse <- function(vm) invMatrix <<- vm 
  
  # Get Cached Value of inverse matrix (output)
  getInverse <- function() invMatrix
  
  #Store all 4 functions.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function below cacheSolve(x, ...) calculates inverse of matrix. If inverse of 
## matrix for input matrix is already computed, it will return value from cache. 
## If new input matrix is passed, then it will compute inverse of matrix and 
## store it in cache for future.
## This function assumes that input matrix can be inverted 

cacheSolve <- function(x, ...) {

  ## Following code tries to get matrix inverse from cache. If exists, it returns.
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  ## There is no cache value of matrix inverse. Following code calculates inverse
  ## of matrix and stores it in cache for future.
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  
  ## Return a matrix that is the inverse of 'x'
  im
}