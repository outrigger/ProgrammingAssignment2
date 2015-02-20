## Two functions - makeCacheMatrix() and cacheSolve() - 
## that cache the inverse of a matrix

## 1. Function for creating a special matrix object that 
##    can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix
  i <- NULL
  
  ## Set the values of the matrix
  set <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  
  ## Get the values of the matrix
  get <- function() {
    x
  }
  
  ## Set the values of the inverse matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the values of the inverse matrix
  getInverse <- function() {
    i
  }
  
  ## Return the methods as a list
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## 2. Function for computing the inverse of the special
##    matrix returned by makeCacheMatrix above. If the 
##    inverse has already been calculated (and the matrix
##    has not changed), then the cachesolve should retrieve 
##    the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  ## Check if the inverse has already been calculated.
  ## If already calculated, get the inverse from the 
  ## cache and skip computation.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Else, get the matrix  
  data <- x$get()
  
  ## Calculate the inverse matrix
  i <- solve(data) %*% data
  
  ## Store the inverse matrix in the cache
  x$setInverse(i)
  
  ## Return the inverse matrix
  i
}