## A pair of functions that cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## Setting the matrix 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Getting the matrix 
  get <- function() x
  
  ## Setting the inverse of the matrix 
  setInverse <- function(mean) m <<- mean
  
  ## Getting the inverse of the matrix
  getInverse <- function() m
  
  ## Return a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated (and the
## matrix has not changed), then `cacheSolve` should retrieve the inverse from
## the cache. The function returns a matrix that is the inverse of 'x'. 

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  ## Return the cached matrix if it's already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse
  m <- solve(data) 
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix 
  m
}