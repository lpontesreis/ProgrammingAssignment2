## Functions to calculate the inverse of a matrix
## by caching data in order to save computation time

## makeCacheMatrix: creates a list to include and retrieve the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function that uses cached data if inverse of a matrix already exists; otherwise, calculates inverse and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv    
}