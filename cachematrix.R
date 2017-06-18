## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. 
##For example, if x is a square invertible matrix, then solve(x) returns its inverse.

## Write a short comment describing this function

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

## This function computes the inverse of the specific “matrix” returned by makeCacheMatrix function above.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
          message("We get already cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
