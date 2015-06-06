
## Matrix inversion is costly, so it is handy to 
## avoid inversion if the inversion had already been 
## done and saved somewhere.  The two functions below 
## allow the user to do exactly that.


## makeCacheMatrix() creates a special matrix 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). If the inverse has already been calculated and the matrix has not changed, it’ll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}