## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setinv <- function(solve) i <<- solve
  getinv <- function () i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: computes inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$sesd

