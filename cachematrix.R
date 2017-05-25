## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 

# set    : set the matrix
# get    : get the matrix
# setinv : set the inverse of matrix
# getinv : get the inverse of matrix

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
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## cacheSolve: computes inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed, cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv (i)
  i
}  

#Test
matrix <- matrix(c(2,5,2,3,6,4,0,0,1), nrow=3, ncol=3)
matrix2 <- makeCacheMatrix(matrix)
cacheSolve(matrix2) 
## inverse
          [,1]       [,2] [,3]
[1,] -2.000000  1.0000000    0
[2,]  1.666667 -0.6666667    0
[3,] -2.666667  0.6666667    1
cacheSolve(matrix2) 
## inverse from cache
getting cached data             
          [,1]       [,2] [,3]
[1,] -2.000000  1.0000000    0
[2,]  1.666667 -0.6666667    0
[3,] -2.666667  0.6666667    1
