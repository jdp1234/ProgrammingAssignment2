## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_m <<- inverse
  getinverse <- function() inverse_m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

  inverse_m <- x$getinverse()
  if (!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  info <- x$get()
  inverse_m <- solve(info, ...)
  x$setinverse(inverse_m)
  inverse_m
  
}

M1 <- matrix(c(4,2,4,7,20,19,13,14,2),3,3)
M1
M2 <- makeCacheMatrix(M1)
cacheSolve(M2) 


