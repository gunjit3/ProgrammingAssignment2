## The following function will cache the inverse of matrix instead of computing it repeatedly.
## As matrix inversion is computationally intensive, there might be benefit in caching the inverses.

## The following function creates a special matrix, which is acturally a list containing four functions
## 1. set : to set the value of matrix
## 2. get : to get the original matrix
## 3. setInverse : to set the value of inverse matrix
## 3. getInverse : to get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(y) {
    inverse <<- y
  }
  getInverse <- function() inverse
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following functions tried to get the inverse of a matrix from cache via getInverse function,
## if no data is found in the cache it gets the original matrix via get function,
## computes the inverse and sets the inverse using setInverse method and returns the inverse
## Solve funtion is used to compute inverse of the matrix. The code assumes that only square 
## invertible matrices will be passed.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse
  if(!is.null(inverse)) {
    message("Returning inverse from cache")
    return(inverse)
  }
  data <- x$get
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
