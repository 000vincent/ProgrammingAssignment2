## function makeCacheMatrix creates a matrix that can store its inverse result for caching
## function cacheSolve returns the inverse of the matrix, from its cache if available

## create the matrix and return it
makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL

  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }

  get <- function() {
    x
  }

  setInverse <- function(inv) {
    inverse <<- inv
  }

  getInverse <- function() {
    inverse
  }
}

## return the inverse of the given matrix, if available from the cache
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    ## result is available in the cache, return it
    return(inverse)
  }

  matrix <- x$get()

  ## calculate the inverse of matrix and store it into the cache
  inverse <- solve(matrix)
  x$setInverse(inverse)

  inverse
}
