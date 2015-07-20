## Implementation of a simple caching mechanism for the calculation
## of the inverse of a matrix. As long as the matrix is not changed, the
## code uses the cached results to speed up the computation time, e.g. in loops

## The function makeCacheMatrix takes a matrix as argument (we assume it is
## invertible) and returns a list of methods that can be used to set and get
## the matrix, as well as setting and getting the inverse matrix.
##
## Example usage:
##
##    M <- makeCacheMatrix(matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2))
##
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The function cacheSolve takes a matrix as argument (we assume it is
## invertible) and returns its inverse matrix. If the inverse has already
## previuosly been calculated, it returns the cached inverse matrix.
##
## Example usage:
##
##    M <- makeCacheMatrix(matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2))
##    cacheSolve(M)
##
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

