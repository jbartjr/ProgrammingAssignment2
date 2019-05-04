## The 2 functions below, when used together, allows for the calculation of the inverse
## of a matrix, the storage of the result in cache or memory, and the subsequent retrieval
## of stored results when asked to calculate again instead of repeating the calculation.

## This function takes a matrix as an input, and returns a list of 4 functions that sets
## and obtains both the matrix and its inverse stored in the cache. To operate, the output
## must be assigned to a variable.
## Sample call:
##    myCacheMatrix <- makeCacheMatrix(myMatrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function takes as an input, the output of the makeCacheMatrix function. It either
## reads the cache and returns the inverse of the matrix argument to makeCacheMatrix, or
## in case it hasn't been calculated yet, it calculates and returns the matrix inverse.
## Sample call:
##    myCacheMatrix <- makeCacheMatrix(myMatrix)
##    cacheSolve(myCacheMatrix)

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("obtaining cached data...")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv          ## Return a matrix that is the inverse of 'x'
}
