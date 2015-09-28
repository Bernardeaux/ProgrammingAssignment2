################################################################################
## R SCRIPT: R version 3.1.2 (2014-10-31)
##
## The pair of functions below, makeCacheMatrix and cacheSolve, cache the
## inverse of a matrix. The supplied matrix is assumed to be invertible.

## USAGE (given an invertible matrix 'm'):
## cachedmatrix <- makeCacheMatrix(m)  # creates the cache
## cacheSolve(cachedmatrix)            # prints the inverse
################################################################################

################################################################################
## makeCacheMatrix()
## takes an invertible matrix as its argument and returns
## a list of defined functions:
## set(): a variable is set up in another envrionment to store the inverse of the
## supplied matrix.
## get(): returns the matrix.
## setinverse(): calls the function solve() on the matrix.
## getinverse(): returns the inverse matrix.
################################################################################

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

################################################################################
## cacheSolve()
## takes the ouput of makeCacheMatrix() as its argument and returns the inverse
## of the matrix. If the call to getinverse() returns NULL, the inverse is
## stored in the cache with setinverse() before being returned. Otherwise, the
## cached value is returned.
################################################################################

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
