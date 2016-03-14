
## makeCacheMatrix takes an invertible matrix and returns a list
## with the following functions:
##
## getInverse: This function can be used to retrieve the currently cached value of the given matrix's inverse
## setInverse: This function can be used to set the inverse of the given matrix
## get: This function can be used to retrieve the matrix currently stored.
## set: This function can be used to set a new invertible matrix.  Note that this will delete any previously cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  
  getInverse <- function() inverse
  setInverse <- function(matrixInverse) inverse <<- matrixInverse
  get <- function() x
  
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }

  list(
    set = set, 
    get = get, 
    setInverse = setInverse, 
    getInverse = getInverse)
}


## cacheSolve can be used to invert a matrix using the result of the makeCacheMatrix function.
## Once the inverse of the matrix has been calculated using cacheSolve, future calls to
## cacheSolve will result in a cached copy of the inverted matrix being returned.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message ("Getting cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
