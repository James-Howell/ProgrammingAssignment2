## makeCacheMatrix and cacheSolve are a pair of functions that take
## a square invertible matrix and calculate the inverse. The inverse
## is stored and retrieved when necessary rather than recalculating
## the inverse.

## makeCacheMatrix creates a list of 4 functions that can be used
## to store and retrieve the value of a matrix, and store and
## retrieve the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Create a set function that immediately stores the matrix passed to
  # makeCacheMatrix, and sets the variable that will be used to store
  # its inverse to null.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Create a function that returns the matrix value
  get <- function() x
  # Create a function that stores the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  # Create a function that retrieves the inverse of the matrix
  getinverse <- function() m
  ## Return a list of the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the vector supplied to
## makeCacheMatrix and stores the value. If the inverse has 
## already been calculated, cacheSolve retrieves that value
## instead of calculating it again.

cacheSolve <- function(x, ...) {
  # Retrieve the inverse of the matrix using the getinverse
  # function of makeCacheMatrix
  m <- x$getinverse()
  # If the inverse has already been calculated, return that value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If the inverse has not been calculated, get the matrix and
  # store the inverse using the makeCacheMatrix functions
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  # Return the inverse
  m
}
