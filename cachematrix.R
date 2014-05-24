## Put comments here that give an overall description of what your functions do
# Two functions are provided, makeCacheMatrix and cacheSolve.
# 1. makeCacheMatrix stores the passed matrix (assumed to be square invertible)
#    and provides functions to replace (set) and return (get) the matrix, and
#    set (setinverse) and return (getinverse) the inverse of the matrix. Note
#    that the inverse itself is set/get from the cacheSolve function and not
#    directly (otherwise you could simply set random values there!).
# 2. cacheSolve takes an object returned by makeCacheMatrix. It queries the
#    object for its inverse. If the inverse has not been previously computed on
#    the same matrix (by cacheSolve itself), it computes it and caches it onto
#    the object. It then simply returns the (cached or computed+cached) inverse.

## Write a short comment describing this function
# Function that returns object that helps to cache the inverse of a matrix. See
# top for detail
makeCacheMatrix <- function(x = matrix()) {
  # Initializes inverse to NULL
  inv <- NULL

  # Replaces matrix, reinitializes inverse
  set <- function(y) {
    # If the matrix is the same, let's not do anything
    if (dim(x) == dim(y) && all(x == y)) {
      message("Same matrix passed, doing nothing")
    }
    else {
      message("Replacing matrix")
      x <<- y
      inv <<- NULL
    }
  }

  # Returns matrix
  get <- function() x

  # Sets inverse
  setinverse <- function(inverse) inv <<- inverse

  # Returns inverse
  getinverse <- function() inv

  # Returns a list of four functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
# Function that actually uses the object returned by the above function to cache
# a matrix inverse. See top for detail.
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  # If inverse was not previously calculated getinverse will return NULL
  inv <- x$getinverse()

  if (is.null(inv)) {
    # Null => calculate and set inverse
    inv <- solve(x$get())
    x$setinverse(inv)
  }
  else {
    # Not null => previously calculated, simply return inv
    message("Getting cached data")
  }
  inv
}
