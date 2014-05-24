makeVector <- function(x = numeric()) {
  # Initializes mean to NULL
  m <- NULL

  # Replaces vector, reinitializes mean
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # Returns vector
  get <- function() x

  # Sets mean
  setmean <- function(mean) m <<- mean

  # Returns mean
  getmean <- function() m

  # Returns a list of four functions
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
  # If mean was not previously calculated getmean will return NULL
  m <- x$getmean()

  if (is.null(m)) {
    # Null => calculate and set mean
    m <- mean(x$get(), ...)
    x$setmean(m)
  }
  else {
    # Not null => previously calculated, simply return m
    message("Getting cached data")
  }
  m
}
