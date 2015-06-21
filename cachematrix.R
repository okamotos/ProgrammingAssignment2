# Implements a "cache matrix" which is a  matrix that caches its inverse to 
# save repeated computation.  The  inverse is onlycomputed when it is needed.
#
# A cache matrix is a list with the following elements:
#   set(x)  function for setting the cache matrix to an invertible matrix x. 
#           Using this to change the represented matrix clears the cached 
#           inverse so it will have to be recomputed next time it is requested.
#   get()   function returning the raw matrix represented by the cache matrix.
#   inverse(return.cached)  function getting the inverse matrix, either by 
#                           returning the cached value or calculating a new
#                           one.  By default returns a raw matrix, but will 
#                           return  a cache matrix if argument return.cache 
#                           is TRUE
#

# makeCacheMatrix(x) creates a cache matrix representing the invertible raw
# matrix x.  (No checking is done to ensure that x is invertible.)
#
# Arguments:
#   x   The invertible, raw matrix.
# Returns:
#   The cache matrix representing x
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    if (!identical(x, y)) {
      x <<- y
      inv <<- NULL
    }
    x
  }
  get <- function()  {
    x
  }
  inverse <- function(return.cached=FALSE, ...) {
    args <- list(...)
    if (is.null(inv)) {
      inv <<- solve(x, ...)
    } else {
      # message ('Getting cached inverse')
    }
    if (return.cached) {
      return(makeCacheMatrix(inv))
    } else {
      return(inv)
    }
  }

  list(set = set, get = get, inverse = inverse)
}


# Gets the inverse of a cache matrix. By default returns a raw matrix, but
# this can be changed with the return.cached argument.
#
# Arguments:
#   x               The cache matrix to find the inverse of.
#   return.cached   Flag to return a cache matrix intstead of a raw matrix.  
#                   Defaults to FALSE.
#   ...             Additional arguments to pass to the solver that computes 
#                   the inverse.
# Return
#   The raw or cache matrix with that is the inverse of 
#   the cache matrix x.
cacheSolve <- function(x, return.cached = FALSE, ...) {
  x$inverse(return.cached, ...)
}

