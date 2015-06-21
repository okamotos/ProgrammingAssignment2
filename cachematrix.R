# Implements a "cache matrix" which is a  matrix that
# caches its inverse to save repeated computation.  The
# inverse is only computed when it is needed.
#
# A cache matrix is a list with the following elements:
#    set(x)  function for setting the cache matrix to an
#            invertible matrix x. Using this to change
#            the represented matrix clears the cached 
#            inverse so it will have to be recomputed next
#            time it is requested.
#    get()   function returning the raw matrix 
#            represented by the cache matrix.
#    setinverse(y) sets the cached inverse matrix to be
#                  y. This shouldn't be called by users.
#

# makeCacheMatrix(x) creates a cache matrix representing
# the invertible raw matrix x.  (No checking is done 
# to ensure that x is invertible.)
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
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() {
    inv
  }
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


# Gets the inverse for a cache matrix.  First checks
# if the inverse has already been computed, and if so,
# returns the cached value.  If not, the inverse is
# computed, cached, and returned.
#
# Arguments:
#   x               The cache matrix to find the inverse of.
#   ...             Additional arguments to pass to the solver
#                   that computes the inverse.
#   return.cached   Flag to return a cache matrix intstead
#                   of a raw matrix.  Defaults to FALSE
# Return
#   The raw or cache matrix with that is the inverse of 
#   the cache matrix x.
cacheSolve <- function(x, ..., return.cached = FALSE) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message('getting cached inverse')
    return(inv)
  }
  mtx <- x$get()
  inv <- solve(mtx, ...)
  x$setinverse(inv)
  inv
}

