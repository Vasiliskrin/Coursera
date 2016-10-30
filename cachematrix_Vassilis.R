## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.


makeCacheMatrix <- function(x = matrix()) {
  minv = NULL
  set = function(y) { 
    x<<- y
    minv <<- NULL
  }
  get = function() x
  setminv = function(inverse) minv <<- inverse
  getminv = function() minv
  list(set = set, get=get, setminv=setminv, getminv=getminv)
}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv = x$getminv()
  if (is.null(minv)){
    return(minv)
  }
  # otherwise, calculates the inverse 
  mat.data = x$get()
  minv = solve(mat.data, ...)
  # sets the value of the inverse in the cache via the setminv function.
  
  x$setminv(minv)
  return(minv)
}