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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv = x$getminv()
  if (is.null(minv)){
    return(minv)
  }
  mat.data = x$get()
  minv = solve(mat.data, ...)
  x$setminv(minv)
  return(minv)
}