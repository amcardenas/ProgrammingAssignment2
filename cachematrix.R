## This program contains two functions, makeCacheMatrix and 
## cacheSolve, which cache the inverse of a matrix.

## This function, makeCacheMatrix, creates a special "matrix"
## object that can cache a matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}

## This function, cacheSolve, computes the inverse of the 
## special "matrix" returned by makeCacheMatrix from the 
## function above.  If the inverse has already been calculated, 
## (and the matrix has not changed), then the cacheSolve
## function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}