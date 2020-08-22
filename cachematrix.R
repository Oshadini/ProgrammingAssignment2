## Put comments here that give an overall description of what your
## functions do

## Whole programme consist of two main functions which are makeCacheMatrix and cacheSolve
## makeCacheMatrix is responsible of creating a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## initializing inverse as null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}  ## function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}  ## function to get the inverse of matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}


## cacheSolve is used to compute the inverse of the special "matrix" or else to retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {   ## checks whether inverse is null
    message("getting cached data")
    return(inv)  ## returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)  ## calculates inverse value
  x$setInverse(inv)
  inv
}
