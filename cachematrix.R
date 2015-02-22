## This is a pair of functions to improve performance by not recalculating the inverse of 
## a matrix if it has not changed

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing functions that
##  1.  set the value of the matrix
##  2.  get the value of the matrix
##  3.  set the value of the inverse
##  4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "vector"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheInverse <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(i)
  i
}
