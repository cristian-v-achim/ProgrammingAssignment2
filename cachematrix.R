##Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly.The following functions can compute and cache the inverse of a matrix.
## It is assumed the inverse exists.

## The first function, "makeCacheMatrix", creates a special "matrix", which
## is a list containing a function to
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function, "cacheSolve", computes the inverse of the special "matrix" created by the
## "makeCacheMatrix" function. If the inverse has already been calculated and the 
## matrix has not changed, then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  m <- x$getInverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}
