## These functions combine to cache the inverse of a matrix 
## given that the matrix is invertible

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  matrix(
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean),
    2,2)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x[2,1]
  m <- solve(data, ...)
  x[1,2](m)
  m
}
