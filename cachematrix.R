## A matrix wrapper which allows caching of the inverse of a matrix.
## 
## Example usage: 
## 
## > mymat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
## > myinv <- cacheSolve(mymat)
## > myinv
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Creates an object which holds a matrix together with its cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(arg) inv <<- arg
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of a matrix held in the cacheMatrix object.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
