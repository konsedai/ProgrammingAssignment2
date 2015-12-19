## makeCacheMatrix function will create an object, wich is able to store a
## matrix and it's inverse for future use. cacheSolve function will operate on
## an object returned from makeCacheMatrix and will return an inverse of given
## matrix, if available the value will be returned from the cache.

## This function will return a list of functions, which will store the matrix
## and it's inverse in the local scope.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes makeCacheMatrix list as an argument and either computes
## inverse of a given matrix, stores it in the cache and then returns it or
## just returns a precomputed value from the cache if it is present. 

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
