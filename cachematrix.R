## Creates a R object to represent a cacheable matrix inverse value
## which wraps an (optional) matrix parameters.
## If no parameter is given, creates a empty matrix
makeCacheMatrix <- function(x = matrix(1)) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a matrix, or returns a previously calculated
## one. The given matrix should be created via the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
