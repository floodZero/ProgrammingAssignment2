## sample code for testing
##
## mt <- makeCacheMatrix(matrix(3:6, 2, 2))
## mt$get()
## cacheSolve(mt)
## cacheSolve(mt)
## mt$getinv()
## mt$setinv(NULL)
## mt$get() %*% cacheSolve(mt)

## This function will save matrix and inversion of the matrix for cache.
## And it returns setter and getter for user interface
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function call the inversion getter function of x.
## And if there are no inversion value, make and cache it.
## And finally returns inversion matrix of x.
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
