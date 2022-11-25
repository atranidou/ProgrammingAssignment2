## This code provides two functions:
## i) makeCacheMatrix: A function that creates a special matrix object that is
##                     able to cache its inverse.
## ii) cacheSolve: A function that either caclulates the inverse of this special
##                 matrix object, or returns its cached inverse if already
##                 caclulated (by a previous call in the cacheSolve function).

## makeCacheMatrix creates a special matrix, that is able to cache its inverse
## in a environmet. It provides the following functions:
## set(x): Set/Update the matrix
## get(): Get the matrix
## setinverse(xinv): Set/Update the inverse matrix
## getinverse(): Get the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  set(x) # set the provided input matrix
  get <- function() x
  setinverse <- function(x_inverse = matrix()) xinv <<- x_inverse
  getinverse <- function() xinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is able to either compute the inverse of a `makeCacheMatrix`
## object, or if already calculated return the cached value to avoid further
## computations

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() # try to get the inverse
  if(!is.null(m)) { # if cached then return it
    message("getting cached data")
    return(m)
  }
  # else compute the inverse
  data <- x$get()
  xinv <- solve(data, ...)
  # cache the inverse
  x$setinverse(xinv)
  xinv
}

## Example usage function, on a 3x3 matrix
exampleUsage <- function() {
  m <- matrix(data = c(1, 2, 3, 4, 7, 6, 7, 8, 9), nrow = 3, ncol = 3)
  x <- makeCacheMatrix(m)
  # Perform and print the result of a matrix multiplication
  print(x$get() %*% cacheSolve(x)) # should print the identity matrix
  print(x$get() %*% cacheSolve(x)) # should print `getting cached data` and the identity matrix
}
