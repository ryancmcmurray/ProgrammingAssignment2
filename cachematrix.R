## These functions will return the inverse of a matrix and cache
## the result, but will check the cached data first before performing
## the operation and return the cached matrix if available. This
## can be necessary due to the possibly high computation cost
## of inverting a large matrix; obtaining cached data is less 
## costly than computing the operation repeatedly.

## This function creates a "matrix" that can store its inverse in
## cache (the "matrix" is actually a list that 1: sets the value
## of the matrix 2: gets the value of the matrix 3: sets the
## inverse matrix value 4: gets the inverse matrix value).  

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


## This function will compute the inverse of the "matrix" created
## by makeCacheMatrix, but will return the cached
## inverse matrix data if available before attempting to make
## the computation. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()                   
  if(!is.null(inv)) { 
      message("getting cached data")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv        ## Return a matrix that is the inverse of 'x'
}
