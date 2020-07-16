## The functions below serve to cache the inverse of a matrix 
## when it has alrady been calculated

## This function takes a matrix as input and outputs a list made up of 
## four functions

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(xi) {
    x <<- xi
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This following fuction calculates the inverse matrix from the
## object created by the above function, if the inverse matrix has already 
## been calculated,it gets the mean from the cache and skip the calculation.

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinverse()
  if (!is.null(inverse_matrix)) {
    message("getting chached data")
    return(inverse_matrix)
  }
  mat<- x$get()
  inverse <- solve(mat,...)
  x$setinverse(inverse)
  inverse
        
}
