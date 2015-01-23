## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
  
  if (nrow(x)!=ncol(x)){
    stop ("the matrix is not square, find another matrix")
  }
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## this function computes the inverse of the matrix created witht "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

