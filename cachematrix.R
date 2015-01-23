## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix; 
## it specifies the function that is applied to the matrix-in this case "solve"-the inverse of a matrix
## it stops and display an error message if the matrix is not a square one
## it checks if the matrix is square or non-singular

makeCacheMatrix <- function(x = matrix()) {
  
  if (nrow(x)!=ncol(x)){
    stop (" matrix is not square, find another matrix")
  }
  
  if (!is.non.singular.matrix(x)){
    stop (" matrix is singular, find another matrix")
}
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function computes the inverse of the matrix created witht "makeCacheMatrix"
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {  
  
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


## testing the functions
## in order to test the function install "matrixcalc" package- tests if the matrix is non-singular
## install.packages("matrixcalc")
library(matrixcalc)
set.seed <- 20
d<-matrix(rnorm(2500),50,50)
e<- makeCacheMatrix(d)
f<-cacheSolve (e)