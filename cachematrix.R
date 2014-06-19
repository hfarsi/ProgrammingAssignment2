## To minimize execution of repetitive cpu-intensive operations
## on the same (large) dataset such as Compution of inverse of 
## a large matrix, R provides the mechanism to cache the result 
## of the operation in memory for fast retrivial and reuse.  
## --------------------------------------------------------------
## makeCacheMatrix:
##   
## This function provides the interface to function (cacheSolve) 
## that computes the inverse of the mattrix given in the argument. 
## It defines the following functions and initiates the cache for 
## storing the matrix and its inverse.
##  setmatrix -   called to perfrom the inverse compution for a matrix 
##                with different value or diminsions. It inititates the 
##                cache for the new matrix and its inverse.
##  getmatrix -   returns the matrix.
##  setinverse -  stores the invered matrix in the cache
##  getinverse -  fectes the invered matrix from the cache. 
##                NULL indicated the inverse hasn't been stored in 
##                the cache.
##  Returned value: the list of defined functions. 
##  

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

##
## This function perfroms the compution of inverse matrix.
##  Input: list of functions from makeCacheMatrix:
##          (setmatrix, getmatrix, setinverse, getinverse)
##  Logic: if inverse is already in cache (m) return the inverse
##          matrix, o.w. get the matrix, compute the inverse, and 
##          store the inverse in cache (through setinverse function)
##

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}