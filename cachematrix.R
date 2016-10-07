## Week3 Assignment 2.
## This script file contains the following functions
## 1. makeCacheMatrix function
## 2. cacheSolve function
##
## General Description: 
## makeCacheMatrix function takes the value from a matrix being assign 
## and inverse the matrix and stores in cache cacheSolve function gets 
## the value from cache
##
## Ussage Example:
## In command line prompt do the following by declaring a variable
## a <- makeCacheMatrix(matrix(c(2,0,0,2),2,2)). (i.e "a": variable) 
## make sure the matrix row and column are equal rows and column (i.e. 2rows by 2 cols)
## then use the cacheSolve function like example below
## cacheSolve(a)


## makeCacheMatrix Function
## This function receives matrix and perfrom inverse
makeCacheMatrix <- function(x = matrix()) {   
  # m set as NULL
  m <- NULL
    
  # resets "m" when matrix change
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
    get <- function() x
    
  ## the following performs inverse matrix  
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  
}


## cacheSolve Function:
## This function inverse the matrix from makeCacheMatrix 
cacheSolve <- function(x, ...) {

  ## obtain the inverse matrix drived from makeCacheMatrix
  m <- x$getsolve()
  
  ## the following condition checks whether the inverse matrix is the cache
  ## and obtain the values
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## the following retrieves original value from the makeCacheMatrix input
  ## and inverse the value (x)
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  ## returns inverse matrix  
  m
}
