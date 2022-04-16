## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix will create a matrix and
## stores it in the Global Environment to be used by cacheSolve
## In case that we need to set another matrix it will set the inverse matrix
## to NULL to allow a new procedure via cacheSolve
## It will also create a list with methods for get/set of both original matrix
## and its inverse, return the list to a parent environment
## This technique allows use of $ operator to access
## each function from the list

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) mInv <<- solve
  getsolve <- function() mInv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

## cacheSolve will use x to return its inverse
## in case that there is no x set it will allow its
## setting in its execution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInv <- x$getsolve()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data, ...)
  x$setsolve(mInv)
  mInv
}
