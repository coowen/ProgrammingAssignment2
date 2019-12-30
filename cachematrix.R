## Put comments here that give an overall description of what your
## functions do
## These functions will create a matrix object that can cache it's inverse.

## Write a short comment describing this function
## This function is for part 1 of the Coursera assignment Week 3 to store the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function () x                       ## get the value matrix
  setInverse <- function(inverse) inv <<- inverse  ## set the value of invert matrix
  getInverse <- function() inv                     ##get the value of invert matrix
  list(setInverse = setInverse, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function takes the output of the previous matrix and checks if the matrix has values or not,
## then returns the matrix and return the message "Getting Cached Data" and returns the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {               ## if inverse matrix is NOT null
    message("Getting Cached Data")  ## return message: Getting Cached Data
    return(inv)                     ##return the inverted matrix
  }
  data <- x$getMatrix()             ## get original matrix data
  inv <- solve(data, ...)           ## use function to solve inverse matrix
  x$setInverse(inv)                 ##set the invert matrix
  inv                               ##return invert matrix
}
