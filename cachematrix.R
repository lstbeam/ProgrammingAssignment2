## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The objective of makeCacheMatrix is to save in cache the inverse of a square matrix
## The inverse matrix is saved in the variable named mxInverse
## makeCacheMatrix has 4 function inside: set(matrix), get(), setMatrixInverse(matrix), getMatrixInverse(), 
## 
makeCacheMatrix <- function(x = matrix()) {
  mxInverse <- NULL
  
  ##function set(): to set the original matrix and clear the saved inverse matrix
  set <- function(y) {
    x <<- y
    mxInverse <<- NULL
  }
  
  ## function get(): gets the original matrix previously setted
  get <- function() x
  
  ## function setMatrixInverse(): set the inverse matrix in chache
  setMatrixInverse <- function(inverse) mxInverse <<- inverse
  
  ## function getMatrixInverse(): gets the inverse matrix in cache
  getMatrixInverse <- function() mxInverse
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)

}


## Write a short comment describing this function

#From the assignment's description page: "assume that the matrix supplied is always invertible."
## The objective of cacheSolve is to calcule an inverse matrix only if it's necessary
## Onces the inverse matrix is calculated, it's saved in cache using the function 
## setMatrixInverse in makeCacheMatrix. Thus, the next time the user needs that same inverse matrix
## cacheSolve will get it from cache, instead of calculating it


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInv <- x$getMatrixInverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data)
  x$setMatrixInverse(mInv)
  mInv
}


