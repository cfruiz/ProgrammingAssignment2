## This file contains a set of two functions which store in cach� a matrix and
## its corresponding inverse in order to avoid repetitive matrix inverse
## computations. If the matrix has already been stored and its inverse computed,
## then the inverse is returned to the user without inverting the matrix again.



## This function returns a list object containing four functions to obtain
## and set a non-singular matrix and its corresponding inverse
## This function makes use of the super assignment operator (<<-) to modify
## global variables x (the non-singular matrix to be inverted) and inv (the
## inverse of x)

makeCacheMatrix <- function(x = matrix()) {
  
  ## Create a null object since the inverse has not yet been computed 
  inv <- NULL
  
  ## Function to set matrix X
  setMatrix <- function(y) 
  {
    ## Change global variable X for formal argument Y
    x <<- y
    ## Matrix has changed so its inverse is set to null (not yet computed)
    inv <<- NULL
  }
  
  ## Function to obtain matrix X
  getMatrix <- function() x
  
  ## Function to set the inverse of X as global variable (store in cach�) 
  setInverse <- function(inverse) inv <<- inverse
  
  ## Function to obtain the cach�d matrix inverse
  getInverse <- function() inv
  
  ## Returns a list object with the following elements:
  ## 1. Function setMatrix
  ## 2. Function getMatrix
  ## 3. Function setInverse
  ## 4. Function getInverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function checks if the inverse of the matrix has already been computed
## If so, the function returns the stores inverse
## If not, the function computes the inverse, stores it and returns it

cacheSolve <- function(x, ...) {
  
  ## Obtain the cach�d inverse of the matrix
  inv <- x$getInverse()
  
  ## If the inverse is not a null object, return the cach�d inverse matrix
  if(!is.null(inv)) {
    message("Matrix inverse stored in cach�. Getting inverse from cach�...")
    return(inv)
  }
  
  message("Matrix inverse not yet computed. Inverting matrix X...")
  
  ## If the inverse is a null object, retrieve the non-singular matrix 
  ## (invertibleMatrix) and compute its inverse with the built-in solve function
  invertibleMatrix <- x$getMatrix()
  
  ## Compute the inverse of invertibleMatrix with solve function
  inv <- solve(invertibleMatrix, ...)
  
  ## Store the recently computed inverse in cach�
  x$setInverse(inv)
  
  ## Return the matrix inverse
  inv
}