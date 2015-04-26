## Coursera: Johns Hopkins
## R Programming Assignment 2
## 26 April 2015

## The makeCacheMatrix function creates a list of functions to:
## 1. set and get matrix data
## 2. set and get the inverse of the matrix data

## Example usage:
## 1. Create a 2 x 2 matrix and assign to "mdata" 
##    mdata <- matrix(rnorm(4),2,2)  
## 2. Create the cached matrix passing mdata as an argument 
##    and assign the returned list to "mlist"
##    mlist <- makeCacheMatrix(mdata)
## 3. Use the cacheSolve function to return the inverse of the matrix
##    cacheSolve(mlist)

makeCacheMatrix <- function(x = matrix()) {
  
  # inverse_matrix is used for the cached inverse matrix
  # initialise with NULL
  inverse_matrix <- NULL
  
  # THis function is used to set the values for the matrix 
    set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  # This function is used to get values from the matrix
  get <- function() x
  
  # This function is used to set the inverse of the matrix 
  setinverse <- function(inverse) inverse_matrix <<- inverse
  
  # This function is used to get the inverse of the matrix
  getinverse <- function() inverse_matrix
  
  # Returns list of functions to set and get matrix data and inverse of the data
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function returns the inverse of a matrix 
##
## If a cached inverted matrix is available, Then 
##   this function will retrieve it and return it, 
## Else 
##   the function will invert the matrix, cache it and return it.
##
## See Example usgae above.
cacheSolve <- function(x, ...) {
  
  # Try to get the inverse of the matrix and assign to inverse_matrix
  inverse_matrix <- x$getinverse()
  
  # If the inverse_matrix contains something i.e. not null, then
  if (!is.null(inverse_matrix)) {
    ## Display a message and return the data
    message("Cached data retrieved and returned")
    return(inverse_matrix)
  }
  
  # Else, the inverse of the matrix needs to be calculated
  
  # 1. Get the matrix data
  matrix_data <- x$get()
  
  # 2. Invert the data and assign to inverse_matrix
  inverse_matrix <- solve(matrix_data, ...)
  
  # 3. Cache the inverted data
  x$setinverse(inverse_matrix)
  
  # 4. Display a message and return the inverted data
  message("Inverse data calculated, cached and returned")
  return(inverse_matrix)
}
