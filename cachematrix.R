## This function creates a matrix and cahces its inverse

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## return value is a list containing functions which are used as input to list()
  
  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


  ## Computes the inverse of the matrix 

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix()
  ## return value is theinverse of the input matrix
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  
  x$setinv(inv)
  
  return(inv)
}
