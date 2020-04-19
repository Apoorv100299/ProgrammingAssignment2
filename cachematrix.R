## The given two functions are used to claculate inverse of matrix

## It can cache inverse of any matrix of any number of rows/columns

makeCacheMatrix <- function(x = matrix()) {
## x is the input matrix 
        ## return value is a list containing functions to be used as input to cacheSolve()
       
        
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

cacheSolve <- function(x, ...) {
        ## x is the output of makeCacheMatrix()
        ## the returned value is the inverse of the matrix
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
