## The functions makeCacheMatrix and cacheSolve together create a mechanism to 
## cache the inverse of a square invertible matrix

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# It takes a square invertible matrix as input and returns a list containing four functions:
#   1. setMatrix: Function to set the matrix value in the cache.
#   2. getMatrix: Function to retrieve the matrix from the cache.
#   3. setInverse: Function to set the inverse of the matrix in the cache.
#   4. getInverse: Function to retrieve the cached inverse of the matrix.
# If the matrix is not yet computed, the inverse is set to NULL.## This function (makeCacheMatrix takes one argument 'x' which is a matrix inv is set to 
##to NULL 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# It takes the special "matrix" object as input and returns the inverse of the matrix.
# If the inverse is already cached, it retrieves the inverse from the cache.
# If the inverse is not yet computed, it calculates the inverse using the solve function in R,
# stores it in the cache using setInverse function, and then returns the inverse.
# Note: This function assumes that the input matrix is invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
            message("Getting cached inverse")
            return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
