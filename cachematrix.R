# Programming Assignment 2 - Lexical Scoping
#
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # The cahced object
  cached_obj <- NULL
  
  set <- function(y) {
    x <<- y
    
    #Clear the cache
    cached_obj <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) { 
    cached_obj <<- inverse
  }
  
  getInverse <- function() {
    cached_obj
  }
  
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has alreadybeen calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

  #get the inverse of matrix x from 'matrix' object
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("Getting cached result")
    return(inv)
  }
  
  #Inverse of x is not cached, we need to calculate it.
  data <- x$get()
  
  #Calcule the inverse of x
  inv <- solve(data,...)

  #cache the result
  x$setInverse(inv)
  
  inv
}
