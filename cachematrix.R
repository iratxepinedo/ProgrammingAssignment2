## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  
  set <- function( matrix ) {
      m <<- matrix
      i <<- NULL
  }
  
  get <- function() m
        
  setInverse <- function(invr) {
      i <<- invr
  }
    
  getInverse <- function(){
    i
  }  
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m)
  x$setInverse(i)
  i
}
        

