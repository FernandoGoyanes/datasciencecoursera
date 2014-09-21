## Coursera - Data Sicence: R - ProgrammingAssignment2
## 2 functions that cache the inverse of a matrix
## (1) Make a cache matrix and (2) compute the inverse

## (1) It creates a matrix object that cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse 
    i
  }
  
  ## Return list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## (2) Compute inverse of the matrix returned befor from cache
cacheSolve <- function(x, ...) {
  
  ## Return matrix that is inverse of x
  m <- x$getInverse()
  
  ## Return inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get matrix from object
  data <- x$get()
  
  ## Calculate inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set inverse to the object
  x$setInverse(m)
  
  ## Return matrix
  m
}
