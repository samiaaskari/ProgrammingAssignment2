## makeCacheMatrix takes matrix as input and creates a special "matrix" object that 
  #can cache its inverse

## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setMatrixInverse <- function(solve) m <<- solve
  getMatrixInverse <- function() m
  
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## This function is copmuting the inverse of the special matrix
  #created using makeCacheMatrix() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # getting results from getInverse() function
  m <- x$getMatrixInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  #computing the inverse of matrix using solve and storing in m
  m <- solve(data, ...)
  
  #setting the inverse matrix results by calling setMatrixInverse function
  x$setMatrixInverse(m)
  m
  
}
