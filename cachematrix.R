
## makeCacheMatrix takes matrix as an input parameter and
  # 1) creates a special object "matrix" that can cache its inverse
  # 2) associate 4 functions to the special object
    
## __Explanation of functions in the makeCacheMatrix() function__
  # (a) set() function: this is created so when set() is called, it assigns value to
  # "x" input parameter and variable "m"

  # (b) get() function: this is created so when get() is called, it retrieves the value "x" 

  # (c) setMatrixInverse() function: this is created so when setMatrixInverse() is called,
  # it stores the cached value of the inverse of the matrix

  # (d) getMatrixInverse() function: this is created so when getMatrixInverse() is called,
  # it returns the cached value of the inverse of the matrix


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


## cacheSolve function is computing the inverse of the special matrix object created 
## using makeCacheMatrix() function

  # It first calls the getMatrixInverse function to get the cached matrix inverse.
  
  # If it doesn't find any cached results, then it uses function get() to get the 
  # original input which was passed to function makeCacheMatrix() and store it in variable "data"
  
  # The function then uses solve() to compute the inverse of the input store in "data".
  
  # It finally calls the setMatrixInverse() and pass the computed inverse matrix to be cached


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
