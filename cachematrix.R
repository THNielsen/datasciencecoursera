## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set new value of matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  ## set new  value of solve (inverse) i.e. not the same as calculate
  setsolve <- function(solve) m <<- solve
  ## get value of inverse matrix
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## cacheSolve calculates the correct inverse value ie has to be called

  m <- x$getsolve()
  if(!is.null(m)) {
    ## here we just get hold of value , or use old one if exists
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## here the inverse value is calculated directly in the cacheSolve function
  m <- solve(data, ...)
  x$setsolve(m)
  m
  }


