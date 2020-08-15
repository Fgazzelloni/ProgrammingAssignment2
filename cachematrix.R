## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix is a vector which is a list of functions to set the inverse of a special matrix object

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y #<<- operator which can be used to assign a value to an object in 
    #an environment that is different from the current environment.
    m <<- NULL
  }
  
  get <- function() x
  setinversa <- function(inverse) m <<- inverse
  getinversa <- function() m
  
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}


## Write a short comment describing this function
##cacheSolve is a function that calculates the inverse of a matrix set by the makeCacheMatrix function above 

cacheSolve <- function(x, ...) {
  
    m <- x$getinversa()
  
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinversa(m)
  m
  }
