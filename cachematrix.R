## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #set default value for inverse matrix
  m <- NULL 
  #set function that assigns x to y and reset the matrix
  set <- function(y) { 
      x <<- y
      m <<- NULL
  }
  #Get a matrix
  get <- function() x  
  #Set an inverse matrix
  setinvr <- function(solve) m <<- solve  
  #Get an inverse matrix
  getinvr <- function() m   
  list(set = set, get = get,
       setinvr = setinvr,
       getinvr = getinvr)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #Get the matrix from cache
  m <- x$getinvr()
  #Check the matrix from cache whether it has been calculated
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  #Otherwise, it'll calculate the inverse matrix
  data <- x$get()
  m <- solve(data, ...)
  #set matrix to cache
  x$setinvr(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
