## These functions return the inverse of the matrix passed 
##  and cache the inversed matrix for future use

## This function creates a matrix to cache the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  curr = NULL
  set = function(y) {
    x <<- y
    curr <<- NULL
  }
  get = function() x
  setinv = function(new) curr <<- new  
  getinv = function() curr
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if an inverse value and is present and if it is not then it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m = x$getinv()
  
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data = x$get()
  m = solve(data, ...)
  
  x$setinv(m)
  
  m
}
