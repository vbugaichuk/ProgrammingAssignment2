## Functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<-y
    m<<- NULL
  }
  get<- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv
  
  ## If the inverse has already been cached, then it is returned
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(x, ...)
  x$setinv(m)
  m
}
