##this file consists of two functions

## this is a function that creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   ##m set to NULL because it is used later in the function
  set <- function (y) {
    x <<- y     ##assigns y to the parent environment
    m <<- NULL  ##m in parent environment set to NULL when x is reset
  }
  get <-function() x ##takes x from parent environment (not defined in makeCacheMatrix)
  setmatrix <- function(solve) m<<- solve ##assign inverse to m in the parent environment
  getmatrix <- function() m ## takes m from parent environment
  list (set=set
      , get=get
      , setmatrix=setmatrix
      , getmatrix=getmatrix) ##make a list in the parent environment of the subfunctions created.
}


## this is a function that returns the inversed matrix of X

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()                    ##assign to m the inverse matrix from the parent environment
  if(!is.null(m)){
    message("getting cached data")
    return(m)                           ##when m is not null then it uses the cached data from the parent environment
  }
  data <- x$get()                       ##otherwise it uses the matrixdata...
  m <- solve(data,...)                  ##...to calculate the inverse of the matrix...
  x$setmatrix(m)                        ##... and assign it to the parent environment
  m                                     ##return the inversed matrix of 'x'
  }
