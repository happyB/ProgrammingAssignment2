## A matrix inversion is a complex mathematical operation 
## (see e.g. http://www.purplemath.com/modules/mtrxinvr.htm).
## The following two functions are meant to allow the repeated determination of the result
## of a matrix inversion, BUT if an inverse of a matrix has been calculated once and the original
## matrix has not changed, the functions will return the CACHED inverse of the original matrix,
## thereby saving computation time.

# The makeCacheMatrix function creates a 'matrix' object, which in reality is a LIST containing a function
# that defines 4 different calls (set, get, setinverse, getinverse). This function expects to be passed a
# "real" matrix object.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) i <<- solved
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The cacheSolve function calculates the inverse of the "matrix" created by the makeCacheMatrix function.
# It first checks whether the inverse has already been calculated for the original matrix, and if it has
# (without the original matrix having been re-set in the meantime) it simply returns the cached inverse
# of the matrix, without new calculation. If the inverse of the matrix is not stored in the 
# makeCacheMatrix function, the cacheSolve function will calculate it and cache the result in the 
# makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
