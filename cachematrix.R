## Function makeCacheMatrix will create four functions for get and set the matrix and
## its inverse while funciton cacheSolve will compute the matrix inverse and save it

## This function creates a special "matrix" object that can cache its inverse. The
## returned list contains four function for getting and setting the matrix and its
## inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Function for setting the matrix and store it
  set <- function(y) {
    x <<- y
    # The inverse is reset
    i <<- NULL
  }
  # Function for getting the stored matrix
  get <- function() x
  # Function for setting the matrix inverse
  setinv <- function(inv) i <<- inv
  #Funciton for getting the stored matrix inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  # If it has been calculated, it is returned
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # Otherwise, we compute and store it
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
