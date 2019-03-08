## This set of functions can create first a "matrix" object (which is a matrix)
## and stores the inverse of said object. This means the inverse is cached and
## can be reused, saving computational time.

## makeCacheMatrix creates the matrix object and is also responsible for caching it.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize objects
  inv <- NULL
  ## Define set function. Assigns to the parent environment.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Define get function. Retrieves x from parent environment.
  get <- function() x
  ## Define setinv function. Assigns solve function to parent environment.
  setinv <- function(solve) inv  <<- solve
  ## Define getinv function. Retrieves solve function from parent environment.
  getinv <- function() inv
  ## Create a list with all functions from above and return it to parent environment.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is responsible for calculating the inverse of the matrix or
## returning the cached data.

cacheSolve <- function(x, ...) {
  ## Get the current value of inv.
  inv <- x$getinv()
  ## Test if inv is not empty.
  if(!is.null(inv)) {
    ## If not, return message and cached data.
    message("Using cached data")
    return(inv)
    }
  ## In case inv is empty, get the matrix.
  data <- x$get()
  ## Calculate the inverse.
  inv <- solve(data, ...)
  ## Assign inverse.
  x$setinv(inv)
  ## Retunr inverse.
  inv
}
