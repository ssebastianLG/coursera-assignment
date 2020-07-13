makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  ## define the 4 different behaviors or functions for object of type
  ## makeCacheMatrix as follows:
  ## 1. set() takes an argument named y or any object name other
  ## than x. y is assumed to be a matrix.
  set <- function(y) {
    x <- y
    invs <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <- inverse
  getinverse <- function() invs
  ## Create the new special "matrix" object by returning a list()
  ## to assign each of these functions as an element within a list ()
  ## and returns it to the parent environment. Here we're naming the
  ## list elements, which allows to use the $ form of the extract
  ## operator to access the functions by name

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## the function attempts to retrieve an inverse from the matrix
## object passed in as the argument. First, it calls the getinverse()
## function on the input object.

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  ## then it checks to see whether the result is NULL. Since
  ## makeCacheMatrix() sets the cached inverse to NULL whenever a new
  ## matrix is set into object, if the value here is not equal to NULL,
  ## we have a valid, cached inverse and can return it to the parent
  ## environment
  
  ## if the result of !is.null(invs) is FALSE, cacheSolve() gets the matrix
  ## from the input object, calculates the solve(), uses the setinverse()
  ## function on the input object to set the inverse in the input object,
  ## and then returns the value of the inverse to the parent environment
  ## by printing the inverse object
  
  mat <- x$get()
  invs <- solve(mat, ...)
  x$setinverse(invs)
  invs
}
