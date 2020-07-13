makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <- y
    invs <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <- inverse
  getinverse <- function() invs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  invs <= x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invs <- solve(mat, ...)
  x$setinverse(invs)
  invs
}
