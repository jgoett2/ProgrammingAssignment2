## These functions allow the user to cache the inverse to a matrix, and reuse the inverse in the future, rather than re-calculating the inverse whenever needed.

## This function creates a list storing functions necessary for caching the inverse of a matrix. The input is the matrix of interest, and the list is output.  This is used in conjunction with the cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function()
    x
  setinverse <- function(i)
    inverse <<- i
  getinverse <- function()
    inverse
  list(
    set = set, get = get, setinverse = setinverse, getinverse = getinverse
  )
}



## This method calculates the inverse of a matrix.  If a cached solution is available, it is used rather than re-calculating the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}
