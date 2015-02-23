## The purpose of makeCacheMatrix and cacheSolve is to hold a matrix 
##and cache the inverse for quick retrieval

## makeCacheMatrix stores a matrix and functions needed to retrieve it and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #Inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## solves for matrix inverse. if inverse is already cached, retrieves it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
