## Get inverse of matrix from cache, if available, else solve and cache for future use

##m ake cache matrix
makeCacheMatrix <- function(x = matrix()) {
  ## set value of matrix
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get value of the matrix
  get <- function() x
  ## set inverse of the matrix
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  ## get inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return matrix being inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ## get inverse of matrix
  i <- x$getinverse()
  ## check if available
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if not available: solve matrix
  data <- x$get()
  i <- solve(data, ...)
  ## set inverse of matrix
  x$setinverse(i)
  i
}