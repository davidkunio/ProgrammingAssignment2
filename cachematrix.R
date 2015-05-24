## This script containts 2 funcions.
## Function 1: Create a Matrix and calculate the inverse
## Function 2: Determine if inverse is cached and if so use cached.
##             If not, calculate the inverse

## Create Matrx and calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Check cash for inverse and use if avail.  Otherwise calc. inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...) %*% m
  x$setinverse(i)
  i
}
