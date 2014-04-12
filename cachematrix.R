## A pair of functions that allows to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## Calculate and store the inverse if it 
## hadn't been calculated before
## otherwise use the stored result
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## Test
#m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))
#m$get()
#m$getinverse()
#cacheSolve(m)
#cacheSolve(m)
#m$getinverse()
