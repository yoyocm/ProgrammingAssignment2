## Functions allows to create a cache matrix in order to 
## keep in cache an inversed matrix and don't reuse one which will be solved yet

## Returns a list of functions 
## First one set the matrix value
## Second get the matrix value
## Third set the inversed matrix value
## Fourth get the inversed matrix value
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Get a matrix in argument and return her inversed
## If the inversed matrix is in cache, it is returned
## Else, the inversed matrix is solved and returned 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
