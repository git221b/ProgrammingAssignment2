## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## There are two functions makeCacheMatrix, cacheSolve
## makeCacheMatrix consists of set, get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## Initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} ## Function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This is used to get the cache data

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){    ## Checking whether inverse is NULL
    message("getting cached data")
    return(inv) ## Returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
