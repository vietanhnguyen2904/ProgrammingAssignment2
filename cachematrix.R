## These two functions compute the inverse of a matrix. If the inverse was already computed before
## (and saved in memory), we will simply read the result from memory. If the inverse hasn't been computed,
## we will compute it by calling the standard function solve in R. We then will save the result in memory for
## the next use.

## This function creates a special matrix, and can do 4 things: set its value, get its value, set its inverse's value,
## and get its inverse's value.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes inverse of the special matrix created above. It first check whether the inverse has been
## computed. If yes, it reads the inverse from memory and skips the computation. If no, it compute the inverse and
## save the result in the cache by the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat)
  x$setinverse(inv)
  inv
}
