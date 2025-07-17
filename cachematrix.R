## Overall, these two functions work to cache the inverse of a matrix to avoid computing it repeatedly.

## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  }
  setinv <- function(i){
    inv <<- i
  }
  getinv <- function(){
    inv
  }
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  } 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
