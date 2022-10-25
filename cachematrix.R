## Put comments here that give an overall description of what your
## functions do

## The function below ("makeCacheMatrix") creates a special “matrix”, 
## Within the special "matrix" function, is a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
set <- function(y){
  x <<- y
  i <<- NULL
} 

get <- function() X
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set, 
     get = get, 
     setinverse = setinverse,
     getinverse = getinverse)
}


## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse is already calculated but the matrix has not changed),
# then cacheSolve function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
