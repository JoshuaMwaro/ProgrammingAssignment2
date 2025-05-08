## Writing a pair of functions that cache the inverse of a matrix to 
## avoid repeatedly computing the inverse 

## The first function 'makeCacheMatrix', creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL
  
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }

  get <- function() x
  
  setinverse <- function(inverse) inver <<- inverse
  
  getinverse <- function() inver
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function 'cacheSolve' function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above.

## If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
  
  inver <- x$getinverse()
  if (!is.null(inver)) {
    message("getting cached inverse")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setinverse(inver)
  inver
}

