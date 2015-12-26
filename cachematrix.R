# R can be used to perform numerous mathematical operations like computing inverse of matrix. It is costly to compute inverse of matrix repeatedly. 
# So, We can compute the inverse and cache it to retrieve if needed later on.
# This file contains two functions makeCacheMatrix and cacheSolve.
# makeCacheMatrix -> create matrix and cache matrix as well as inverse of that matrix.
# cacheSolve -> Retrives the inverse of matrix if cached. Otherwise calculate the inverse of the matrix created by makeCacheMatrix

#makeCacheMatrix creates a matrix and also can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#CacheSolve computes the inverse of the matrix returned by makeCacheMatrix function. 
#If the inverse is cached, it retrives inverse of that matrix from cache. Otherwise, it computes the inverse of that matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
