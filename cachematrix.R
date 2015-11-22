# Matrix inversion is usually a costly computation and there 
#may be some benefit to caching the inverse of a matrix rather than 
#computing it repeatedly . To acive this bellow 2 functions are developed.
#   
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# Assumption :  the matrix supplied is always invertible.


##makeCacheMatrix:
##This function  makeCacheMatrix creates a special "matrix" object that can cache its inverse
## this cache  R function is able to cache potentially time-consuming computations and saves time
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

# cacheSolve: 
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
###then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square is done with the solve(X) function in R

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}




### sample run
# x <- matrix(1:4, 2, 2)
# m <- makeCacheMatrix(x)
# cacheSolve(m)
# cacheSolve(m)


