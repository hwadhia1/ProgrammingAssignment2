## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## makeCacheMatrix creates a list containing a function to do the following:
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

## Note:  This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## if getinverse() value is existing (i.e. existing in cache) then return otherwise
  ## get the data set and compute inverse and store in cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get data from cache
  data <- x$get()
  
  ## compute inverse matrix
  m <- solve(data)
  
  ## Set inverse matrix value in cache
  x$setinverse(m)
  
  ## return inverse of matrix
  m
}

# Sample Run
# > source("cachematrix.R")
# > amatrix<-makeCacheMatrix(matrix(c(1:4),nrow=2,ncol=2))
# > amatrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#
# Cache empty before first run and inverse computed
# > cacheSolve(amatrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# Retrieve from cache in second and subsequent runs
# > cacheSolve(amatrix)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
