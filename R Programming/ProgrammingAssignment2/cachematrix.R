## cachematrix.R
##
## The following pair of functions offer the possibility to compute 
## the Inverse of a square Matrix with a caching mechanism for better
## performance.



## This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverseMatrix <- NULL
  ## Set Matrix
  set <- function(y) {
    x <<- y
    cachedInverseMatrix <<- NULL
  }
  ## Get Matrix
  get <- function() x
  ## Set Inverse Matrix
  setInverse <- function(inverse) cachedInverseMatrix <<- inverse
  ## Get Inverse Matrix
  getInverse <- function() cachedInverseMatrix
  ## List of public functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function will compute the inverse of a special "matrix" object.
## If the inverse has already been calculated, it will return the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Get inverse matrix if previously cached
  inverseMatrix <- x$getInverse()
  
  ## If inverse matrix was cached, return it
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  ## If not, calculate inverse matrix and cache it
  matrix <- x$get()
  inverseMatrix <- solve(matrix)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
