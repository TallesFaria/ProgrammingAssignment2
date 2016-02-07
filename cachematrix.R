## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#create an special "matrix" that can cache its inverse

makeCacheMatrix <- function(z = matrix()) {
  inverse <- NULL
  set <- function(y) {
    z <<- y
    m <<- NULL
  }
  get <- function() z
  setInverse <- function(Inverse) inverse <<- Inverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {
  inverse <- z$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- z$get()
  #calcular matrix inversa 
  inverse <- solve(data)
  z$setInverse(inverse)
  inverse
}
        
