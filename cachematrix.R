## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Note x= numeric() is working
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    if(length(x)==0){
      message("set and get x")
      set(y)
    }
    x
  }  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("calculating the inverse for the first time")
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
