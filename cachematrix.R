## The two functions below are used to create a special object that stores a matrix and caches its inverse matrix.
## For this assignment, we assume that the matrix supplied is always invertible.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## We use the <<- operator here to assign a value to an object in an environment that is different from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  set <- function(y) {
    x <<- y
    m <<- NULL
  }
 
 get <- function() x 
 setinv <- function(solve) m <<- solve
 getinv <- function() m
 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
 
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
   return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
