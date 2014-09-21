## Put comments here that give an overall description of what your
## functions do
## by aaimard

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  ## Function has get/set for 2 properties
  ## 1- A matrix called x
  
  
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  ## 2- The inverse of that matrix called invx
  setinvx <- function(solve) inv <<- solve
  getinvx <- function() inv
  list(set = set, get = get,
       setinvx = setinvx,
       getinvx = getinvx)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      
  
  ## Return a matrix that is the inverse of 'x'
  
  ## let's check if the inv is already stored in cache
  invx = x$getinvx()
  
  if(!is.null(invx)) { ## well lookie here, we have the cached version.. just serve it fresh
    message("getting cached data")
    return(invx)
  }
  
  ## no luck this time, so let's Solve the inverse of the matrix 
  matrix.data <- x$get()
  invx <- solve(matrix.data, ...)
  
  ## and store it in cache for next time
  x$setinvx(invx)
  invx
}
