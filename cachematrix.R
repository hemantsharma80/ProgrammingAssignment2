## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix function: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  
  # set the matrix
  
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  
  # get the matrix
  
  
  get <- function() x
  
  # set the inverse 
  
  setinverse<- function(inverse) invX <<-inverse
  
  # get the inverse
  
  
  getinverse <- function() invX
  
  # return the inverse matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve function : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invX <- x$getinverse()
  
  # check whether inverse exist 
  
  if (!is.null(invX)) {
    message("getting cached inverse matrix")
    
    # return the inverse if it exist
    
    return(invX)
  } else {                        # inverse does not exist
    invX <- solve(x$get())
    
    # cache the inverse 
    
    x$setinverse(invX)
    
    # return the inverse 
    
    return(invX)
  }
}
