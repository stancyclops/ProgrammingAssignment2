## Matrix inversion is usually a costly computation, so this pair of functions
## caches the inverse of a matrix.

## This function creates a special matric object that can cache its inverse.

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


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix. 

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
            
      }
      data <- x$get()
      m <- solve(data, ...)
      
        ## Return a matrix that is the inverse of 'x'
      
      x$setinverse(m)
      return(m)
}
