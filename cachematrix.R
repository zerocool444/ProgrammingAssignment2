## Functions purposes are to store a matrix and cache the inverse of the matrix.
## If matrix has not changed, then return the cached vesion of the inverse.

## Create function that will store a matrix and store a cached version of the
## inverse to that matrix

makeCacheMatrix <- function(x = matrix()) {
     cmInverse <- NULL
     set <- function(y) {
          x <<- y
          cmInverse <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) cmInverse <<- inverse
     getInverse <- function() cmInverse
     list(set = set, get = get,
          setInverse = setInverse, 
          getInverse = getInverse)
}


## Function is use to get the inverse of a matrix.  If the matrix is not new
## The function will return the cached version.  Else the function will inverse
## the matrix and store that version to the object.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     if(!is.null(m)) {
          message("Returning cached data...")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}
