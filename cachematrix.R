## The below code will first get a matrix, store its values and then create and store 
## the inverse of that matrix if it hasn't done so already.  In short we will be caching the 
## inverse of a matrix

## This code has four steps 1.  set the value of the matrix 2.  get the value of the matrix
## 3.  set the value of the inverse 4.  get the value of the inverse

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
     x
     getinv
}


## These functions calculate the inverse of the matrix created with the above function. However, 
## it first checks to see if the inverse has already been calculated. If so, it `get`s the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix  
## and sets the value of the inverse matrix in the cache via the "setinv" function.

cacheSolve <- function(x, ...) {
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
        ## Return a matrix that is the inverse of 'x'
}

