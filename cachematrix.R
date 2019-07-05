## Creating two functions, that creates a matrix into cache and make the inverse of the matrix.


## Creating matrix into cache, saving the inverse of the x matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## Getting the processed matrix from cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if (!is.null(i)) {
          message("getting DATA from cache")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}


mx <- matrix(c(5,6,9,7),2,2)
M <- makeCacheMatrix(mx)
cacheSolve(M)
cacheSolve(M)
