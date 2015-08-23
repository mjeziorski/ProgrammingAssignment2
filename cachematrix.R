## This pair of functions calculates the inverse of a matrix,
## but checks first to determine if the inverse has already been calculated.
## If so, it skips calculation and returns the cached inverse.

## makeCacheMatrix is a function designed to cache a matrix and its inverse.
## It returns a list containing a function to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix using solve()
##    4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the vector created by makeCacheMatrix above.
## First it checks to see if the inverse has already been calculated.
## If so, it gets the stored inverse and skips the computation.
## Otherwise, it computes the inverse and stores it in the cache using setinverse.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}