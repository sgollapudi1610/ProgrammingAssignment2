## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## Here, a pair of functions that cache the inverse of a matrix are created.
 

## The function makeCacheMatrix creates a matrix object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
               x <<- y
               inv <<- NULL
          }
          get <- function() x
          setinv <- function(inverse) inv <<- inverse
          getinv <- function() inv
          list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The function cacheSolve determines whether or not a matrix inverse has already
## been cached. If there is an inverse calculation already cached, the function
## will get that calculated value. Otherwise, it will calculate the inverse of
## the matrix (it is assumed that the matrix is always invertible).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        if(!is.null(inv)) {
               message("getting cached data")
               return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv

}
