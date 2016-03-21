## Put comments here that give an overall description of what your
## functions do

## A matrix that can cache it's inverse
## Assumes the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Computes the inverse of a special matrix which has a cached value of its inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
