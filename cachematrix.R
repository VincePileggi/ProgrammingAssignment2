## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix creates a special "matrix" object that can cache 
# its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## makeCacheMatrix creates a special "matrix" object that can cache 
##  its inverse.
makeCacheMatrix = function(x=matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve(x)
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
cacheSolve <- function(x, ...){
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}