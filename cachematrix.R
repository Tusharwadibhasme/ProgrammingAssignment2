## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Programming Assignment 2: Lexical Scoping
# This R script defines two functions: makeCacheMatrix and cacheSolve

# This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function computes the inverse of the special matrix returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

# Example usage:
# m <- matrix(c(1, 2, 3, 4), 2, 2)
# cm <- makeCacheMatrix(m)
# cacheSolve(cm)
# cacheSolve(cm)  # This time should return cached result
