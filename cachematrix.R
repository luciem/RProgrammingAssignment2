## The following pair of functions can be used to cache the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the inverse is retrieved from the cache, thus allowing the programmer
## to avoid costly computations.

## N.B. Both functions assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    set <- function(Y) {
        X <<- Y
        inv <<- NULL
    }
    get <- function() X
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" object returned by
## makeCacheMatrix above.
cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'X'
    inv <- X$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- X$get()
    inv <- solve(data, ...)
    X$setinverse(inv)
    inv
}
