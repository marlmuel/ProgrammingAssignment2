## These 2 functions below calculate the inverse of a matrix, if the same computation gets repeated a cached version
## of the result is returned, instead of calculating it again.
## This only works, if the result of the "makeCacheMatrix" is stored in a variable and this variable is passed onto
## the "cacheSolve" function. If this is repeated for the same matrix, the cached inverse is used.

## The "makeCacheMatrix" functions defines 4 internal functions: 1. set the matrix, 2. get the matrix, 
## 3. set the inverse of the matrix, 4. get the inverse of a matrix. It returns a list containing these 4 variables. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The "cacheSolve" function uses the matrix constructed in the above function. 
## It checks if for this matrix an inverse was already calculated, 
## if yes, it prints "getting cached data" and prints/returns the inverse to the console and the program ends, 
## if not it gets the new matrix and calculates the inverse of it
## and sets the inverse "globally" to the result of this new computation.
## Finally it returns the inverse (to the console)

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}