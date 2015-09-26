##############################################################################
## Cacheing a matrix and it's inversion
## Author: Charlie Buffkin
## Date: 26-Sep-2015
## Class: Coursera, R Programming (rprog-032)
##############################################################################

## The two functions below work together to establish and hold a matrix and
## its inverse in such a way that once the inverse is calculated, it will
## remain with the function's scope.

## This function creates a special "matrix" object that can cache its inverse.
## This matrix is really an object with functions contained within that perform
## the actions of storing the original matrix and using cacheSolve to 
## establish the inverse and store it in the "matrix" object.
makeCacheMatrix <- function(x = matrix()) {
    ## `m` is the manipulated object that allows us to test whether the inverse
    ## has been stored yet or not.
    m <- NULL
    
    ## the `set` function takes the input when establishing this "matrix" and
    ## stores it within its scope as `x`. it sets `m` to null so that we can
    ## know that the value has not yet been stored (this is tested in
    ## cacheSolve)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## the `get` function returns the matrix that is stored in `x`
    get <- function() {x}
    ## the `setinverse` function accepts the input and stores it in `m`. in
    ## this case, it is expected that the input is the inverse of the matrix
    ## `x` which is how it is manipulated within the cacheSolve function
    setinverse <- function(inverse) {m <<- inverse}
    ## the `getinverse` function returns the contents of `m` which should be
    ## the inverse of `x`, but may be null if it has not yet been calculated.
    ## the cacheSolve function uses this to determine if the value has yet
    ## been calculated. if it has not, it will calculate the inverse and put
    ## it in `m` using the `setinverse` function.
    getinverse <- function() {m}
    ## this simply returns the content of the functions if you simply run the
    ## makeCacheMatrix without any reference to the specific list items
    ## (internal functions).
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## pull the inverse matrix value stored in our special "matrix"
    m <- x$getinverse()
    ## if the inverse returned is not null, then use it and state that it is
    ## pulled from cache.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if the returned value for `m` is null, then we need to calculate
    ## the inverse and then push it into the inverse holder of `x` for use the
    ## the next time it is requested.
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
