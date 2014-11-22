## These two functions work together to solve for the inverse of a matrix and 
## cache the solution. If the matrix had already been solved, then the previously 
## cached solution is returned before the "solve" function can be run again.

## This function creates the list of four functions get, set, getInv, setInv which together
## let the user cache the inverse of a newly solved matrix or retrieve the inverse of
## a matrix that had already been solved.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <- inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function takes a "matrix" 'x' (created by makeCacheMartix) and returns the inverse. 
## If the matrix had previously been solved then its inverse should have been cached. 
## In that case, the function returns the cached solution and ends rather than 
## find the inverse again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if ( !is.null(inv) ) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
