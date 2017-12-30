## Put comments here that give an overall description of what your
## functions do

## This function creates a cache of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    myInverse <- NULL

    get <- function() x

    set <- function(y) {
        myInverse = NULL;
        x <<- y
    }

    setinverse <- function(inverse) {
        myInverse <<- inverse
    }

    getinverse <- function() {
        myInverse
    }

    list(
        get = get,
       set = set,
       setinverse = setinverse,
       getinverse = getinverse
    )
}


## This function computes the inverse of a matrix
## If the inverse is already calculated, it just returns that value
## Otherwise, the inverse is calculated and cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (is.null(inverse)) {
        message("Calculating Inverse")      ## Inverse needs to be calculated
        data <- x$get()
        inverse <- solve(data, ...)         ## Find the inverse
        x$setinverse(inverse)
        return (inverse)
    }
    message("Fetching cached Inverse")      ## If we get here, the Inverse was already calculated and avaiable.
    return(inverse)
}
