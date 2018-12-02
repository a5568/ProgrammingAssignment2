## With these 2 functions its possible to save a matrix um a variable
## and calculate its inverse

## This function receive ded matrix and store in the variable
## it create the possibily of save an cache with the inverse
## using the methods setint and getinv - to save and load
## the saved inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inversion) inv <<- inversion
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function calculate the inverse of a matrix, but first it
## check if it already has the inverse saved

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
