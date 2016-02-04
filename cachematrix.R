## Put comments here that give an overall description of what your
## functions do

## The function returns a list with the properties to set and get the
## initial input, and to set and get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(x) {
        m <<- x
        inverse <- NULL
    }
    
    get <- function()
        m
    
    setInverse <- function(x) {
        inverse <<- x
    }
    
    getInverse <- function()
        inverse
    
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Return the inverse of a matrix if it exists
## Compute it and cache it if it doesn't, and then return it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        return (inverse)
    }
    
    m <- x$get()
    
    inverse <- solve(m, ...)
    x$setInverse(inverse)
    inverse
}
