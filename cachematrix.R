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


## Return the inverse if it exists or compute it and cache it if not
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        return (inverse)
    }
    
    m <- x$get()
    identityDim <- prod(dim(m))
    identity <-
        matrix(rep.int(1, times = identityDim), nrow = nrow(m), ncol = ncol(m))
    
    inverse <- solve(m)
    x$setInverse(inverse)
    inverse
}
