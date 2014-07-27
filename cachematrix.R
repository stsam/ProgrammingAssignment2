## makeCacheMatrix and CacheSolve are used to find the inverse of a matrix.
## if the inverse has been calculated then it uses previously calculated value

## makeCacheMatrix function creates a cache matrix with the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    ## Define set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Define get function
    get <- function() x
    
    ## Define setsolve function
    setsolve <- function(solve) m <<- solve
    
    ## Define getsolve function
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## CacheSolve function checks if the inverse has been previouly calculated 
## otherwise calculates the inverse
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    ## if the cache is calculated then return cached value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if cache is not calculated then calculate it and return value
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setsolve(m)
    return(m)
}