## makeCacheMatrix makes a matrix, cacheSolve inverts this 
## matrix (if invertable) and stores the inverse in cache

## makeCacheMatrix creates a matrix x.1 and a list of three functions:
## get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
        m.1 <- NULL
        get <- function() x
        setinverse <- function(buffalo) m.1 <<- buffalo
        getinverse <- function() m.1
        list( get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve looks for a previous inverse of x and returns it, if possible.
## If not, it solves for an inverse of x and returns that

cacheSolve <- function(x, ...) {
        m.2 <- x$getinverse()
        if(!is.null(m.2)) {
                message("getting cached data")
                return(m.2)
        }
        data <- x$get()
        m.2 <- solve(data, ...)
        x$setinverse(m.2)
        m.2
}
        ## Return a matrix that is the inverse of 'x'

