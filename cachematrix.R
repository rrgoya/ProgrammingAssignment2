## This function create the inversion of a matrix
## may it be called again, it provides a cache copy

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setcache <- function(solve) m <<- solve
        getcache <- function() m
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## Return the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getcache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setcache(m)
        m
}
