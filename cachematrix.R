## Functions in combination caches the inverse of a matrix to speed up calculations in a loop, as it allows to 
## store the results in the enclosing environment and then fetch them from this environment

## Function creates a cache matrix that will store the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
	        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function() m <<- solve(x)
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Function returns an inverse by either calculating it or by fetching it from th environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
