## Creates a matrix object that enables caching
## of inverse matrix.
makeCacheMatrix <- function(mat = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mat <<- x
                inverse <<- NULL
        }
        get <- function() mat
        setinverse <- function(x) inverse <<- x
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Solves for inverse matrix and caches it
cacheSolve <- function(mat, ...) {
        ## Checks if the inverse matrix has been calculated
        inverse <- mat$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## Inverse has not been calculated.
        ## Solve and then cache result.
        data <- mat$get()
        inverse <- solve(data, ...)
        mat$setinverse(inverse)
        inverse
}