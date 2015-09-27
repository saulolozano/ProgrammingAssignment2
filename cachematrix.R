## The following functions handle the calculation of the inverse of a matrix and caches its result
## if the calculus made, then the result is extracted from the caches instead re-calculating the inverse


## This function returns a list with setter and getter of the inverse of a given matrix and the matrix itself

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(newInverse) inverse <<- newInverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function determines if an inverse of the matrix previously given exists in the cache, if not it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        
        x$setinverse(inverse)
        inverse
}
