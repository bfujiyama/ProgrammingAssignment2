# Create two functions to create a special matrix that can cache its inverse for recall or to otherwise compute the inverse. Assumes the matrix is always invertible.

# makeCacheMatrix creates special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# cacheSolve checks to see if the inverse of the matrix already exists or uses solve(x) to calculate and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setInverse(inv)
                inv
}
