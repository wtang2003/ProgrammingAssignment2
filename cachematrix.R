## Programming assignment 2:write an R function that is able to cache 
## potentially ## time-consuming computations

# The makeCacheMatrix funtion creates a special "matrix" object that can cache
# its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set, get = get, 
               set_inverse = set_inverse, 
               get_inverse = get_inverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix function. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        # Check whether the inverse has been been calculated already
        inv <- x$get_inverse()

        ## If yes, return a message and the matrix that is the inverse of 'x'
        if (!is.null(inv)) {
                message("getting inversed cache data")
                return(inv)
        }
        
        # Get the inverse and populate the values into the matrix
        # created to store the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}
