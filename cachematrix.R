makeCacheMatrix <- function(x = matrix()) {
    # Creates a list object which stores passed vector value and cached value. 
    #     
    # Args: 
    #   x:  inversable matrix
    #         
    # Returns:
    #   a list object containing 4 functions: set(), get(), setsolve(), getsolve()
    #   
    
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
    # Calculates the inverse of a matrix 'x'
    #     
    # Args: 
    #   x:  object created by makeCacheMatrix()
    #         
    # Returns:
    #   the cached inverse of a matrix unless the object is null
    #       
    ## Return a matrix that is the inverse of 'x'
    
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
