## ---------------------------------------------------------------------------
## R Programming: Coursera programming assignment number 2
##
## Work of Pascal Bauermeister - September 2017
##
## ---------------------------------------------------------------------------
## Goal of the assignment: matrix inversion, with caching of results,
## to have only the first access consuming CPU. Closure/environment
## allow us to do the caching.
## ---------------------------------------------------------------------------


## Factory function returning, for a given matrix, a list object
## containing the matrix, its cached inverse, and access functions.
## No computation here, only caching.

makeCacheMatrix <- function(x = matrix()) {
    # data, additionally to x
    inverse <- NULL
        
    # setters (note: as we do not provide a setter for x, the original
    # matrix cannot change)
    setInverse <- function(m) inverse <<- m
    
    # getters
    get <- function() x
    getInverse <- function() inverse
    
    # bindings    
    list(get=get, setInverse=setInverse, getInverse=getInverse)
}


## Return the inverse of a cached matrix built by calling
## makeCacheMatrix().  If the inverse has been previously computed and
## cached, return the cached value, else solve the matrix and cache
## it.

cacheSolve <- function(x, ...) {
    result <- x$getInverse() # maybe cached
    if (!is.null(result)) {
        message("  Getting cached data")
    } else {
        message("  Computing and caching inverse")
        result <- solve(x$get())
        x$setInverse(result)
    }
    result
}


##
## Test the above with a big matrix.
##

test <- function() {
    size <- 1500
    m <- matrix(sample.int(100, size*size, replace=TRUE), size, size)
    message("Original matrix")
    print(m)

    # access the inverse, and display benchmark
    doit <- function() {
        before <- Sys.time()
        
        inverse <- cacheSolve(cm)
        
        now <- Sys.time()
        message("  Time: ", now-before)
        message()
    }

    # fresh call
    cm <- makeCacheMatrix(m)
    message("1st call")
    doit()
    
    # cached call
    message("2nd call")
    doit()
    
    # cached call
    message("3rd call")
    doit()

    # fresh call again
    cm <- makeCacheMatrix(m)
    message("1st call, recreated cached matrix")
    doit()
    
    # cached call
    message("2nd call")
    doit()
    
    # cached call
    message("3rd call")
    doit()
    
    "Test done"
}
