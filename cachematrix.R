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
## Test the above.
##

testCaching <- function() {
    message("TESTING THE CACHING")
    message("-------------------")

    # make a "big" matrix, so that the computation time is noticeable with today's CPU
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
}

testInverse <-function() {
    message("TESTING THE INVERSION")
    message("---------------------")

    m <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
    message("Original matrix:")
    print(m)

    cm <- makeCacheMatrix(m)
    inverse <- cacheSolve(cm)
    message("Inverse matrix:")
    print(inverse)

    shouldBe <- matrix(c(-24, 20, -5, 18, -15, 4, 5, -4, 1), 3, 3)
    difference <- inverse -shouldBe
    message("Difference:")
    print(difference) # hopefully small

    message("Equal (under tolerance):")
    print(all.equal(inverse, shouldBe)) # hopefully TRUE
}


test <- function() {
    testCaching()
    testInverse()
}
