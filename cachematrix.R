##
## These functions establish the facility to calculate the inverse of a matrix
##  and store the results, returning the "cached" results on subsequent calls
##  allowing for a single "inversion" action to be performed once and then
##  the results when needed again.
##
##  The R function "solve" is used to perform tha matrix inversion
##
##  ex: 
##   i <- matrix(c(2/3,-1/3, 1/3, 1/3), nrow = 2, ncol = 2, byrow = TRUE,
##               dimnames = list(c("row1", "row2"),
##                               c("C.1", "C.2")))
##   i2 <- makeCacheMatrix(i)
##   cacheSolve(i2)
##

 #
 # The makeCacheMatrix function creates a vector of functions to support caching 
 #   and retrieving the result of a matrix inversion. 
 #
 # Call as: makeCacheMatrix(x) where x is a valid, invertible matrix
 # (makeCacheMatrix does not test for validity) 
 #
 #

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


 #
 # This function returns an inverted matrix, first testing to see 
 #   if it exists in memory (has been cached) it returns this object
 #   if not - it solves (inverts) the matrix and caches the results
 #
 # Call as: cacheSolve(x) where x is a valid, invertible matrix
 # (cacheSolve does not test for validity) 
 #

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}


