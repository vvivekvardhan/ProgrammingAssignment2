# This code is for Matrix inversion : Programming assignment 2 Coursera
# Read.md consists of the Problem statement and instructions for this assignment

# Two functions has been declared in the below code. 
# (1) makeCacheMatrix() and (2) cacheSolve()

# makeCacheMatrix function is used to store the matrix and the cached value 
# of its inverse
# This function has 4 sub functions described below
# setM - Set value of the matrix
# getM - Get the value of the matrix
# setInv - Set the value of inverse of the matrix
# getInv - Get the value of inverse of the matrix

# cacheSolve function will calculates the inverse of the special matrix
# created by makeCacheMatrix and returns the output

# Solution: To demonstrate the function consider an example consider a 2X2 matrix 
# set v <- matrix(c(1, 2, 3, 4), c(2, 2))
# assign mcm <- makeCacheMatrix(v)
# cacheSolve(mcm) this will return the solved matrix below  
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
        
        # creating a variable to hold cached value or NULL

        cache <- NULL
        
        # store a matrix
        setM <- function(y) {
                x <<- y
                cache <<- NULL
        }

        # returns the stored matrix

        getM <- function() { x }

        setInv <- function(b) {  cache <<- b   }

        # retrieve the cached value
        
	getInv <- function() { cache }
        
        list(
		setM = setM,
		getM = getM,
		setInv = setInv,
		getInv = getInv
	    )
}


cacheSolve <- function(x, ...) {

        cache <- x$getInv()

        if(!is.null(cache)) {
                message("get cached data")
                return(cache)
        }

        d <- x$getM()
        cache <- solve(d, ...)
        x$setInv(cache)
        cache
}
