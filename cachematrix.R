## These two functions implement a method for caching the calculation
## of the inverse of a matrix

## This function provides a set of 4 auxiliary functions
## needed for the caching system: setting (1) and retrieving (2) the matrix
## that is being inverted, and setting (3) and retrieving (4) the inverted
## matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL			## flag saying that no cached inverted matrix is 
					## available yet, since the caching system is
					## about to be set up for the first time

	set <- function(y) {
		x <<- y
		inv <<- NULL	## flag saying that any cached inverted matrix 
					## should not be used, since the matrix to be 
					## inverted has changed 
	}
	get <- function() x
	setinv <- function(inverted) inv <<- inverted
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverted matrix of the matrix
## given as an argument, using cached results whenever possible,
## and calculating the inverse from scratch when not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}


