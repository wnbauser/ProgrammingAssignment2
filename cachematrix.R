## The following functions are used to cache the inverse value of a matrix.

## This function creates a new matrix object that sets the inverse value of a matrix and then gets the inverse value.

makeCacheMatrix <- function(x = matrix()) {
	
	## Initialize the inverse
	m <- NULL
	
	## Set the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## Get the matrix
	get <- function() x
	
	## Set the inverse of the matrix
	setInverse <- function(inverse) {
		m <<- inverse
	}
	
	## Get the inverse of the matrix
	getInverse <- function m
	
	## List the matrix objects
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## This function computes the inverse of the matrix established by the prior function.
## The function checks if the inverse has previously been calculated. If a solution already exists, and the matrix remains the same, then this functions skips the computation and returns the stored inverse value.
## If the inverse has not been calculated, then the function computes the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
	
	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	
	## If the inverse value is already set, return that value
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## Access the matrix
	data <- x$get()
	
	## Compute the matrix inverse value
	m <- solve(data, ...)
	
	## Set the inverse of the matrix
	x$setInverse(m)
	
	## Return the matrix
	m
}
