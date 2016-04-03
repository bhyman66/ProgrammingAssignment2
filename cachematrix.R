## =====================================================================
## cachematrix.R
## 
## Collection of functions that demonstrate lexical scoping features
## available in R. Satisfies the requirements of ProgrammingAssignment2 
## in the Coursera: R Programming course by JHU.
## =====================================================================

## Function: makeCacheMatrix()
## ---------------------------
## This function constructs a special matrix object that can invert
## its conetents and cache the result to speed inversion operations.

makeCacheMatrix <- function(x = matrix()) {

	# Initially set xInverse to NULL
	xInverse <- NULL

	# The set() function initializes the matrix x with the passed value
	# and initializes the xInverse to NULL. xInverse will be properly
	# calculated when the cacheSolve() function is called.
    set <- function(y) {
            x <<- y
            xInverse <<- NULL
    }

    # the get() function returns the current value of x.
    get <- function() x

	# the setsolve() function sets the cached xInverse variabl0e
    # the getsolve() function returns the local cached xInverse variabl0e
    setsolve <- function(solve) xInverse <<- solve
    getsolve <- function() xInverse

    # return the list of functions associated with the cachematrix object
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## Function: cacheSolve()
## ----------------------
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # Determine if the inverse matrrix has already been cached
    tmpSolve <- x$getsolve()
    if(!is.null(tmpSolve)) {

    	# xInverse has already been calculated and cached, so just return the 
    	# value of xInverse
        message("getting cached data")
        return(tmpSolve)
    }

    # xInverse is NULL, so solve() for x and stor result in xInverse
    data <- x$get()
    tmpSolve <- solve(data, ...)
    x$setsolve(tmpSolve)

    # return inverse matrix
    tmpSolve
}
