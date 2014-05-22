##The below two functions allow R to chache the inverse of a Matrix.

## This function creates a matrix that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	
	# m will store the cached inverse matrix
    	m <- NULL

    	# Setter for the matrix
    	set <- function(y) {
        	x <<- y
        	m <<- NULL
    		}
    	
	# Getter for the matrix
    	get <- function() x

    	# Setter for the inverse
    	setinv <- function(inverse) m <<- inverse
    
	# Getter for the inverse
    	getinv <- function() m

    	# Return the matrix with our newly defined functions
    	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of the first function.  
## If the inverse has been calculated it will return the inverse from the cache.

cacheSolve <- function(x, ...) {
	
	m <- x$getinv()

    	# If the inverse is already calculated, return it
    	if (!is.null(m)) {
        	message("getting cached data")
        	return(m)
    		}

    	# If it isn't calculated, calculate it
    	data <- x$get()
    	m <- solve(data, ...)

    	# Cache the inverse
    	x$setinv(m)

    	# Return it
    	m
}
