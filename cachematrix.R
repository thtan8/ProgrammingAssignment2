## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
	
	# To initialize the inverse property
	g <- NULL
	
	# To set the matrix
	set <- function(matrix){
		m <<- matrix
		g <<- NULL
	}
	# To set the matrix
	get <- function(){
		m
	}
	# To set the inverse of the matrix
	setInverse <- function(inverse){
		g <<- inverse
	}
	# To get the inverse of the matrix
	getInverse <- function(){
		g
	}
	# Return a list of methods
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse of 
## the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Return the inverse if it is already set
        if (!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        
        ## Get the matrix from the object
        data <- x$get()
        
        ## Calculate the inverse
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Return the matrix
        m
        
}
