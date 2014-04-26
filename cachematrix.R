## The two functions below cache the inverse of a matrix so that when we 
## need it again, it can be looked up in the cache rather than recomputed.
 

## The first function, makeCacheMatrix, creates a special "matrix" object 
## that can cache its inverse. It creates a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y      
		    # note the <<- operator assigns a value to an object in an 
		    #	environment that is different to the current environment.
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.


cacheSolve <- function(x, ...) {
      

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) 
	  # the solve function computes the inverse of a square matrix.
        x$setinverse(m)
        m
}