## The following functions are used in order to cache the inverse of an input matrix.



## The function "makeCacheMatrix" creates a special "matrix" object that 
## can cache its inverse. Its input is a matrix (or, is coerced into a
## matrix), while it returns a list with four functions in order to set/get
## the input matrix, as well as its inverse.


makeCacheMatrix <- function(x = matrix()) {
                ## Creates a special "matrix" object that makes the inverse 
				## of a matrix cache-able.

				
                # Initialize the free variable that will be used to hold
                # the inverse of the matrix to NULL.
                i <- NULL
                
                # Define a function that assigns its input as "x" and initializes
                # the free variable, i, to NULL. Used to set the input matrix.
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                
                # Define a function that returns the content of the free variable x.
                # Used to return the cached input matrix.
                get <- function() x
                
                # Define a function that assigns the inverse of the input matrix
                # to i.
                setinverse <- function(inverse) i <<- inverse
                
                # Define a functions that returns the contents of the free variable i.
                # Used to return the cached inverse matrix.
                getinverse <- function() i
                
                # Return a list with the four previous functions 
                list(set = set, get = get,
                        setinverse = setinverse,
                        getinverse = getinverse)
}




## The function "cacheSolve" computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.


cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
        
                # Get the inverse of the input matrix.
                i <- x$getinverse()
                
                # If the inverse of the input matrix isn't NULL (thus, it's already
                # cached, print out a message and return the cached inverse.
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }

                # Since the inverse is not cached, assign the input matrix to
                # the free variable "data".
                data <- x$get()
                
                # Use the "solve" function in order to calculate the inverse
                # of "data" matrix.
                i <- solve(data, ...)
                
                # Cache the inverse, so there is no need to re-calculate it in
                # the future.
                x$setinverse(i)
                
                # Return the inverse
                i

}
