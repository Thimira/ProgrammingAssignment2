## These functions would allow generating matrix objects of which the inverse 
## values can be cached. This would increase the processing efficiency in a 
## scenario where the inverse needs to be repeatedly calculated

## makeCacheMatrix - This function creates a special matrix object with a 
## cacheable inverse value. The object is represented as a list, with get and 
## set functions for both the matrix value and the inverse.

makeCacheMatrix <- function(x = matrix()) {
        cinverse <- NULL
        
        set <- function(y) {
                x <<- y
                cinverse <<- NULL ## clear the cache value 
                                  ##    when the matrix is changed
        }
        
        get <- function() {
                x
        }
        
        setinverse <- function(inverse) {
                cinverse <<- inverse
        }
        
        getinverse <- function() {
                cinverse
        }
        
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve - This function calculates the inverse of a matrix object created 
## by the makeCacheMatrix function. It will check for cached inverse value, and 
## will calculate and cache if not already cached. Additional parameters to the 
## matrix solve function can be passed if required.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if (!is.null(inverse)) { ## ceck if the inverse value is cached
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...) ## calculate the inverse of the matrix
        x$setinverse(inverse)
        inverse
}
