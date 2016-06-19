## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse. Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly.
## makeCacheMatrix has a single argument, which is an invertible matrix x.
## It will return a list containing four functions used to access and change
## the stored data:

## 1. set:              set the value of the matrix.
## 2. get:              get the value of the matrix.
## 3. setinverse:       set the inverse matrix.
## 4. getinverse:       get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        # Variable that stores the inverse matrix:
        inv <- NULL
        
        # Functions to set and get the stored matrix:
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        # Functions to set and get the inverse matrix:
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        # List containing the four functions previously defined:
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        
}


## The cacheSolve function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of 
## the matrix and sets its value in the cache via the setinverse function.
## cacheSolve has a single argument, which is a special "matrix" object created
## by the makeCacheMatrix function. It will return the value of the inverse 
## matrix after being saved in the cache.

cacheSolve <- function(x, ...) {
        
        # Get the current value of the inverse matrix:
        inverse <- x$getinverse()
        
        # If the value is different than NULL, it means that it has already 
        # been calculated. In this case,  we use the cached version:
        if(!is.null(inverse)) {
                message("we use the cached data")
                return(inverse)
        }
        
        # Read the original matrix:
        matrix <- x$get()
        
        # Calculate the inverse matrix:
        inverse <- solve(matrix, ...)
        
        # Store the inverse matrix:
        x$setinverse(inverse)
        
        # Return the inverse matrix:
        inverse
        
}
