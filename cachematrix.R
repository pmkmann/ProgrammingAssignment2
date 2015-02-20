## Coursera - Data Scientist Track
## Course: R-Programming
## Week: 3
## Assignment: 2
## Date: 19-February-2015
## Author: Paul Kaufmann
##
## ----------------------------------------
##
## Note: The requirements of the assignment have been implemented in a single
## function: makeCacheMatrix. This differs from the stubs provided which has the matrix
## being solve and cached in a separate function. Here the solve/cache function is done
## in the specical matrix object whenever the matrix is set. In this way, the matrix that
## that is cached will never be out of step with the cached inverese.
##
## I feel this approach is a better implementation. It also more closely resembles object-oriented
## programming in that the special matrix object holds (encapsulates) all the functions (methods) 
## that pertain to it.


makeCacheMatrix <- function(x = matrix()) {
    
    ## The purpose of this function is to create a list object containing functions that:
    ##
    ## 1. Set/cache the matrix object. Solve and cache its inverse.
    ## 2. Get the matrix object.
    ## 3. Get the matrix inverse.
    ##
    ## Whenever the matrix object is set, including on initial creation of this object,
    ## the inverse is calculated and cached. Whenever the inverse matrix is requested, it is returned
    ## from the cache.
    
    minv <<- NULL # Initialize the matrix inverse to null.
    
    # Define function to set the matrix.
    set <- function(y) {
        x <<- y
        minv <<- NULL
        
        # Compute and cache the inverse. Always compute the inverse whenever the matrix is set.
        # This way, the matrix and its inverse will never be out of sync.
        minv <<- cacheSolve(x)
    }
    
    # Define function to get the matrix.
    get <- function() x
    
    
    # Define function to get the inverse (which is a simple retreival of the cached inverese)
    getinv <- function(...) {
        
        if(is.null(minv)) {
            message("no inverse found.")
        }
 
        return(minv)
    }
    
    # Define internal function to find the inverse.
    cacheSolve <- function(x, ...) {

        minv <<- solve(x)
    }
    
    # Set the matrix inverse by calling set(x)
    set(x)
    
    # Finally, return the list of exposed functions.
    list (set = set, get = get, getinv = getinv)
}

