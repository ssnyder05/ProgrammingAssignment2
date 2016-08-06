## The overall objective of this assignment is to demonstrate the concept
## of lexical scoping. Specifically, the functions in this script cache
## the inverse of a matrix.
##
## Sandra Snyder, Coursera Data Science Specialization, R Programming
##                Course, Programming Assignment 2, 6 August 2016.


#############################################################
#                makeCacheMatrix()                          #
#                                                           #
# Create a special matrix object that can cache its inverse #
#############################################################
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize inverse matrix
    inv <- matrix()
    
    # Method to set CacheMatrix info
    set <- function(y) {
        x <<- y
        inv <<- matrix()
    }
    
    # Method to get CacheMatrix info
    get <- function() {
        x
    }
    
    # Method to set the inverse matrix
    setInv <- function(inverse) {
        inv <<- inverse
    }
    
    # Method to get the inverse matrix
    getInv <- function() {
        inv
    }
    
    # Return a makeCacheMatrix object
    list(set=set, get=get, setInv=setInv, getInv=getInv)
    
} # end makeCacheMatrix()


################################################################
#                   cacheSolve()                               #
#                                                              #
# Returns the inverse of a matrix returned by makeCacheMatrix. #
# If the inverse has been previoiusly calculated and cached,   #
# cacheSolve() retrieves and returns that inverse. Otherwise,  #
# it computes and caches the inverse.                          #
################################################################
cacheSolve <- function(x, ...) {
    
    # Try to get a cached inverse matrix
    inv <- x$getInv()
    if (!all(is.na(inv))) {
        # Succcessful getting a cached inv
        message('Getting cached data')
        return(inv)
    }
    
    # Execute the rest of the function if we were unsuccessful
    #   at getting a cached inversion.
    # In this case, we need to get the matrix data, compute the 
    #   inverse, and cache it for possible future use.
    message('No cached data...computing inverse')
    data <- x$get()
    
    # A matrix is invertible if it is square and its determinant
    #   is non-zero
    tDim <- dim(data)
    # Make sure matrix is square
    if (tDim[1] == tDim[2]) {
        # Check determinant
        if (det(data) != 0) {
            inv <- solve(data)
            x$setInv(inv)
        } else {
            message('Matrix is singular (not invertible).')
        }
    } else {
        message('Matrix is not square (not invertible).')
    }
    
    # Return the inverse matrix
    inv
    
} # end cacheSolve()
