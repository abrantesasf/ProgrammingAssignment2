#===============================================================================
# This file is my response to the Programming Assignment of week 3 of the R
# Programming course on Coursera
# (https://www.coursera.org/learn/r-programming/home/welcome).
#
# Student: Abrantes Araújo Silva Filho
#          abrantesasf@gmail.com
#          2017-11-27
#
# OBJECTIVE:
# To write an R function that is able to cache the inverse of a matrix rather
# than compute it repeatedly (if the matrix does not change, the inverse
# could be looked up in the cache rather than recomputed). We must use the
# two alredy supplied functions templates: makeCacheMatrix and cacheSolve.
#
# SOLUTION:
# Two functions were create for this task: makeCacheMatrix and cacheSolve.
# This functions must be used together as the example below:
#    m <- matrix(1:4, ncol=2)
#    ma <- makeCacheMatrix(m)
#    cacheSolve(ma)             # calculate the inverse matrix
#    cacheSolve(ma)             # get the cached inverse matrix
#===============================================================================


#===============================================================================
# Function: makeCacheMatrix
#
# Description: this function creates a list with 4 functions:
#   1) setMatriz: set the value of the matrix
#   2) getMatriz: get the value of the matrix
#   3) setMatrizInversa: set the inverse of the matrix
#   4) getMatrizInversa: get the inverse of the matrix
#
# Inputs: A inversible matrix
#
# Outputs: A list with 4 functions
#
# Assumptions: -
#
# Internals: Note the use of <<- operator
#===============================================================================
makeCacheMatrix <- function(x = matrix()) {
    # Initializate object im (Inverse Matrix) as NULL,
    # in every run of function, to monitorate matrix changes
    im <- NULL

    # Function to set the value of the matrix
    # (not strictly necessary in this context, but keep it anyway)
    setMatriz <- function(y) {
        # Set the value of the matrix
        x <<- y
        # Set im (inverse matrix) as NULL
        im <<- NULL
    }

    # Function to get the value of the matrix
    getMatriz <- function() {
        # Just return matriz x:
        x
    }

    # Function to set the inverse matrix
    setMatrizInversa <- function(solve) {
        im <<- solve
    }

    # Function to get the inverse matrix
    getMatrizInversa <- function() {
        # Just return the inverse matrix:
        im
    }

    # Return the list of 4 functions:
    list(setMatriz = setMatriz,
         getMatriz = getMatriz,
         setMatrizInversa = setMatrizInversa,
         getMatrizInversa = getMatrizInversa)
}




#===============================================================================
# Function: cacheSolve
#
# Description: this function takes the special list created by makeCacheMatrix()
#              and return the cached inverse matrix or calculates the inverse
#
# Inputs: A special list object created by makeCacheMatrix()
#
# Outputs: The inverse matrix
#
# Assumptions: -
#
# Internals: -
#===============================================================================
cacheSolve <- function(x, ...) {
    # Get the inverse matrix:
    im <- x$getMatrizInversa()
    
    # Test to see if the inverse matrix alredy exists in cache:
    if(!is.null(im)) {
        # im exists, just return it!
        message("Cached Inverse Matrix exists, getting data...")
        return(im)
    } else {
        # im NOT exists, so we must calculate it.
        # First, we get the original Matrix:
        data <- x$getMatriz()
        # Now we compute the inverse matrix using R solve function:
        im <- solve(data, ...)
        # Now we put the inverse matrix on cache:
        x$setMatrizInversa(im)
        # And finally, we return the inversed matrix:
        return(im)
    }
}
