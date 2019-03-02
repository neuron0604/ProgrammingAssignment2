#
# Course: R_Programming.Week3_Assignment_2
# File: cachematrix.R
# Author: Joseph L R
# Date: 03/01/2019
#
# Description:
# This file contains R functions to manage cached matrix inversions.
# Two functions are provided:
# a) makeCacheMatrix() - This creates a cacheable inverted matrix.
# b) cacheSolve() - Given a matrix this returns the inverted matrix,
#    retrieving the inverted matrix from the cache as needed.


## Write a short comment describing this function
# Given a square invertible matrix, this function caches the matrix.
# It exports
# get - return the source matrix
# set - set input as source matrix
# getInverse - return the cached inverted matrix
# setInverse - set the inverted matrix to the given input value.

makeCacheMatrix <- function(inpMatrix = matrix()) {
    matrixInv <- NULL
    set <- function(y) {
        inpMatrix <<- y
        matrixInv <<- NULL
    }

    get <- function() inpMatrix
    setinverse <- function(minverse) matrixInv <<- minverse
    getinverse <- function() matrixInv

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Given a matrix this function computes its inverse and returns the result.
# It caches the inverse of the matrix so that subsequent calls return the cached value.
# The input must be a cached matrix generated using makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached matrix inverse data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
