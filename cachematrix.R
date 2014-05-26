## This file includes two functions. First one (makeCacheMatrix) creates special object containing matrix  
## and associated functions which can cache the results so that processing power and time can be saved
## when inverse of matrix is calculated repeatedly. Second function (cacheSolve) calls first functions to
## check if cached copy of inverse matrix is available, otherwise it computes inverse and passes copy of
## inverse matrix to 1st function which sets it so that cached copy can be used in future.

## How to use the functions
## testMatrix <-matrix(c(3,1,2,1),nrow=2,ncol=2)
## testCacheMatrix<-makeCacheMatrix(testMatrix)
## cacheSolve(testCacheMatrix)

## Creates matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
		# m (used to save cached matrix) is initalized to NULL 
        m <- NULL
        # set function sets value of matrix (x)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # returns original matrix (x)
        get <- function() x
        # sets cached copy of inverse of matrix (using operator <<-)
        setinverse <- function(inverse) m <<- inverse
        # gets value of inverse matrix. Result is NULL if inverse's cached copy is not processed even once
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## Returns inverse of special matrix object (created using makeCacheMatrix function).
## Function uses cached copy of inverse matrix if available
cacheSolve <- function(x, ...) {
		# getInverse of matrix provided in the arguments
		# x is the matrix that is generated using makeCacheMatrix
        m <- x$getinverse()
        # Checks for cached copy and returns if available
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # If cached copy of inverse matrix is not available (checked in previous step), then value of original matrix - "x" is saved in "data"
        # and inverse matrix is computed using solve function
        data <- x$get()
        m <- solve(data, ...)
        # cached copy of inverse matrix is saved and can be use next time function is called
        x$setinverse(m)
        m
}