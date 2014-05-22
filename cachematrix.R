

## This is a pair of functions that cache the inverse of matrix
## The two functions are "makeCacheMatrix" and "cacheSolve"


## This function creates a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## create a special matrix returning a list of functions that 
        ## sets and gets the matrix value and its inverse


        # initialize cachedInverse object to hold the inverse of matrix "x"
        cachedInverse <- NULL
        
        # define a set function to provide an option to change the matrix data
        # since the matrix has changed, re-initialize the cachedInverse object 
        set <- function(xdata) {
                x <<- xdata
                cachedInverse <<- NULL
        }
        
        # define a get function to get the current data of matrix "x"
        get <- function() x

        # define "setinverse" function to save the inverse matrix data to cache
        setinverse <- function(invx) cachedInverse <<- invx
        
        # define "getinverse" function to get the cached inverse matrix data
        getinverse <- function() cachedInverse
        
        # return the list of the functions
        list ( set=set ,
               get=get ,
               setinverse=setinverse,
               getinverse=getinverse)

}


## This function computes the inverse of matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # define object "invx" that will hold the inverse of the matrix "x"
        # initially, retrieve the inverse from the cache by getinverse function 

        invx <- x$getinverse()

        # if "invx" is not null, just return the cached inverse matrix data
        if(!is.null(invx)) {
                message("data taken from cached inverse matrix")
                return(invx)
        }
        
        # otherwise (invx is null), calculate the inverse of the matrix
        
        # get the matrix data (store to "xdata")
        xdata <- x$get() 
        
        # calculate the inverse of the matrix "invx" using the solve function
        invx <- solve(xdata, ...)

        # save the inverse of the matrix to cache using setinverse function
        x$setinverse(invx)
        
        # return the inverse of the matrix
        invx 

}
