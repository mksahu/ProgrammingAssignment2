## There are two functions (makeCacheMatrix and cacheSolve) in this
## program file that are used to create a special object that stores
## a matrix and cache's it inverse.


## The function makeCacheMatrix creates a special "matrix", which is
## a list containing a function to
##  (1) set the value of the matrix
##  (2) get the value of the matrix
##  (3) set the value of the inverse matrix
##  (4) get the value of the inverse matrix
## It takes a matrix as input and creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        
        ## Set/update the cache if 'x' is not same as 'y' 
        set <- function(y) {
                if (!identical(x,y)) {
                        ## x' is not same as 'y'
                        x <<- y
                        invX <<- NULL
                }
        }
        ## Get the matrix 'x'
        get <- function() x
        
        ## Set the inverse of matrix 'x'
        setinverse <- function(solve) invX <<- solve

        ## Get the inverse of matrix 'x'
        getinverse <- function() invX
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns an empty matrix if 'x' is empty matrix
        data <- x$get()
        if(identical(data, matrix())) {
                return(matrix())
        }
        
        invX <- x$getinverse()
        ## Checks if inverse of 'x' is present in cache or not
        if(!is.null(invX)) {
                ## Inverse of 'x' is present in cache
                message("getting cached data")
                return(invX)
        }

        ## Inverse of 'x' is not present in cache
        ## Following lines will compute matrix inverse then store it in cache
        invX <- solve(data, ...)
        x$setinverse(invX)
        invX
}
