## The first function makeCacheMatrix creates a list                 
## with a matrix; and a get and  set function for the matrix;          
## and a getsolve for getting the inverse of the supplied matrix   
##  (there is also setsolve bit this should not be directly called )
#####################################################################
## The second function cacheSolve is used to call the solve on the 
## matrix created in makeCacheMatrix and store for use in latter calls(cache it)


## Create special matrix with getter&setter for the matrix and the inverse of it
##
## following example call creates a list with a 3x3 matrix and set and get function
## and it is assigned to x
## x<-makeCacheMatrix(matrix(rexp(9,rate=1),3,3))

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve creates or retrieves the inverse matrix of the supplied matrix
## the supplierd matrix has to be created with makeCacheMatrix.
## First call to the function will return the inverse matrix 
## of x, next call to cache Solve results in the cached inverse matrix to be returned

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
    ## Return a matrix that is the inverse of 'x'
}
