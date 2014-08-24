## cachematrix.R
# Functions create a cached matrix object with additional functionality, 
# such as getting/setting the matrix and (if available) its inverse. 


# makeCacheMatrix will create a special object containing a matrix. 
# It takes one argument, a matrix. This input is coerced into a matrix.
# Inner functions set or get the matrix, and get or set its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    x <- as.matrix(x)
    inverse <- NULL
    
    set <- function(y) {
        x <<- as.matrix(y)
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve will, if necessary, calculate and cache the inverse of the matrix stored in an object created by makeCacheMatrix() 
# The first parameter is assumed to be an object created by makeCacheMatrix(). 

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}