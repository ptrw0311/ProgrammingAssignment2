## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverseMatrix
## 4. get the value of the inverseMatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        }
        get <- function() x 
        setinversematrix <- function(inverse) m <<- inverse 
        getinversematrix <- function() m 
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)

}

## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has 
## already been caclulated. If so, it gets the inversematrix from the cache
## and skips the computation. Otherwise, it calculates the inverseMatrix and 
## sets the value of the inverseMatrix in the cache via the 'setinversematrix' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}
