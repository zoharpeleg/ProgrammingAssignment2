## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to
## 1.   set the value of the matrix
## 2.   get the value of the matrix
## 3.   set the value of the inverse matrix
## 4.   get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        set <- function(y) {
                m <<- y
                s <<- NULL
        }
        get <- function() m
        setinv <- function(inv) s <<- inv
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above, by using the "solve" function
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve would retrieve the inverse matrix from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
