## this file contains two functions, makeCacheMatrix() and cacheSolve(). These 
## two functions are aimed to cache and calculate the inverse of a matrix. 

## The makeCacheMarix() is created as a list to cache inverses. It has four 
## elements, set, get ,setmatrix and getmatrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) {
                s <<- solve
        }
        getmatrix <- function() s
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## The cacheSolve() is created to computes the inverse of a matrix returned by 
## makeCacheMatrix() above. If the inverse has already existed, then this function 
## will retrieve the inverse from the cache in the parent frame.

cacheSolve <- function(x, ...) {
        s <- x$getmatrix()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        t <- x$get()
        s <- solve(t, ...)
        x$setmatrix(s)
        s
}