## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix below creates a list containing 4 functions:
## setm -> setm set the values of the matrix, getm gets the values of te matrix
## setinverse set the values of inverse of the matrix
## getinverse get the values od the inverso of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setm <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getm <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(setm=setm, getm=getm, setinverse=setinverse, getinverse=getinverse)
}

## The function cacheSolve returns the inverse of the  matrix returned by `makeCacheMatrix` above 
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve`
## retrieves the inverse from the cache and the message "cached data retrieved."
## If not, it computes the inverse, sets the value in the cache and shows the result.
## Assumption : the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("cached data retrieved.")
      return(inv)
    }
    data <- x$getm()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}



## Example:

##> m <- makeCacheMatrix(rbind(c(1, -1/2), c(-1/2, 1)))

## m$getm()
##    [,1] [,2]
##[1,]  1.0 -0.5
##[2,] -0.5  1.0

##> cacheSolve(m)
##          [,1]      [,2]
##[1,] 1.3333333 0.6666667
##[2,] 0.6666667 1.3333333

##> cacheSolve(m)
##cached data retrieved. 
##          [,1]      [,2]
##[1,] 1.3333333 0.6666667
##[2,] 0.6666667 1.3333333
