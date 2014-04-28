## The following functions "makeCacheMatrix" and "cacheSolve", cache the
## inverse of a given matrix, since matrix inversion can be a costly
## computation.
##
## Assuption: the following two functions assume that the matrix that we
##            are given is a square invertible matrix.



## "MakeCacheMatrix" returns a special object, which is in reality a list
## with a function that:
## a) sets the value of a given matrix
## b) gets the value of the matrix
## c) sets the value of the inverse of the matrix
## d) gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    set(x)
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## "cacheSolve" first checks to see if the inverse of the special matrix
## created by "makeCacheMatrix" has been already been computed. If the 
## inverse has already been calculated, it returns the value of "i"
## that has been stored in the global environment -- otherwise, it
## computes and returns the value.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
