## Pair of functions makeCahceMatrix and CacheSolve that cache the inverse of
## a matrix

## makeCacheMatrix function creates a special matrix object that can cache its
## inverse. InvMatrix variable is the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    InvMatrix <- NULL
    set <- function(y){
        x <<- y
        InvMatrix <<- NULL
    }
    get <- function() x
    SetInverse <- function(Inverse) InvMatrix <<- Inverse
    GetInverse <- function(Inverse) InvMatrix
    list(set = set, get= get, SetInverse = SetInverse, GetInverse = GetInverse)
}


## CacheSolve function computes the inverse of the matrix returned by 
## makeCacheMatrix. If the inverse of the matrix has already been calculate, 
## CacheSolve gets the matrix from cache.

cacheSolve <- function(x, ...) {
        InvMatrix <- x$GetInverse()
        if(!is.null(InvMatrix)) {
            message("getting cached data")
            return(InvMatrix)
        } else {
        data <- x$get()
        InvMatrix <- solve(data, ...)
        x$SetInverse(InvMatrix)
        InvMatrix
        }
}
