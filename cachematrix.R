## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix" which is a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invMat <<- inverse
    getInverse <- function() invMat
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "vector"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache via 
## the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInverse()
    if (!is.null(invMat)) {
        message("getting cache data")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(data, ...)
    x$setInverse(invMat)
    invMat
}
