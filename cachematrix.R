## The pair of functions, makeCacheMatrix and cacheSolve,
## cache the inverse of a matrix

## Function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,        ## create list with methods for
         setinverse = setinverse,     ## get and set both the original
         getinverse = getinverse)     ## matrix and its inverse

}


## Function cacheSolve first checks if the inverse of the special
## "matrix" has already been calculated for the matrix that has not 
## been changed and retreives the inverse from the cache. Otherwise,
## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) { 
    inverse <- x$getinverse()
    if(!is.null(inverse)) {              
        message("getting cached data")
        return(inverse)         ## Return cached inverse of 'x'
    } 
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse) 
    inverse                 ## Return a matrix that is the inverse of 'x'
}
