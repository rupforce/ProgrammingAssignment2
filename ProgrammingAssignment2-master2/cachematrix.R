## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special vector which contains a series of functions to set / get the matrix and the inversion
makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y){
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinversion <- function(inversion) iv <<- inversion
    getinversion <- function() iv
    list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}


## Write a short comment describing this function
# The cacheSolve function checks to see if the inversion has already been calculated, if not it will calculate the inversion.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    iv <- x$getinversion()
    if(!is.null(iv)){
        message('getting cached data')
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setinversion(iv)
    iv
}
