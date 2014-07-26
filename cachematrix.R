## These functions are designed to "cache" the inverse of matrix
## to (hopefully) reduce the time needed to make the time
## consuming computations

## Creates a special "matrix" object that can cache its inverse.
## Consists of a matrix object with additional defined functions
## to get and set inverse values

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## Actually does the computation of the special matrix inverse
## created by makeCacheMatrix.  Will check to see if the inverse
## has already been computed and if so return the solved value.
## If not, will compute the inverse.

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
