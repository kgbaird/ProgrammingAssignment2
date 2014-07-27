## These functions are designed to "cache" the inverse of matrix
## to (hopefully) reduce the time needed to make the time
## consuming computations

## Creates a special "matrix" object that can cache its inverse.
## Consists of a matrix object with additional defined functions
## to get and set inverse values

makeCacheMatrix <- function(x = matrix()) {
    mat.inv <- NULL
    set <- function(y) {
        x <<- y
        mat.inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) mat.inv <<- solve
    getsolve <- function() mat.inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Actually does the computation of the special matrix inverse
## created by makeCacheMatrix.  Will check to see if the inverse
## has already been computed and if so return the solved value.
## If not, will compute the inverse.

cacheSolve <- function(x, ...) {
    mat.inv <- x$getsolve()
    if(!is.null(mat.inv)) {
        message("getting cached data")
        return(mat.inv)
    }
    data <- x$get()
    mat.inv <- solve(data, ...)
    x$setsolve(mat.inv)
    mat.inv
}
