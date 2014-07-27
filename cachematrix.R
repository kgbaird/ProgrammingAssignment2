## These functions are designed to "cache" the inverse of matrix
## to (hopefully) reduce the time needed to make the time
## consuming computations

## Creates a special "matrix" object that can cache its inverse.
## Consists of a matrix object with additional defined functions
## to get and set inverse values

makeCacheMatrix <- function(x = matrix()) {
    mat.inv <- NULL
    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        mat.inv <<- NULL
    }
    ## Get the value of the matrix
    get <- function() x
    ## Set the value of the inverse
    setsolve <- function(solve) mat.inv <<- solve
    ## Get the value of the inverse
    getsolve <- function() mat.inv
    ## Return the matrix and the functions
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
    ## If getsolve does not return NULL, it means
    ## we have already have the value.  So, return
    ## it.
    if(!is.null(mat.inv)) {
        message("getting cached data")
        return(mat.inv)
    }
    ## Otherwise calculate it
    data <- x$get()
    mat.inv <- solve(data, ...)
    ## Update our cache to store it
    x$setsolve(mat.inv)
    ## return it to the user
    mat.inv
}
