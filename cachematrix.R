##
## The functions in the cachematrix.R are a set of routines for creating and interacting with a matrix
## whose inverse is calculated and stored on first call. Subsequent calls returns a stored/cached copy
## of the inverse without recalculation. 
##
## The routines in this file are:
##    makeCacheMatrix(): Creates a cachable matrix and reurns a list of functions for 
##                       - creating and reading a matrix whose inverse will be cached
##                       - calculating and reading the inverse of the cached matrix (for internal use)
##    cacheSolve(): Calculate and cache the inverse of matrix if it does nor exist else return the 
##                  cached inverse if the inverse was previously calculated and cached.


makeCacheMatrix <- function(x = matrix()) {
    # makeCacheMatrix() creates a matrix whose inverse is cached whenever computed. The function 
    # makeCacheMatrix() can create an unpopulated or a populated matrix. The values in a matrix can 
    # be updated with the function ExampleMatrix$set(...) and the values of a matrix retrieved with the 
    # function ExampleMatrix$get() 
    #
    # Args:
    #   x: Square matrix
    #
    # Returns:
    #   A list wih the functions set(), get(), setInverse(), getInverse()
    #
    # Usage:
    #   ExampleMatrix <- makeCacheMatrix()                  Creates an empty matrix
    #   ExampleMatrix$set(matrix(......)))                  To set the values of empty ExampleMatrix
    # or
    #   ExampleMatrix <- makeCacheMatrix(matrix(.......))   Creates a populated matrix
    #
    #   ExampleMatrix$get()                                 Retrieves matrix values
    #   Retrieve inverse with cacheSolve(ExampleMatrix)
    #
    #   ExampleMatrix$setinverse()                          For internal use. Calculate matrix inverse
    #   ExampleMatrix$getinverse()                          For interbal use. Retrieves inverse
    #
    #
    
    Inverse_x <- NULL                   # Storage space for inverse of x
    set <- function(y) {
        # Set matrix values and set 'Inverse_x' to NULL to ensure
        # inverse recalculation
        x <<- y
        Inverse_x <<- NULL
    }
    
    get <- function() x                 # Retrieve matrix
    
    setInverse <- function(Mat) Inverse_x <<- Mat
    getInverse <- function() Inverse_x
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
   

}


cacheSolve <- function(x, ...) {
    # Returns the inverse of the matrix 'x'. The first cacheSolve() call for a particular argument will
    # result in the matrix inverse being calculed while futher calls will result in a cached copy of the
    # matrix being returned
    #
    # Args:
    #   x: Matrix created with makeCacheMatrix
    #
    # Returns:
    #   Matrix inverse of the argument 'x'
    #
    # Usage:
    #   InverseMatrix <- cacheSolve(ExampleMatrix)
    #
    
    Inverse_x <- x$getInverse()         # Get the cached inverse if it exists
    if(!is.null(Inverse_x)) {
        # Return the previously cached copy of the inverse
        message("getting cached inverse")
        return(Inverse_x)
    }
    Inverse_x <- solve(x$get(), ...)    # Calculate the inverse
    x$setInverse(Inverse_x)             # Cache the inverse    
    Inverse_x
    
}
