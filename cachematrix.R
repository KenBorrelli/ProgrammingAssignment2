## Put comments here that give an overall description of what your
## functions do


## Create a psuedotype augmenting a matrix by enabling it to have a cached inverse
## This is designed to be used in conjunction with cacheSolve to return the
## value of that cached matrix.  
makeCacheMatrix <- function(x = matrix()) {
    # This funciton should be passed a matrix and wil return the cachematrix
    # psuedo-type
    inverse = NULL # Store the inversion of the matrix
    set = function(new_matrix){
        # This is a public function designed to allow the matrix to be
        # changed.  Note that changing the matrix requires a new
        # inversion to be calculated
        x <<- new_matrix
        inverse <<- NULL # Reset inverse if matrix changes
    }
    get = function(){
        # Public function that returns the matrix.  This should be used
        # for all operations on the matrix other than inversion
        x # Return the matrix 
    }
    set_inverse = function(in_inverse){
        # This is a private function that should only be used by cacheSolve
        # It returns the value of the stored inverse or Null if none exists
        inverse <<- in_inverse
    }
    get_inverse = function(){
        # Private function used only by cacheSolve
        # Sets the cached value of the matrix inverse
        inverse
    }
    # Return the set of functions needed to access this
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

## Get the inverse of a cachematrix psuedo-type created with makeCacheMatrix
## The output is a simple matrix and not the makeCacheMatrix psuedo type
cacheSolve <- function(x, ...) {
    inverse = x$get_inverse()
    if(is.null(inverse)){
        # Cached copy does not exist
        message("calculating inverse data")
        data = x$get()
        inverse = solve(data, ...)
        x$set_inverse(inverse)
    } else {
        # Cached copy does exist
        message("getting cached data")
    }
    ## Return a matrix that is the inverse of 'x'
    inverse
}
