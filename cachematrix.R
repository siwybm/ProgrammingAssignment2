## This function creates a list object which holds the functions for accessing the matrix object. 
# Following functions are available:
# set() - sets the matrix object
# get() - returns the matrix object
# setinverse() - sets the inverse of the matrix by passing inverse (in the form of matrix). 
#       Note, reasonably, this function should be used as an interface for the external function (i.e. cacheSolve). 
# getinverse() - returns the inverse of the matrix object

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function computes and returns the inverse of the matrix. Note, on input, this function takes the list object created by
# makeCacheMatrix function. In case the matrix (represented by the list object passed on input) does already have the inverse stored,
# then computation step is omitted. NULL is returned if cannot return the inverse.
# Additionally, althoug this is out of scope of this task, this function checks for preconditions 
# that must be satisfied in order to be able to invert the matrix, i.e. testing singularity and square of the input matrix object.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<- x$getinverse()
        if(!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        }
        data <- x$get()
        #Checking if matrix is not square
        if (ncol(data)!=nrow(data)){
                message ("Matrix is not square, thus not invertible."); 
                return(NULL)
        }      
        #Checking if matrix is not singular
        if (det(data)==0){
                message ("Determinant is equal to 0, thus matrix is not invertible."); 
                return(NULL)
        }       
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}


