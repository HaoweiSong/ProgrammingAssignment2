#<<- superassignment operator, something like the global variables in C/C++
## creates a special "matrix" object that can cache its inverse

## Generate a vector of fucntions to do:
## set(matrix), set the value of the original matrix, also use to initialize the fucntion
## get(), get  the value of the matrix
## setinverse(matrix) the value of the inversed matrix
## getinverse(), get the value of the inversed matrix



makeCacheMatrix <- function(x = matrix()) {
    #inversed_x is the cache of inversed matrix of x
    inversed_x <- NULL
    set <- function(y) {
        x <<- y
        inversed_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inversed) inversed_x <<- inversed
    getinverse <- function() inversed_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#Test if the matrix is inveresed or not. If it is return it, it is not inverse and cache it
#the argument x is the vector of functions that generated from "makeCacheMatrix"
cacheSolve <- function(x, ...) {
    inversed_x <- x$getinverse()
    if(!is.null(inversed_x)) {
        message("getting inversed matrix")
        return(inversed_x)
    }
    data <- x$get()
    inversed_x <- solve(data, ...)
    x$setinverse(inversed_x)
    inversed_x
}
