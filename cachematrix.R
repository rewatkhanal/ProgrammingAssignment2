## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list which contains a function that
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   # the result of inversion is stored here
    set <- function(y){
        x <<- y
        inv <<- NULL   # it also initialises inv to null
    }
    get <- function() x   # it returns the input matrix
    setinverse <- function(inverse) inv <<- inverse  # it sets the inversed matrix
    getinverse <- function() inv  # it returns the inversed matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    # it returns a list which contains the functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()  # it gets the inversed matrix from the object x
    # it will be null if uncalculated, according to the first line "inv <- NULL" in the previous function
    if(!is.null(inv)) {
        # if the inversion result is already stored
        message("getting cached data")
        return(inv)
        # it returns the calculated inversion
    }
    # otherwise the inverse of the matrix is calculated
    data <- x$get()   
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv  # it returns the solved matrix
}
