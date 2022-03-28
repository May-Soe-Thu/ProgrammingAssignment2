## Put comments here that give an overall description of what your
## functions do

## This is to make a function called makeCacheMatrix and its function is to create a special "matrix" object that can cache its inverse.
## 1. create a function of a matrix.
## 2. set the inverse for the function.
## 3. if the input is matrix, we get the function.
## 4. then, use the inverse function to set an inverse of the matrix.
## 5. get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This is to compute the inverse of a square matrix using the solve function in R
## Get the cached data from previous function if the inverse is not a NULL value.
## Return the inverse of 'x' in the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
