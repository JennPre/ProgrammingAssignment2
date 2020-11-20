## Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of matrix rather than compute it repeatedly.

## Assignment: Write a pair of functions that cache the inverse of a matrix 
## (makeCacheMatrix, cacheSolve)

## Remember:
## The operator '<<-' can be used to sign a value to an object in an environment that
## is different from the current environment.

## Function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {                            ## For setting the value of the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x                             ## For getting the the value of the matrix
        setinverse <- function(inverse) i <<- inverse   ## For setting the value of the inverse
        getinverse <- function() i                      ## For getting the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function cacheSolve
## Computes the inverse of the special "matriz" returned by makeCaheMatrix above. If 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {                               ## For checking if any value is NULL
                message("getting cached data")
                return(i)                               ## Returns the inverse value
        }
        data <- x$get()
        i <- inverse(data, ...)                         ## For calculating the inverse value
        x$setinverse(i)
        i
}
