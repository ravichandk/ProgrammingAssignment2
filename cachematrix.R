## Functions that cache the inverse of a matrix
##
## Usage example:
##
## > source('cachematrix.R')
## > a <- matrix(1:4, 2, 2)
## > b <- makeCacheMatrix(a)
## > cacheSolve(b)
##      `[,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMatrix) inv <<- inverseMatrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
            message("getting cached data")
            return(inverseMatrix)
        }
        matrixData <- x$get()
        inverseMatrix <- solve(matrixData)
        x$setinverse(inverseMatrix)
        inverseMatrix
}
