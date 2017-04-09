## Calculates the inverse of a matrix and caches the result for reusing in the future
## Usage example:
##
## > mtx <- matrix(c(1, 2, 2, 1), nrow = 2)
## > cachedMtx <- makeCacheMatrix(mtx)
## > inverseMtx <- cacheSolve(cachedMtx)
## > inverseMtx
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## >

## Object class to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the inverse of the matrix previously stored in an instance of makeCacheMatrix
## If the inverse is still calculated, the result is returned
## If not, the invers is calculated, and the result is stored for future uses and returned

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # gets the original matrix
        data <- x$get()
        # inverts it
        m <- solve(data, ...)
        # stores it
        x$setinverse(m)
        # and returns
        m
}
