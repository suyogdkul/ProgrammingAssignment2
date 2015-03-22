# As stated as part of the problem too, Matrix inversion is a costly computation 
# It is helpful to caching the inverse of a matrix rather than compute it repeatedly. 
# Please find below the two functions are used to cache the inverse of a matrix.

# makeCacheMatrix does the following
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    set <- function(y) {
        x <<- y
        invr <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invr <<- inverse
    getinverse <- function() invr
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The next function mentioned below returns the inverse of the matrix. 
# It checks if the inverse has already been computed. 
# If yes, it gets the result and the computation is skipped. 
# Otherwise, it computes the inverse, sets the value in the cache via the setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    invr <- x$getinverse()
    if(!is.null(invr)) {
        message("getting cached data.")
        return(invr)
    }
    data <- x$get()
    invr <- solve(data)
    x$setinverse(invr)
    invr
}


## Demo run:

## > m1 = rbind(c(1, 2), c(7, 1))
## > m2 = makeCacheMatrix(m1)
## > m2$get()
##     [,1] [,2]
##[1,]    1    2
##[2,]    7    1

## The first run .. With no cache

## > cacheSolve(m2)
##            [,1]        [,2]
##[1,] -0.07692308  0.15384615
##[2,]  0.53846154 -0.07692308

## Getting it  from the cache in the second run

## > cacheSolve(m)
##getting cached data.
##            [,1]        [,2]
##[1,] -0.07692308  0.15384615
##[2,]  0.53846154 -0.07692308
## > 
