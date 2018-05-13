## Submitted by Aline Teresa L. Mendoza 
## Put comments here that give an overall description of what your
## functions do a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run:
## > x = matrix(1:4, 2, 2)
## > x
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## No cache in the first run
## cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > cacheSolve(m)
## getting cached data.
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
