## Cache the inverse of a matrix
## R Programming Assignment #2


## Create special matrix object that can cache its inverse

## Example usage:
## a <- makeCacheMatrix(x)   ## where x is a a square invertible matrix
## cacheSolve(a)  ## returns inverse of matrix returned by makeCacheMatrix()




makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

       ## change mean to solve, mean to inverse	
        setinverse <- function(solve) m <<- inverse    
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Compute inverse of special matrix return by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


