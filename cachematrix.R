## Coursera: R Programming - Assignment 2
## These functions compute the inverse of a square matrix and cache the result

## The first function makes a list of functions that set and get the matrix
## ('set' and 'get') and the inverse matrix ('setinverse' and 'getinverse') 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function computes the inverse of a matrix or retrieves the  
## inverse from a cache using the list of functions created by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
