## These functions implement a "cacheable matrix" which is used
## to optimize inverse computation of a matrix. 
## makeCacheMatrix - cache matrix implementation
## cacheSolve      - computes the cache matrix inverse

## This function create a "cacheable matix" which caches the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL

        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

        get <- function() x

        setinverse <- function(inv) inverse <<- inv

        getinverse <- function() inverse

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the matrix inverse. It makes use of the cached value
## if one is available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()

        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse        
}
