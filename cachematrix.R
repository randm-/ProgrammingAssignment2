## These functions implement a "cacheable matrix" which is used
## to optimize inverse computation of a matrix. 
## makeCacheMatrix - cache matrix implementation
## cacheSolve      - computes the cache matrix inverse

## This function create a "cacheable matix" which caches the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize inverse to NULL
        inverse <- NULL

        set <- function(y) {
                ## Set new matrix and set inverse to NULL
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
                ## return cached inverse if it is not null
                message("getting cached data")
                return(inverse)
        }

        ## compute and save inverse since it is not cached
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse        
}
