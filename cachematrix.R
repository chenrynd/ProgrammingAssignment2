## The function is to cache the inverse of a matrix, so that when caculating the inverse of a matrix, retrieve the inverse if it's already existed in the cache.
## create a special matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
       setmatrix <- function(y){
               x <<- y
               inv <<- NULL
       }
        getmatrix <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## get the inverse of the special matrix. retrieve the inverse if it's already there in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mtx <- x$getmatrix()
        inv <- solve(mtx, ...)
        x$setinverse(inv)
        inv
}
