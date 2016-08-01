
##This function creates a special matrix object that caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
     inver <- NULL
         set <- function(y) {
                x <<- y
                inver <<- NULL
        }
            get <- function() x
            setinverse <- function(solve) inver <<- solve
            getinverse <- function() inver
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## If inverse has already been calculated, cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

            inver <- x$getinverse()
            if(!is.null(inver)) {
                    message("getting cached data")
                    return(inver)
            }
            data <- x$get()
            inver <- solve(data, ...)
            x$setinverse(inver)
            inver


}
