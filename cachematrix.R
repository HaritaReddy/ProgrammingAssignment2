
##This function creates a special matrix object that caches its inverse 
##This is because inverse computation is costly

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If inverse has already been calculated for the given matrix, cacheSolve retrieves the inverse from the cache
##Inverse is computed using solve function

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
