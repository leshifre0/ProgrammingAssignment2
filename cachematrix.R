##The function will create a list of functions to set a matrix and inverse of it.

makeCacheMatrix <- function(x = matrix()) {
        k <- NULL
        set <- function(y) {
                x <<- y
                k <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) k <<- inverse
        getinverse <- function() k
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

        
##The function calculates inverse of the matrix created with the above function. 
##If inverse has already been calculated it gets from the cache.

cacheSolve <- function(x, ...) {
         
        m <- x$getInverse()
         
         if( !is.null(m) ) {
                message("getting cached data")
                return(m)
         }
        
         data <- x$get()
         m <- solve(data) %*% data
         x$setInverse(m)
        m 
}        

