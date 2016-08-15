## The function create a special "matrix" object that can cache its inverse
## First we have to compute the inverse value of each function

makeCacheMatrix <- function(x = matrix()) {

       m <- NULL
       set <- function(y) {
               x <<- y
               m <<- NULL
       }
       get <- function() x
       setinverse <- function(mean) m <<- mean
       getinverse <- function() m
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}




## This function will return the inverse of 'x'

     cacheSolve <- function(x, ...) {
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

}
