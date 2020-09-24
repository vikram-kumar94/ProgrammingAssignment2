## makeCacheMatrix is used to set the inverse value of the matrix
## cacheSolve is used to find the inverse of the matrix, if not already stored in cache

## This function will set the value of the original matrix and define the functions to set and get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y) {
                x <<- y
                j <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {j <<- inverse}
        getinverse <- function() {j}
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function will check if the inverse is available in cache, and if so print it. If not, it will compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getinverse()
        if(!is.null(j)) {
                message("getting cached data")
                j
        }
        
        k <- x$get()
        j <- solve(k, ...)
        x$setinverse(j)
        j
}
