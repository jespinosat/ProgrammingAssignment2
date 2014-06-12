## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This functions creates an special matrix containing its inverse if provided 
#by the user in order to save computation time. 
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
        
        
}


## Write a short comment describing this function
#Given the previous functions, this one calculates the matrix inverse if not provided by the user before,
#if the second case happens, it will return the supplied inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
