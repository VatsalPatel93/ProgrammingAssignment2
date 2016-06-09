# Calculating Inverse of a Matrix is generally an expensive operation. So It would
# be helpful if one can somehow cache the inverse of a matrix and use the cached
# version insted of calculating repeatedly. Following two functions cache the inverse
# of the matrix

# makeCacheMatrix() function returns a list of functions to set the value of a matrix and 
# its inverse and get the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        temp_inv <- NULL
        set <- function(y) {
                x <<- y
                temp_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) temp_inv <<- inverse
        getinverse <- function() temp_inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve() function checks if the inverse of a matrix is saved in the cache or not. If so,
# it uses the value from the cache, otherwise it calculates the value of the inverse
# and returns the value.

cacheSolve <- function(x, ...) {
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
