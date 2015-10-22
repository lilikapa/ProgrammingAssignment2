## The following two functions are used to calculate the inverse of a matrix,
# if the matrix has been cached before,
# the inverse of a matrix will be returned right now rather than compute it repeatedly

# The following function makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
# 1. "Set" sets the matrix established by the user and prints it in the first item of the list, so that the matrix can be changed with e.g. a$set()
# 2. The function "get" only prints the established matrix in the second item of the list.
# 3. The function "setinverse" sets the inverse and prints in the third item of the list. Should never be used, because it changes the value of inverse, e.g. through "a@setmean()"
# 4. The funciont "getinverse" prints the inverse matrix in the third item of the list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the inverse matrix in the cache via the "setinverse" function to m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
