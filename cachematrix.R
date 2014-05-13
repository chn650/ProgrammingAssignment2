#The following functions cache the inverse of a matrix

#The first function, makeCacheMatrix, creates a special "matrix", 
#which is really a list containing a function to:
#1. Set the value of the matrix
#2. Get the value of the matrix
#3. Set the value of the inverse of the matrix
#4. Get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        } 
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#The second function, cacheSolve, calculates the inverse of the special "matrix" created 
#with the above function. 
#1. Check to see if the inverse has already been calculated. 
#2. If so, it gets the inverse from the cache and skips the computation. 
#3. Otherwise, it calculates the inverse of the data and sets the value of the 
#inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
