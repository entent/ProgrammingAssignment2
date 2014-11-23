## These two functions are used to store the inverse of a 
## matrix.  The first stores the inverse of the matrix, 
## while the second calls it, or else calculates it, 
## and then calls it.

## Much like the example given in the README file, this 
## function creates a list of functions that either call 
## or store the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
               x <<- y
               inv <<- NULL
       }
       get <- function() x
       setinv <- function(inverse) inv <<- inverse
       getinv <- function() inv
       return(list(set = set, get = get,
            setinv = setinv,
            getinv = getinv))
}


## This function, given a matrix input, will check if its 
## inverse has been cached in the past.  If so, it will 
## output the inverse, along with a message saying that it 
## has been retrieved.  Otherwise, it calculates the 
## inverse of the matrix as its output, as well as caches the 
## inverse for retrieval in future function calls.

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
       if(!is.null(inv)) {
               message("getting cached data")
               return(inv)
       }
       data <- x$get()
       inv <- solve(data)
       x$setinv(inv)
       inv
}






















