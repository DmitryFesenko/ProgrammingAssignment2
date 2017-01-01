## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  #initialize function argument, by default as an empty matrix
        inv <- NULL #define the object inv as a placeholder for a future value
        set <- function(y) { #define a function for setting (mutate) the data values within an object.
                x <<- y #assign the input argument to the x object in the parent environment
                inv <<- NULL #assign the value of NULL to the inv object in the parent environment
        }
        get <- function() x #retrieve (access) data within an object. Since the symbol x is not defined within get(), R retrieves it from the parent environment 
        setinverse <- function(solve) inv <<- solve #assign the input argument to the value of inv in the parent environment.
        getinverse <- function() inv #defines the getter for the inverse inv to retrieve its value
        list(set = set, #assigns each of these functions as an element within a list(), and returns it to the parent environment.
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse) #each element in the list is named (to use $ operator to access the function by name)
}



cacheSolve <- function(x, ...) {
        inv <- x$getinverse() #get the inverse of the special "matrix" (which is the input of cacheSolve)
        if (!is.null(inv)) {
                message("getting cashed data")
                return(inv) # If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
        }
        data <- x$get() #If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix
        inv <- solve(data, ...) #inv calculates the inverse
        x$setinverse(inv) #x$setinverse(inv) stores it in the object inv in makeCacheMatrix.
        inv
        ## Return a matrix that is the inverse of 'x'
}
