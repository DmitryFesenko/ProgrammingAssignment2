makeVector <- function(x = numeric()) {
        m <- NULL #begins by setting the mean to NULL as a placeholder for a future value
        set <- function(y) {
                x <<- y       #defines a function to set the vector, x, to a new vector,  y, 
                m <<- NULL    #and resets the mean, m, to NULL
        }
        get <- function() x    #returns the vector, x
        setmean <- function(mean) m <<- mean  #sets the mean, m, to mean
        getmean <- function() m #returns the mean, m
        list(set = set, get = get, #returns the 'special vector' containing all of the functions just defined
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}