## makeCacheMatrix includes 5 functions:
        ## makeCacheMatrix initializes the inverse cache and loads the matrix
        ## set replaces the initial matrix with new values
        ## get returns the matrix
        ## setinverse caches the inverse of the matrix
        ## getinverse returns the invers of the original matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                     # Initialize m so cache is not pulled
        set <- function(y) {        
                x <<- y               # Modify the matrix
                m <<- NULL            # Initialize m so cache is not pulled
        }
        get <- function() x          
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m    
        list(set = set, get = get,    # List of functions to reference with $
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'
        ## If the inverse is already cached, it is returned from the cache
        ## if not, the inverse is calculated and cached

cacheSolve <- function(x, ...) {
        m <- x$getinverse()             # Get the value of the stored inverse
        if(!is.null(m)) {               # Is it not NULL (i.e., cached)?
                message("getting cached data")
                return(m)               # Pass m to cacheSolve
        }
        data <- x$get()                 # Get the original matrix
        m <- solve(data, ...)           # Invert it
        x$setinverse(m)                 # Cache the inverse
        m
        
}
