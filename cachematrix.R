## An utility functions that caches the inverse of a matrix and a consumer 
## wrapper function to find the inverse which internally uses the solve() 
## function fron standard R libraries. 


## Creates a special matrix object that can cache the given matrix
## and returns a list of methods as getters and setters
makeCacheMatrix <- function( m = matrix() ) {
        
        ## Initialize the inverse data member
        inv <- NULL
        
        ## Member function to set the matrix
        set <- function( matrix ) {
                m <<- matrix
                inv <<- NULL
        }
        
        ## Member function to get the matrix
        get <- function() {
                ## Return the matrix
                m
        }
        
        ## Member function to set the inverse of the matrix
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        ## Member function to get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse data member
                inv
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of the special matrix object returned by 
## "makeCacheMatrix". If the inverse has already been calculated 
## and the matrix has not changed, then the "cacheSolve" method should retrieve
## the inverse of the matrix from the cache.
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Just return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Return the matrix
        m
}