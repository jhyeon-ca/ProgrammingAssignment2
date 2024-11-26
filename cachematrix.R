## Creates a special matrix object that can cache its inverse
## It has functions to set & get the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse as NULL
    inv <- NULL  
    set <- function(y) {
            
        ## Set the value of the matrix
        x <<- y  
            
        ## Reset the inverse cache when the matrix is updated
        inv <<- NULL  
    }
        
        ## Get the matrix 
    get <- function() x  
        
        ## Set the value of the inverse
    setInverse <- function(inverse) inv <<- inverse  
        
        ## Get the value of the inverse
    getInverse <- function() inv  
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Creates the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed
## it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {

        ## Get the cached inverse, if it exists
    inv <- x$getInverse()  
    if (!is.null(inv)) {
        message("Getting cached inverse")

        ## Return the cached inverse
        return(inv)
    }
        
        ## Get the matrix
    mat <- x$get() 

        ## Compute the inverse of the matrix
    inv <- solve(mat, ...) 

        ## Cache the inverse
    x$setInverse(inv)  

        ## Return the computed inverse
    inv 
}
