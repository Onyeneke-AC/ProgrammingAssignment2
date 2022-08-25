## Function caching the inverse of a matrix
## where arg x must be an invertible matrix

## To try out both functions below, we can make use of:
## x = makeCacheMatrix(matrix(rnorm(4), 2, 2))
## cacheSolve(x)

makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    
    set <- function(j) {
        x <<- j
        inve <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(inverse) inve <<- inverse
    
    get_inverse <- function() inve
    
    list(set = set, get = get, set_inverse = set_inverse,get_inverse = get_inverse)

}


## Function computing and caching the inverse of a matrix
## where arg x is a result of makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    
    inve <- x$get_inverse()
    
    if(!is.null(inve)) {
        
        message("getting cached matrix inverse")
        
        return(inve)
    }
    
    inverse_data <- x$get()
    
    inve <- solve(inverse_data, ...)
    
    x$set_inverse(inve)
    
    inve
}
