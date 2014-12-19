## The two functions below work together to facilitate caching 
## of the inverse matrix calculation that is often costly
## especially when computed repeatedly

## makeCacheMatrix does the prep work for caching
## It needs to be called only once

makeCacheMatrix <- function(x = matrix()) {
        ## Prepare to cache matrix inverse calculations 
        
        # Inverse Matrix 
        ix <- NULL
        
        # set is used to remember the Original Matrix
        # i.e. the matrix to be inversed
        set <- function(mtx) {
                # Cache the input matrix
                x <<- mtx
                # Set the result to null
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(inv_mtx) ix <<- inv_mtx
        getinverse <- function() ix
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve uses makeCacheMatrix to enable caching 
## rather than recalculation of matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ix <- x$getinverse()
        if(!is.null(ix)){
                message("getting cached matrix data")
                return(ix)
        }
        # Otherwise we need to calculate and cache the result
        mtx <- x$get()
        ix <- solve(mtx, ...)
        x$setinverse(ix)
        ix
}
