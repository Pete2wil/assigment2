## Put comments here that give an overall description of what your
## functions do

## makeCachematrix creates a matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL                                      ## set matrix
        set <- function(mx) {                           
                x <<- mx
                iv <<- NULL
        }                                               

        get <- function() x                             ## initialize get with matrix

        setInverse <- function(inverse) iv <<- inverse  ## set matrix inverse
        
        getInverse <- function() iv                     ## get matrix inverse

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)                   ## set and get matrix inverse
}

## cacheSolve computes the inverse of the matrix created by makeCacheMatrix, unless the inverse has already been calculated
## and the matrix hasn't changed (in which case it retrieves the inverse from the cache)

cacheSolve <- function(x, ...) { 
        iv <- x$getInverse()                            ## return a matrix that is the inverse of 'x' 
        if (!is.null(iv)) {                             ## if inverse is not NULL (has already been caculated), retrieve cached inverse
                message("getting cached data") 
                return(iv) 
        } 
        mx <- x$get()                                   ## initialize matrix mx 
        iv <- solve(mx, ...)                            ## set iv to inverse solution of matrix mx
        x$setInverse(iv)                                ## create the inverse of iv 
        iv                                              ## output iv
} 
