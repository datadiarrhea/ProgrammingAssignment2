## Matrix inversions can be a heavy consumer of computational resources. 
## The burden of repetitive calculations from matrix inversions can be reduced 
## by caching, which makes the calculations readily available mitigating 
## the need to redo them.

## The following pair of functions cache the inverse of a matrix:



## The first function caches the matrix's inverse:

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## The second function computes the inverse of the above matrix named 
## makeCacheMatrix. It should retrieve the inverse from the cache if 
## the inverse has been calculated and the matrix remains the same.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
