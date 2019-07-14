## Function makeCacheMatrix caches the inverse of a matrix.
## Function cacheSolve finds the inverse of a matrix, and if it is already cached, it
## returns the cached inverse matrix.

## makeCacheMatrix: sets the matrix value as a cached matrix, sets the inverse, and
## gets the inverse matrix. 
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(inter) {
        mat <<- inter
        inv <<- NULL
    }
    get <- function() mat
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(mat, ...) {
    ## returns an inverse matrix of matrix called mat.
    inv <- mat$getinverse()
    if (!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    data <- mat$get()
    inv <- solve(data, ...)
    mat$setinverse(inv)
}


