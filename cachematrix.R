# These functions solve a matrix's inverse once, and then return
# cached inverse matrices for each future call to the cacheSolve
# function. Example usage as follows:
#
# A <- matrix(c(-3,5,1,-2), 2, 2)
# mat <- makeCacheMatrix(A)
# Aprime <- cacheSolve(mat)
# Aprime <- cacheSolve(mat)
# round(A %*% Aprime, 10)

# Makes the matrix object with inverse caching capabilities
# Input: 	x = an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Gets the matrix inverse by cache lookup, or by solving if necessary
# Input: 	x 	= the matrix object created by "makeCacheMatrix" function
#			... = any additional params to be added to the "solve" function
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}