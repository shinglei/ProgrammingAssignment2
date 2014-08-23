## makeCacheMatrix stores any matrix passed to it as the working matrix
## and resets m and it's inverse to null until the matrix has been
## passed to cacheSolve. As m is not null, then the inverse of the matrix
## has been stored, while if m is null, then the inverse of the working matrix
## has not been calculated before and then gets passed through the solve
## function. m is then set to the inverse of the working matrix now.

## Stores matrix passed to the function and resets m to null.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Gets cached inverse of matrix x through m or calculate if it m is null.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, diag(ncol(data)), ...)
    x$setinverse(m)
    m
}
