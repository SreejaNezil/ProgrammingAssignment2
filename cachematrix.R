## A pair of functions that calculate the inverse of a matrix and cahce the value

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_mx <- NULL
        set <- function(y){
                x <<- y
                inv_mx <<- NULL
        }
        get <- function()x
        setinv <- function(inv) inv_mx <<-inv
        getinv <- function() inv_mx
        list(set = set, get = get, setinv = setinv, getinv =getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mx <- x$getinv()
        if(!is.null(mx)) {
                message("getting cached data")
                return(mx)
        }
        data <- x$get()
        mxinv <- solve(data)
        x$setinv(mxinv)
        mxinv
}