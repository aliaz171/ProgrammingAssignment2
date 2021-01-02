## Put comments here that give an overall description of what your
## functions do
    # Gets a matrix as input (needs to be invertible), calculates its inverse and caches it for later access.
## Write a short comment describing this function
    # set the value of the matrix
    # get the value of the matrix
    # set the value of the inverse
    # get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y){
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() {x}
    setinv <- function(inv) {mat_inv <<- inv}
    getinv <- function() {mat_inv}
    list(set = set, get = get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function
# The following function calculates the inverse of the matrix created with the above function. However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getinv()
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        d <- x$get()
        mat_inv <- solve(d, ...)
        x$setinv(mat_inv)
        mat_inv
}

    # Here's how you can test it:
    # t<-makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
    # t$get()
    # t$getinv()
    # cacheSolve(t)
    # t$getinv() # now everytime you type this you'll get the inverse, because it has been calculated.
