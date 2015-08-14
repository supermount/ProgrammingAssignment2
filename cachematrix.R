## The two functions below are structures to store both the Matrix
## and its inverse (cached after first calculation)

## Matrix structure to hold matrix value and its inverse value

makeCacheMatrix <- function(x = matrix()) {
        # initialize inverse to NULL
        inv <- NULL
        # set matrix value
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get matrix value
        get <- function() x
        #set inverse
        setinv <- function(inverse) inv <<-inverse
        #get inverse
        getinv <- function() inv 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## get the inverse value of the matrix structure
## if a cache version not available, calculate inverse

cacheSolve <- function(x, ...) {
        # get inverse value and assign to inv
        inv <- x$getinv()
        # check to see if inv exists, yes get cached data
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # no, cache so get matrix value and caluclate inverse
        data <- x$get()
        inv <- solve(data, ...)
        # set calculated inverse into cache.
        x$setinv(inv)
        inv
}
