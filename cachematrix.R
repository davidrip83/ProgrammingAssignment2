## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- mean ## replaced mean() with solve()
    getinverse <- function() i ## replaced mean with inverse
    list(set = set, get = get,
         setinverse = setinverse, ## replaced mean with inverse
         getinverse = getinverse) ## replaced mean with inverse
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() ## replaced mean with inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i) ## replaced m with i
    }
    data <- x$get()
    i <- solve(data, ...) ## replaced m with i
    x$setinverse(i) ## replaced m with i
    i
}
