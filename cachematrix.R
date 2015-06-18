## This code contains two functions: makeCacheMatrix() and cacheSolve()
## The two functions combined allow you to cache a matrix function or its 
## inverse in a special matrix object
## There is a separate function to calculate the inverse of the matrix object
## if the cached matrix is not an inverse

## makeCacheMatrix create a special matrix object i
## This cached object contains a matrix or its inverse

makeCacheMatrix <- function(x = matrix()) { ## input is a matrix x
    i <- NULL ## i is the special matrix object
    set <- function(y) { ## assign values to x and i
        x <<- y
        i <<- NULL
    }
    get <- function() x 
    setinverse <- function(solve) i <<- inverse ## calculate inverse
    getinverse <- function() i ## get inverse
    list(set = set, get = get, ## list to set and get inverse
         setinverse = setinverse, 
         getinverse = getinverse) 
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
