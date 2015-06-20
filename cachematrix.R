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
        x <<- y ## substitute x with main input y from function above
        i <<- NULL ## old value of inverse not required anymore
    }
    get <- function() x 
    ## note: setinverse() and getinverse() don't calculate the inverse
    setinverse <- function(solve) i <<- inverse ## set inverse
    getinverse <- function() i ## get inverse
    list(set = set, get = get, ## store function in makeCacheMatrix
         setinverse = setinverse, 
         getinverse = getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() ## get the inverse from makeCacheMatrix and store in i
    if(!is.null(i)) { ## if the inverse is stored get the cached inverse
        message("getting cached data")
        return(i) ## return the inverse from the cache
    }
    data <- x$get() ## if the inverse is not stored in cache
    i <- solve(data, ...) ## calculate the inverse of data
    x$setinverse(i) ## set the inverse equal to i
    i ## return i
}