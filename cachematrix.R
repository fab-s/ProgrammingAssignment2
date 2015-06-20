## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "vector", which is a listt
#containing a function to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse matrix
#4.  get the value of the inverse matrix

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


## Write a short comment describing this function
#This function calculates the inverse of the special "vector"
#created with the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse matrix
#and sets the inverse in the cache via the `setinverse`
#function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if( !is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    m <-x$get()
    m <- solve(m)
    x$setinverse(m)
    m
}
