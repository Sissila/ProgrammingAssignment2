## The following two functions are used to cache the inverse of a matrix. 


## The first function is called "makeCacheMatrix"
## It returns a list of functions to set, get, set the inverse 
## and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The second function is called "CacheSolve"
## First of all, it checks if the inverse is already computed 
## If so, it retrieves it from the cache. Otherwise, it computes
## the inverse. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        return(inv)
    }
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
}
