## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x<<- y
                s<<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s<<- solve
        getinverse <- function() s
        list (set=set, get=get, setinverse = setinverse,
              getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return (s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}


##Tests below: please ignore for grading, thanks!
a <- makeCacheMatrix(matrix(c(1,4,3,9),2,2))
cacheSolve(a) 
a$set(matrix(c(1,5,3,9),2,2))
a$getinverse()
