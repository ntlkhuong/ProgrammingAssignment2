#Assignment 2: Caching the Inverse of a Matrix

#This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set<- function(y){
        x <<-y
        i <<-NULL
    }
    
    get<-function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)

}

## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting invers of matrix ")
        return (i)
    }
    matrix <- x$get()
    i <- solve(matrix,...)
    x$setinverse(i)
    i
}
