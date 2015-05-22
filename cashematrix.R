##  Below are two functions that are used to create a special object 
##  that stores a numeric matrix and cache's its inverse.

## This function creates a special "matrix" , which is really a list 
## containing a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x=matrix()){
        s <- NULL
        setMatrix <- function(y) {
                x <<- y
                s <<- NULL
        }
        getMatrix <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed),then the cachesolve will 
## retrieve the inverse from the cache.


cacheSolve <- function(x,...){
        s <-x$getSolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$getMatrix()
        s <- solve(data)
        x$setSolve(s)
        s
}