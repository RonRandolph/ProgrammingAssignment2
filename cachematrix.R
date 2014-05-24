## This R function is able to cache potentially time-consuming inverse matrix computations.

## The first function, "makeCacheMatrix" creates a  vector list containing a function to 
## 1: set the value of the matrix
## 2: get the value of the matrix
## 3: set the value of the inverse matrix
## 4: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#   This "cacheSolve" function returns the inverse of the matrix. If the inverse is
#   inverse is already calculated, it instead retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
    
    minv <- x$getinverse()   #returns inverse of the matrix
    if(!is.null(minv)) {
        message("retrieving cached data")   #returns message to console
        return(minv)
    }
    mat <- x$get()
    minv <- solve(mat, ...)  #compute inverse
    x$setinverse(minv)
    minv   #returns inverse
}
###
