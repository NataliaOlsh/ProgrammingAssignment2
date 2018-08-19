## This is a pair of functions that provide a convinient method to get inverse
## for a square invertible  matrix without repeating this calculation for one matrix.
## First time called for the matrix the inverse is calculated and put into cache,
## after that it is taken from cache without calculating for a same matrix.

## Use exapmple
## > ff<-makeCacheMatrix(mm)
## > cacheSolve(ff)
##      [,1] [,2]
## [1,]   -5    3
## [2,]    2   -1
## > cacheSolve(ff)
## getting cached data
##      [,1] [,2]
## [1,]   -5    3
## [2,]    2   -1
 

## makeCacheMatrix function is a list of methods(functions) for a matrix:
##   - set value of the matrix
##   - get value of the matrix
##   - set value of the inverse matrix
##   - get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(iMatrix) i<<-iMatrix
        getsolve <- function() i
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve function calculates inverse for a square invertible matrix which is
## processed by previous function (so the result has all 4 methods).  
## First it checks wheather the inverse for the matrix is already cached:
##   - If so it gets the inverse matrix from cache without calculating.
##   - Otherwise it calculates the inverse matrix, puts it into cache and returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getsolve()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data, ...)
        x$setsolve(i)
        i
}
