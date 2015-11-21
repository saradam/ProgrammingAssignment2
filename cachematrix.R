## Below set of functions take a matrix and return its inverse that has been computed previously. 
## If not computed, then the value is computed and returned.

## This function builds a list that contains the getter and setter for matrix 
## and its inverse based on the lexical scope of the matrix using <<- operator
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set the new data
    ## delete the cached data so that new data will be cached when accessed
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## this function returns the input matrix
    get <- function() x
    ## this function sets the inverse of the matrix
    setmatrix <- function(solve) m <<- solve
    ## this function returns the inverse of the matrix
    getmatrix <- function() m
    ## makeCacheMatrix returns a list of functions to get and set matrix and its inverse
    list(set = set, get = get,  setmatrix = setmatrix, getmatrix = getmatrix)
}


## This function returns the inverse of the matrix if present from cache else by computing

cacheSolve <- function(x, ...) {
    ## gets the values from cache
    m <- x$getmatrix()
    ## if cache has value returns it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if data not in cache, compute inverse set cache and return
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m
}

## example usage - run below code
## x<-matrix(rnorm(9),3,3)
## m<-makeCacheMatrix(x)
## cacheMatrix(m)
## m$set(matrix(rnorm(16),4,4))
## cacheMatrix(m)
