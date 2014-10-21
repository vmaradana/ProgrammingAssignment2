## makeCacheMatrix and cacheSolve are a pair of functions that
## demonstrate caching concept of time consuming calculations
## using scoping rules in R.

## makeCacheMatrix , makeVector creates a special "vector", 
## which is really a list containing a function to 
##   1.set the value of the vector
##   2.get the value of the vector
##   3.set the value of the mean
##   4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse matrix to NULL
    i <- NULL
    
    ## "set" function to change the value of matrix x
    ## when x changes set the inverse matrix to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## "get" function for returning value of x
    get <- function() x
    
    ## "setinverse" function sets the value of i
    ## to inverse
    setinverse <- function(inverse) i <<- inverse
    
    ## "getinverse" function get the inverse
    getinverse <- function() i
    
    ## list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special
## "vector" created with the above function. However, it 
## first checks to see if the mean has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## getinverse from the special vector to see if cached
    i <- x$getinverse()
    ## if not NULL return cache
    if(!is.null(i)) {
        ## Return inverse from cache
        return(i)
    }
    
    ## get the matrix
    data <- x$get()
    
    ## calculate inverse
    i <- solve(data, ...)
    
    ## set the inverse
    x$setinverse(i)
    i
}
