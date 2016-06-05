## This is R programming course, function that's purpose to is to cache the inverse of a matrix
##Written 6/5/2016 by author: Steierjo
##So matrix inversion is known to be costly... how can we make this better?



##Essentially the function is 
makeCacheMatrix <- function(x = matrix()) { ## define the argument with matrix
    inv <- NULL                             ## initialize inv as NULL so that it will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function
        x <<- y                             ## 
        inv <<- NULL                        ## resetting to null
    }
    get <- function() x                     ## need the get function because returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv 
    getinverse <- function() inv                     ## gets the value of inv 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## This function computes the inverse of the matrix.
## If the inverse has already been calculated 
## then cacheSolve will get the inverse from the cache
## in this assignment we assume the matrix is invertible 
##Might be more realistic to account for non-invertible..
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

