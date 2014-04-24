## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions which are
## set - sets value of matrix
## get - gets value of matrix
## setinver - set inverse of matrix
## getinver - gets inverse of matrix

makeCacheMatrix <- function(x = matrix()) 
{
	inver <- NULL  # initialises the value of inverse matrix to NULL
    set <- function(y)  # to set the value of matrix
	{
        x <<- y
        inver <<- NULL   # since the matrix changed
    }
    
    get <- function() x # to get the value of the matrix
    
    setinver <- function(temp) inver <<- temp # to set the inverse
    
    getinver <- function() inver # to get the inverse

    return(list(set = set, get = get, setinver = setinver, getinver = getinver)) #list of functions
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.
## Else it calculates, stores it into cache and returns the inverse.

cacheSolve <- function(x, ...) 
{
	inver <- x$getinver()
    if(!is.null(inver)) #check if inverse is already cached
	{
        message("retrieving cached data")
        return(inver)
    }
    
    data <- x$get() # get the matrix into data
    
    inver <- solve(data, ...) # compute the inverse
    
    x$setinver(inver) # cache the inverse
    
    return(inver) # return inverse
}
