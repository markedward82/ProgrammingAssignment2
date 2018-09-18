
## create a function that have get, set, getinverse, setinverse.
## the arguement will take 1 matrix
## this function gives 4 calls, get, set, getinverse, and setinverse
makeCacheMatrix <- function(x = matrix){
        invr <- NULL
        set <- function(y){
                x <<- y
                invr <<- NULL
        }
        
        get <- function() x
        setInvr <- function(inverse) invr <<- inverse
        getInvr <- function() invr
        list(set = set, get = get, setInvr = setInvr, getInvr = getInvr)
}


## this function will convert the matrix to inverse
## and cache the inverse information for setinverse and getinverse
## before setting inverse, getinverse in makeCacheMatrix appears null.


cacheSolve <- function(x, ...){
        
        invr <- x$getInvr()
        
        if(!is.null(invr)){
                message("getting cache data")
                return(invr)
        }
        matrx.Data <- x$get()
        invr <- solve(matrx.Data, ...)
        x$setInvr(invr)
        return(invr)
}
