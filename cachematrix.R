##calculates the inverse of a matrix, which can take a long time. 
#Cache the inverse of a matrix rather than compute it repeatedly 
#this pair of functions will caculate and cache the inverse of a matrix

## This function takes your matrix and makes it so tit can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
                
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i

        list(set = set, get = get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function computes the inverse of the matrix made by makeCacheMatrix  
## If the inverse has already been calculated, and the matrix hasn't changed,
## then the cachesolve will retrieve the inverse from the cache rather than recalculating it 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
