

## A set of function that make and reverse a matrix 
## (if not reversed already) 


## based on the example given this function is a list of functions
## which sets, and gets the value of a matrix then sets the value of
## the inverse and gets it.

makeCacheMatrix <- function(x = matrix() ) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setInverse<- function(solve) invs <<- solve
        getInverse <- function() invs
        list(set= set, get = get,
             setInverse = setInverse, getInverse = getInverse)
        
}

##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x = solve()) {
        invs <- x$getInverse()
        if(!is.null(invs)) {
                print("getting cached data")
                return(invs)
        } else
                data <- x$get()
        z <- solve(data) 
        x$setInverse(z)
        print("getting the inverted cached data")
        return(z) 
}

