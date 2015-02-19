## cacheSolve() and makeCacheMatrix() work together to allow
## the user to create a cache with a matrix and its inverse so
## subsequent calls for the inverse use the cached inverse rather
## than recalculating the invers. Below is a usage example.
## Be sure to source both functions first!
##
## Example first use of makeCacheMatrix and cacheSolve:
##
## Create an 8x8 matrix of random numbers (hope it inverts!).
##    myMat <- matrix(rnorm(64),8,8)
## 
## Create the "matrix", which is a list storing the matrix and inverse
## (initally null) and functions for getting and setting.
##
##    myMatList <- makeCacheMatrix(myMat)
##
## Get the inverse of the matrix.
##
##    myInverse <- cacheSolve(myMatList)
##
## Call cacheSolve again to get the cached inverse. Note you see
## "getting cached data" this time if you uncomment the ## message
## call in line 71 below.
##
##    cacheSolve(myMatList)
##
## Verify that it is the inverse (look for '1' on diaginal; '0' off)
## Expect computation error in the last few digits if you do not round.
##
##    round(myInverse %*% myMat,digits=14)
##
## To get the matrix back:
##
##    myMatList.get()  
##
## To get the inverse directly(once cacheSolve() has been run):
##
##    myMatList.getInverse() 
##
## Or when unsure if cacheSolve() has been run (much safer!):
##
##    cacheSolve(myMatList)

## makeCacheMatrix takes a matrix and stores it in list along with
## functions for getting and setting the matrix and its inverse.
## cacheSolve below can then use the list from makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve takes a list made by makeCacheMatrix() and
## it returns the inverse of the matrix solving for it once,
## caching the result, and returning the cached result subsequently.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)) {
##             message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setInverse(inv)
      inv
}
