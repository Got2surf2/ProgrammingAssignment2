## This function takes a matrix as a parameter.  Matrix is assumed
## to be a square 

## It provides a set of functions to interact with the matrix
##    set - sets the value of the matrix supplied by the calling routine
##          Also nulls out the inverse of the matrix

##    get - returns the value of the matrix

##    setinverse - computes the inverse of the matrix and assignes it to a
##                 a local object

##    getinverse - returns the inverse of the matrix

makeCacheMatrix <- function(myMatrix = matrix()) {
  inverse <- NULL                                  ## initialize inverse to null
  set <- function(y) {                             ## define the set function 
    myMatrix <<- y                                 ## force the assignment to the global object
    inverse <<- NULL                               ## set inverse to null in case it was previously calculated
  }
  get <- function() myMatrix                       ## get the value of the martix
  setinverse <- function(solve) inverse <<- solve  ## set the inverse of the matrix
  getinverse <- function() inverse                 ## set the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the supplied matrix
## It first checks to see if the inverse already exists - if it does
## it returns the inverse otherwise it computes and then returns the
## inverse

cacheSolve <- function(myMatrix, ...) {
  inverse <- myMatrix$getinverse()                 ## gets the matrix inverse
  if(!is.null(inverse)) {                          ## checks to see if it is calready calculated
    message("getting cached data")                 ## returs the already calculated matrix
    return(inverse)
  }
  data <- myMatrix$get()                           ## get the matrix
  inverse <- solve(data, ...)                      ## computes the inverse
  myMatrix$setinverse(inverse)                     ## caches the inverse for future use
  inverse                                          ## returns the just calculated inverse
}
