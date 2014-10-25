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
  inverse <- NULL
  set <- function(y) {
    myMatrix <<- y
    inverse <<- NULL
  }
  get <- function() myMatrix
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the supplied matrix
## It first checks to see if the inverse already exists - if it does
## it returns the inverse otherwise it computes adn then returns the
## inverse

cacheSolve <- function(myMatrix, ...) {
  inverse <- myMatrix$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- myMatrix$get()
  inverse <- solve(data, ...)
  myMatrix$setinverse(inverse)
  inverse
}
