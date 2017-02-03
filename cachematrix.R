## makeCacheMatrix contains functions to set/get the value of a matrix 
## and its inverse embedded in a list variable

makeCacheMatrix <- function(x = matrix()) {
  var.inverse <- NULL
  set <- function(y) {
    x <<- y
    var.inverse <<- NULL
  }
  get <-function() x 
  setinverse <-function(inverse) var.inverse <<- inverse
  getinverse <-function() var.inverse
  list(set=set, get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    var.inverse <- x$getinverse()
    if(!is.null(var.inverse)) {
      message("fetching cached ")
      return(var.inverse)
    }
    data <- x$get()
    var.inverse <- solve (data, ...)
    x$setinverse(var.inverse)
    var.inverse
  }
  
## Returns a matrix that is the inverse of 'x'

##Below, I've set up two variables to expedite testing. Simply run cacheSolve(test.var) to return
##the inverse of the test 2x2 matrix
testmatrix<-matrix(1:4,nrow=2,ncol=2)
test.var<-makeCacheMatrix(testmatrix)


