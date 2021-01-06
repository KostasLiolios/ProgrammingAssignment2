## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL           ## Will hold the value of matrix inverse 
  set <- function(y){
    x <<- y
    invmat <<- NULL        ## In case of new matrix resets the value to NULL
  }
  get <- function()x
  setInverse <- function(inverse) invmat <<- inverse
  getInverse <- function() invmat 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes and creates the inverse of a matrix from the above function
## "makeCacheMatrix"
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  invmat <- x$getInverse()
  if(!is.null(invmat)){
    message("getting cached data")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat,...)
  x$setInverse(invmat)
  invmat
}
