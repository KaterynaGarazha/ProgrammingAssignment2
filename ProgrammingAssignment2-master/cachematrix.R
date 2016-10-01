makeCacheMatrix <- function(M = matrix()) {
##This function creates a special "matrix" object that can cache its inverse
    inverse <- NULL
    set <- function (y) {
      M <<- y
      inverse<<- NULL
    }
    get <-function() M
    setmatrix <- function(solve) inverse<<- solve ##set the value of the inverse matrix
    getmatrix <- function() inverse ##get the value of the inverse matrix
    list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

cacheSolve <- function(M, ...) {
  inverse<- M$getmatrix()
  if(!is.null(inverse)){
    return(inverse)
  }
  data <- M$get()
  inverse<- solve(data,...)
  M$setmatrix(inverse)
  inverse
  ## Return a matrix that is the inverse of 'M'
}
## To check how the functions work, lets try:
a <- matrix(7:10,3,3)
b <- makeCacheMatrix(a)
cacheSolve(b)
> cacheSolve(b)
             [,1]        [,2]        [,3]
[1,] -0.234848485  0.01515152  0.28030303
[2,]  0.257575758 -0.24242424  0.01515152
[3,]  0.007575758  0.25757576 -0.23484848