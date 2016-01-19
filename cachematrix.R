## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a list containing functions to
## set the value of the matrix x
## get the value of the matrix x
## set the value of the matrix inverse_matrix using the solve function
## get the value of the matrix inverse_matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse_matrix <<- solve
  getsolve <- function() inverse_matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function
## ths cacheSolve function first gets the value of the inverse matrix
## and checks if the inverse matrix was already calculated (is not null)
## If so, the value is returned
## If not, the inverse matrix is calculated from x using the solve function
## Finally, the inverse matrix that is the inverse of 'x' is cached then and returned
cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getsolve()
  if(!is.null(inverse_matrix)) {
    message("get the cached inverse matrix")
    return(inverse_matrix)
  } else {
    message("calculate the inverse matrix")
  }
  data <- x$get()
  inverse_matrix <- solve(data)
  x$setsolve(inverse_matrix)
  inverse_matrix
}
