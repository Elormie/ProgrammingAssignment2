## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A pair of functions namely; makeCacheMatrix and cacheSolve, to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
 matrix_inverse <- NULL   ##Inverse made NULL
  set <- function(y){
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x    ##Getting a matrix of x
  setInverse <- function(inverse) matrix_inverse <<- inverse  ##Setting the inverse of a matrix
  getInverse <- function() matrix_inverse    ##Getting the inverse of a matrix
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Function to cache data

cacheSolve <- function(x, ...) ##Caching data
{
matrix_inverse <- x$getInverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data!")
    return(matrix_inverse)
    ##Return an Inverse of matrix
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...) ##Solving Inverse value
  x$setInverse(matrix_inverse)
  matrix_inverse
  ## Return a matrix that is the inverse of 'x'
}
