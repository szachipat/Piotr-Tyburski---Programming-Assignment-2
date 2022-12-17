
## First function, makeCacheMatrix takes a matrix. This is similar to the function mentioned in the assignment's example. 
## We're going to take a matrix instead of a vector and inverse it (using the second function), not calculate the mean, general gist remains the same however.
## We intend to create a special "matrix" (really a list), which contains a function to set and get the value of the matrix
## and to set and get the inversed matrix (setInversion and getInversion). 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
        x <<- y
        inv <<- NULL
  }
  get <- function() x
  setinversion <- function(solveMatrix) inv <<- solveMatrix
  getinversion <- function() inv
  list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}

## Second function intends to return o matrix that is the inverse of 'x' used in the first function.
# First (using "if") it checks whether the matrix has already been inversed. In that case, a special message "Cache data" is additionally returned
# Later on, the function returns the inversion of the matrix using special function inv() which is destined to do so.

cacheSolve <- function(x, ...) {
  inv <- x$getinversion()
  if(!is.null(inv)){
        message("Cache data")
        return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinversion(inv)
  inv      
}

