## These functions are designed to avoid repiting calculations
## If we know that we are going to need the inverse of a matrix lot of times
## we can calculate it once, and store the result for the next time that we need it.
## These functions are for that.


## This function creates from a matrix a special type of object that stores:
## 1) A function to set the matrix that's going to be returned byt the nex function
## 2) A function to get that matrix
## 3) A function to set the inverse of the matrix
## 4) A function for setting it.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y   
    inv <<- NULL
  }
  
  get <- function() {x}
  
  setInverse <- function(inverse){inv <<- inverse]
  
  getInverse <- function() {inv}
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This function is the one that user has to use if he/she wants to avoid extra calculation.
## In this function, the inverse of the matrix is calculated and the value is stored for further calculations (if it hasnt been calculated yet)
## or the value of the inverse is returned from a variables that is storing it
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv      
  
}
