##- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## R Programming on Coursera
## Programming Assignment #2: Create two functions
##
##  (1) makeCacheMatrix
##      This function creates a special "matrix" object that can cache its inverse.
##      For this assignment, assume that the matrix supplied is always invertible.
##  (2) cacheSolve: 
##      This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
##- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


## Function #1: makeCacheMatrix
## makeCacheMatrix takes an invertible, square matrix and returns a list of four functions that:
## (1) set the value of the matrix (2) get the value of the matrix (3) set the value of the inverse (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  if(ifelse(nrow(x) == ncol(x), ifelse(det(x) != 0, TRUE, FALSE), FALSE)) { # check if matrix is square and invertible
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  else { # if matrix not square and invertible, then throw an error
    message("[makeCacheMatrix error]: 'x' must be a square, invertible matrix")
  }
}


## FUNCTION #2: cacheSolve
## cacheSolve takes the list created by makeCacheMatrix and returns the inverse of the matrix represented in that list. 
## If the inverse has already been calculated, cacheSolve pulls the inverse from the cache.  
## Otherwise, cacheSolve calculates and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}