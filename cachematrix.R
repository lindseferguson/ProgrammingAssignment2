## This is for assignment 2 of the R Programming course on Coursera.  
## Goal is to create function that will first check to see if the inverse of a matrix
## has been calculated before.  If it has it will pull the inverse from the cache and
## if it hasn't then it will calculate the inverse.

## This function creates a special matrix to be used in the cacheSolve function.
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## This function calculates the inverse of the matrix but first checks to see if it has 
## already been calculated using the makeCacheMatrix function.  
## If it has it pulls the value from the cache.

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}