## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix will input a matix and create a list containing a function to 
## set: set the value of the matrix
## get: get the value of the matrix
## setinverse: set the inverse of the matrix
## getinverse: get the inverse of the matrix

## Write a short comment describing this function
## This functions creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ##first set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## next get the value of the matrix
  get <- function() x
  ## set the inverse of the matrix using solve function
  setinverse <- function(solve) m <<- solve
  ## if first time seeing matrix, set getinverse to null (m)
  getinverse <- function() m
  ## generate the list which has the values for set, get, setinverse and getinverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## check if the matrix has changed (first time seeing matrix, getinverse will be null)
  if(!is.null(m)) {
    ## if the matrix hasn't changed, get the cached data
    message("getting cached data")
    ## return from the function and give the inverse of the matrix which is m
    return(m)
  }
  ## return the matix as data
  data <- x$get()
  ## use solve to invert the matrix
  m <- solve(data, ...)
  ## setinverse gets the inverse matrix and getinverse is not null
  x$setinverse(m)
  ## return the inverse matrix
  m
}
