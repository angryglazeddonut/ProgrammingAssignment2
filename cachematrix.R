## Coursera R Programming Week 3 - Programming Assignment 2
## Caching the Inverse of a Matrix

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL       #initializing inverse as NULL
  set <- function(y) {  #define set function
    x <<- y
    inv <<- NULL
  }
  get <- function()x      #define the get function that gets matrix x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv   #function that gets the inverse of matrix
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function
## This function is used to get the cache data
## It computes the inverse of the speical matrix returned by the above function


cacheSolve <- function(x, ...) {        #this gets cache data
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {             #check if inverse is NULL
    message("getting cached data")
    return(inv)           #returns inverse value
  }
  matdata <- x$get()
  inv <- solve(matdata, ...)       #calculate inverse value
  x$setInv(inv)
  inv             #return a matrix that is the inverse of x
}