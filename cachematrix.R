## For this assignment we are to use the example functions to create 2 functions
##the first function (makeCacheMatrix) creates a reversible matrix (square matrix)
##and scores the reverse of that matrix in the cache. The second function either
## finds the cached matrix (avoiding recomputation) or computes the reverse matrix
## if it is not in the cache

## This function creates a matrix and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #create empty cache
  im <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the inverse
  setinverse <- function(inverse) im <<- inverse
  #get the inverse
  getinverse <- function() im
  #create the list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  im <- x$getinverse()
  #check to see if its in cache...
  if(!is.null(im)) {
    message("Using Cached Data")
    return(im)
    #if its not, calculate it...
  } else {
    data <- x$get()
    #solve: calculates the inverse
    im <- solve(data)
    #inverse matrix created(im), now set it...
    x$setinverse(im)
  }
  #return inverse matrix (im)
  im
}

