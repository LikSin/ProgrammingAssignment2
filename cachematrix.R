## Caching the Inverse of a Matrix

## This pair of functions will cache the inverse of a matrix to reduce the computational
## cost in computing the inverse matrix repeatedly.

## makeCacheMatrix creates a list containing functions that:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix
## 4.  get the value of the matrix
## makeCacheMatrix assumes that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL;
  
  ##store the matrix into the cache
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  ##retrieve the matrix
  get <- function() x
  
  ##store the inverse matrix into the cache
  setinverse <- function(inverse) m <<- inverse
  
  
  ##retrieve the inverse matrix
  getinverse <- function() m
  
  ##return a list of functions available
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve first checks if the inverse matrix has been calculated.
## If it has already been calculated, it will return the inverse matrix without recalculating
## If the inverse matrix has not been calculated, it will calculate the inverse matrix,
## store the inverse matrix in the cache,
## and return the inverse matrix.

cacheSolve <- function(x, ...) 
{
  
  ##check if inverse matrix has already been calculated
  ##if inverse matrix has already been calculated, return inverse matrix
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  ##if inverse matrix has not been calculated, retrieve the matrix...
  data <- x$get()
  
  ##...inverse the matrix...
  m <- solve(data, ...)
  
  ##...and put the inverse matrix into the cache
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
