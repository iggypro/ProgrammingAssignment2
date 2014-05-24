## Coursera | R Programming | Assingment #2
## May 24, 2014
## Assignment: Caching the Inverse of a Matrix
## Functions:
## (1) makeCacheMatrix() - creates special "matrix" object that can cache its inverse
## (2) cacheSolve() - computes the inverse of the special "matrix"

## creates special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  ##define function to cache original matrix
  inverseMatrix <- NULL
  setMatrix <- function(y) 
    {
      x <<- y
      inverseMatrix <<- NULL
    }
  ##define function to retrieve original matrix from cache
  getMatrix <- function() x
  ## define function to cache inverse matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  ## define function to retrieve inverse matrix from cache
  getInverse <- function() inverseMatrix
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## creates special "matrix" object that can cache its inverse returned by makeCacheMatrix()
cacheSolve <- function(x, ...) 
{
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) 
    {
      message("getting cached data")
      return(inverseMatrix)
    }
  data <- getMatrix()
  inverseMatrix <- solve(data)
  x$setInverse(inverseMatrix)
  return(inverseMatrix)
}
