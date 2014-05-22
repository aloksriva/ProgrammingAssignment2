## This file contains two functions which help invert a matrix, and leverage caching of   
## invertedmatrix so as to avoid re-inversion when inversion function is called on same 
# data set.

## makeCacheMatrix() : accepts a matrix as input parameter and returns a set of cached 
##                     matrices. Includes get() and set() functions for input matrix &
##                     its inverse. Does not perform the matrix inversion, rather 
##                     stores the results.

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL              ## Initialize a placeholder for inverse matrix
  
  getMatrix <- function() x      ## get the input parameter matrix' data
  
  setMatrix <- function(y) {     ## set matrix data
    x <<- y
    invMatrix <- NULL
  }
  
  getInverse <- function() invMatrix ## get cached inverted matrix
  
  setInverse <- function(z) {        ## set inverse for input matrix
    invMatrix <<- z
  }
  return(list(getMatrix = getMatrix, setMatrix = setMatrix,
              getInverse = getInverse, setInverse = setInverse))
}


## cacheSolve() : accepts a matrix as an input parameter. Checks if an inverse of input
##                exists in cache. If yes, returns the cached inverted matrix; Else, 
##                computes inverted matrix and returns the same

cacheSolve <- function(x, ...) {
  
  invMatrix <- x$getInverse() ##try getting inverted matrix from cache
  
  ## Check if the result is not null implying inverted matrix exists in cache, hence no
  ##  need tore-compute the inverse and the result is returned to calling program/user.
  if (!is.null(invMatrix)) {
    message("Fetched Inverted Matrix from Cache..")
    return(invMatrix)
  }
  
  ## Control comes to this section in case invMatrix is null, i.e., inverted matrix is not 
  ## available in cache
  message("Inverted Matrix not found in cache, computing inverse now..")
  data <- x$getMatrix         ## Get input matrix data
  
  invMatrix <- solve(data)    ## Compute inverse of input matrix data
  
  x$setInverse(invMatrix)     ## Cache inverted matrix for subsequent use
  
  invMatrix                   ## Return inverse of input matrix
}
