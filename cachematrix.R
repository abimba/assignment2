## A pair of functions 'makeCacheMatrix' and 'cacheSolve' that cache the inverse of a matrix.
##  Use:
##  mat<-matrix(data,nrow,ncol)  -- create matrix 'mat' values=data, no.of rows=nrow, no.of column=ncol
##  z<-makeCacheMatrix(mat)      -- create special matrix 'z' using the makeCacheMatrix function
##  cacheSolve(mat)              -- solve for the inverse of matrix 'mat' using the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  
  # This function creates a special "matrix" object that can cache its inverse.
  
  mat<-NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x                                   # get original matrix
  setInverse <- function(inverseMat) mat <<- inverseMat # save cached matrix
  getInverse <- function() mat                          # get cached matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)                         # return special matrix
  
}

cacheSolve <- function(x, ...) {
  
  ## This function returns a matrix that is the inverse of 'x' 
  ## How it works:
  ## 1. Gets the inverse of the special matrix 'z'
  ##    which is returned by makeCacheMatrix above. 
  ## 2. It checks if the inverse has been calculted, if it has been calculated
  ##    it retrives it from the cache else it calculates the inverse and stores it in the cache
  ## 3  It returns the inverse
 
  mat <- z$getInverse()             # retrive matrix inverse
  if(!is.null(mat)) {               # check if inverse has been calculated
    message("getting cached data")
    return(mat)                     # return inverse from cache
  }
  data <- z$get()                   # get original matrix 'x'
  mat <- solve(data, ...)           # calculate inverse of original matrix 'x'
  z$setInverse(mat)                 # save result in cache
  mat                               # return inverse of matrix 'x'
}



