## This pair of functions computes the inverse of a matrix
## and stores it in cache to lower computational cost.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It is a function that stores a list of functions
## needed to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set is a function that changes the matrix stored in the main function.
  set <- function(y) {
    x <<- y ## double arrow replaces x in main fct, not just in set
    m <<- NULL ## resets the inverse
  }
  get <- function() x ## returns x stored in function() - the input
  setinv <- function(inv) m <<- inv ## uses set function
  getinv <- function() m
  ## store all these functions in makeCacheMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve checks if the inverse of a matrix is already stored in cache
## if not, it inverts the matrix and stores the inverse in cache

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## use makeCacheMatrix to assign inverse if already stored in cache
  m <- x$getinv()
  ## check if inverse has already been stored if so get from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } ## otherwise invert the matrix
  data <- x$get() ## gets the matrix stored with makeCacheMatrix
  m <- solve(data, ...) ## calculates the inverse of that matrix (now called 'data')
  x$setinv(m) ## store the inverse in the cache
  
  m ## return m, the inverse of 'x'
}
