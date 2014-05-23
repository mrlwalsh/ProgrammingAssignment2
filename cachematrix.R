## makeCacheMatrix and cacheSolve work together to compute,
## store, and retrieve the inverse of a matrix.  Calculation
## is cached to save computation time if inverse is needed
## again

## This function creates a special "matrix" object
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvr <- function(invr) m <<- invr
  getinvr <- function() m
  list(set = set, get = get,
       setinvr = setinvr,
       getinvr = getinvr)
}

## This function computes the inverse of the special 
#  "matrix" returned by makeCacheMatrix above. If the 
#  inverse has already been calculated 
#  (and the matrix has not changed), then the cachesolve 
#  should retrieve the inverse from the cache.  

cacheSolve <- function(x, ...) {
  m <- x$getinvr()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvr(m)
  m
}