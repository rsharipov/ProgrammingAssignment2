# creates a list wrapping a given matrix "x"
# that adds caching capabilities for making
# a calculation on the matrix "x"
makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL
  set <- function (y) {
    x <<- y
    cached <<- NULL
  }
  get <- function() x
  setCached <- function (newCached) cached <<- newCached
  getCached <- function () cached
  list(set = set, get = get, setCached = setCached, getCached = getCached)
}


# function takes a list "x" created with makeCacheMatrix
# and returns an inverse for the matrix set to the list "x"
# the result is only calculated once and then cached
# for future calls to cacheSolve on the same list "x"
cacheSolve <- function(x, ...) {
  cached <- x$getCached()
  if (!is.null(cached)) {
    message("getting cached data")
    return(cached)
  }
  data <- x$get()
  cached <- solve(data)
  x$setCached(cached)
  cached
}


# tests the two functions above
test <- function() {
  # testing that cacheSolve in fact returns an inverse
  m <- matrix(1:4, 2, 2)
  cachedM <- makeCacheMatrix(m)
  stopifnot(cachedM$get() == m)
  
  stopifnot(is.null(cachedM$getCached()))
  
  mInv <- cacheSolve(cachedM)
  stopifnot(mInv %*% m == diag(2))
  
  # testing that setting another matrix and calling 
  # cacheSolve returns an inverse of the newly set matrix
  m <- matrix(c(-1, 0, 1, 0, -1.5, 0.5, 0, -2, 0), 3, 3)
  cachedM$set(m)
  mInv <- cacheSolve(cachedM)
  stopifnot(mInv %*% m == diag(3))
}