## These two functions takes an input matrix and cache's the inverse

#The first function takes an input Matrix and returns the Inverse
# 1. Set the Matix value
# 2. Get the value of the matix 
# 3. Set the value of the inverse matrix
# 4. Get the value of the inverse Matix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve #Solve() is the inverse function for R
  getinvert <- function() m
  list(set = set, get = get,
       setsolve = setsolve, 
       getinvert = getinvert)
}



## CacheSolve function caches inverse Matrix from makeCacheMatrix
# Or if already cached returns the cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvert()
  if(!is.null(m)) {  #if the Matrix is already Cached return Value
    message("Getting Cached Inverse")
    return(m)        #Returns the cached Matrix
  }
  data <- x$get()  
  m <- solve(data, ...)
  x$setsolve(m)
  
}


