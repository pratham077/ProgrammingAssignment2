## Put comments here that give an overall description of what your
## functions do
# These functions Calculate the inverse of a matrix and caches it and returns the Cached value if the inverseis
# already calculated

## Write a short comment describing this function
#Calculates inverse of the matrix and Caches it
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
# If the inverse has already been calculated it will return the cached inverse instead of 
# computing it again 
# If the inverse has not aleady been  calculated it will calculate the inverse sets the value of the 
# inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
