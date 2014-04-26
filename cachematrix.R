## Matrix inversion is a difficult function to execute, so caching the 
## inverse matrix will benefit instead of computing it repeatedly. The functions 
## create a special matrix object that can cache its inverse. 
## The inverse matrix already been calculated then the function will 
## retrieve the inverse from the cache 

## The makeCacheMatrix is a function which creates the matrix, really 
## is a list containing a function to set and get matrix & set and get 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
    }
  get <- function () x
  setinv <- function (rev) m <<- rev
  getinv <- function () m
  list (set = set, get = get, 
        setinv = setinv, 
        getinv = getinv)

}

## The function calculates the inverse matrix with the above function. First it 
## checks whether the inverse matrix had been calculated, if so it gets the 
## inverse matrix from the cache and skips computation

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message ("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve (data, ...)
  x$setinv(m)
  m
}
