## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(c(1,3,5,7),2,2)){
  m <- NULL
  setmtx <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmtx <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = setmtx, get = getmtx,
       setinverse = setinv,
       getinverse = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m              ## Return a matrix that is the inverse of 'x'
} 
