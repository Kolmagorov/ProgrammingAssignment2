## Two functions  below compute 

# This function create a list containing 4 elements, input matrix itself, inverse matrix,
# new input matrix and recalculated cached matrix

makeCacheMatrix <- function(x = matrix(c(1,3,5,7),2,2)){
  m <- NULL
# setting a new matrix for a new calculation
  setmtx <- function(y) {
# Declaring so called public variables it means that x and m including their values
# can be used by other functions  
    x <<- y
    m <<- NULL
  }
  getmtx <- function() x # Returns input matrix
  setinv <- function(inv) m <<- inv # redefining of cache 
  getinv <- function() m # returns inverse matrix of x if it has been computed
  list(set = setmtx, get = getmtx,
       setinverse = setinv,
       getinverse = getinv)
}

# CacheSolve computes inverse matrix using as an input a matrix provided by makeCacheMatrix
# General speaking, it is not being used matrix itself as input data. X  is a list 
# of several marices returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
# Assigning to m variable a function that allocates a space in X-list for inverse matrix
  m <- x$getinverse()
# Next step verifies whether is m empty or not. In other word, whether inverse matrix
# has been computed previously.
  if(!is.null(m)) {
# If it will be true i.e. m contains computed inverse matrix for a given inputdata,  
# a message will appear and cached data will be returned.
    message("getting cached data")
    return(m)
  }
# Otherwise, input matrix invoking by x$get() will be assigned to variable 'data'
  data <- x$get()
# inverted by function solve, and then result is transferred to variable m - caching
m <- solve(data, ...)
# Finally, result of invertion is written in X-list using x$setinverse()
  x$setinverse(m)
  m              ## Return a matrix that is the inverse of 'x'
} 
