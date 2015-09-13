## The following two functions allow to create a cache to
## store a matrix and its inverse. A matrix cache is
## created using the 'makeCacheMatrix' function which
## takes a normal matrix. The inverse of the matrix
## is obtained by calling 'cacheSolve' with the cached matrix.
## The first iteration will calculate the inverse,
## subsequent calls return the cached inverse.

## The 'makeCacheMatrix' function creates a special 'matrix', 
# which is a list of functions which can:
# 1. set the value of the matrix cache
# 2. get the value of the cached matrix
# 3. set the value of the inverse of the cached matrix
# 4. get the value of the inverse of the cached matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## The 'cacheSolve' function takes the result of the
# 'makeCacheMatrix' function and returns the inverse 
# of the cached matrix. It will first check if the
# inverse has already been calculated and cached 
# beforehand and return the cached inverse if so.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}