## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "vector", which is containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function calculates the inverse of special matrix created with makeCacheMatrix function
## It first checks to see if the inverse has already been calculated 
## If so, it gets the inverse from the cache and skips the computation 
## Otherwise, it calculates the inverse of the matrix
## Then, sets the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix)
  x$setinverse(i)
  i 
}
