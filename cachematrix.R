## With the combination of following two functions, it is able to cache potentially time-consuming computations. 
## Specifically, it realizes the function that caches the inverse of the matix and gets it without
## recomputations each time.

## makeCacheMatrix function:
## makeCacheMatrix returns a list, which contains four functions:
## 1. set(): set the actual value of the matrix
## 2. get(): get the actual value of the matrix
## 3. setinverse(): set the inverse matrix of the matrix
## 4. getinverse(): get the inverse matrix of the matrix
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
       setinverse = setinverse ,
       getinverse = getinverse)
}
 
## cacheSolve  function:
## cacheSolve returns a matrix that is the inverse of x
## if the value of the inverse matrix has already been calculated and set, then it prints out
## a message, get that value directly without computation and returns it;
## otherwise, it calcuates the inverse matrix, sets it in the cache and returns the value
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i ## Return a matrix that is the inverse of 'x'
}
