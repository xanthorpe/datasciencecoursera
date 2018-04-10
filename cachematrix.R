## We are creating an R function which is able to cache potentially
## time-consuming computations. This is useful for cases where the contents of
## the vector are not changing. 

## The following function creates a list containing a function to :-
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the matrix inverse of the list created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
   return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Example
## mat <- matrix(c(1,3,5,6), nrow=2, ncol=2, byrow=TRUE)
## > mat1 <- makeCacheMatrix(mat)
## > mat
##      [,1] [,2]
## [1,]    1    3
## [2,]    5    6
## > cacheSolve(mat1)
##            [,1]       [,2]
## [1,] -0.6666667  0.3333333
## [2,]  0.5555556 -0.1111111
## 
##
