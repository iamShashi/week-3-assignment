## Put comments here that give an overall description of what your
## functions do

## make cache matrix function recieves a matrix sets min to null caches the inverse matrix
## then the other functions as are just to get values and are to cache data 

makeCacheMatrix <- function(x = matrix()) {

    minv <- NULL
    set <- function(y) {
      x <<- y
      minv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) minv <<- inverse
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## this matrix first checks for the inverse existence if it does that is it was cached and the getting cache
##cached data is published or else inverse of matrix is found using the function solve
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setinv(minv)
  minv
}
