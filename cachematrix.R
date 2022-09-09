## these functions cache the inverse of a matrix
## inside of the matrix

## Creates a matrix in which to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
  x<<- y
  m <<- NULL
}
get<- function()x
setInverse<- function(inverse)m<<- inverse
getInvers<-function()m
list(set = set, get= get, setInverse= setInverse,
     getInverse= getInverse)
}


## Calculates the inverse of makeCacheMatrix if
## that solution is not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
