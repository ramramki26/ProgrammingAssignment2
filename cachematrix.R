## Comments on overall working of the two functions:
## makeCacheMatrix initialises the matrix to be cached and contains 
## the functions to set the data to be accessed by the cacheSolve func
## -tion which then manipulates that data to obtain the inverse of the 
## given matrix either by computing it or retrieving it.


## Comments on makeCacheMatrix:
## makeCacheMatrix initialises two empty matrices, one is an argument 
## to be supplied by the user, the other is the inverse. These are
## manipulated using get,setinverse,getinverse methods by the
## cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
m <- matrix()
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Comments on cacheSolve:
## cacheSolve first sets the previous value of inverse. If it was calc
## -ulated before, the value would be retrieved and the if condition
## would be true leading to the cached data being printed. If the
## input is new, the set value would be a matrix of NAs and hence the
## if condition would be false causing the code to be skipped and the
## inverse to be computed and then returned.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
  if(any(!is.na(m))) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
