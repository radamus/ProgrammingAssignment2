## Functions are able to cache (and make use of) the result of a matrix inverse 

## Function defines a special matrix that is able to cache its own inverse. 
## Function assumes that argument matrix is solvable

makeCacheMatrix <- function(x = matrix()) {
  solvematrix <- NULL
  set <- function(newmatrix){
    x <<- newmatrix
    solvematrix <<- NULL    
  }
  
  get <- function() x
  setsolve <- function(sm) solvematrix <<- sm
  getsolve <- function() solvematrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function uses makeCacheMatrix to cache solvable matrix inversion for future use

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

