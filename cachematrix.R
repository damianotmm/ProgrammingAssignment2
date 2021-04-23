## The two functions listed below use the scoping rules and the environment concept 
## to save the inverse of a matrix in the global environment (cache)
## and thus avoid recalculating it if it is available

## This function creates a functions list to set and get a matrix, 
## to set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(N) {
    x <<- N
    inv <<- NULL
  }
  get <- function() x
  setInv <- function() inv <<- solve(x) 
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## the function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv()
  inv
}
