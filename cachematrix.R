## Functions allowing to cache values of the inverse matrix 


## Function "makeCacheMatrix" creates an object (matrix object) that has 4 "subfunctions".
## .$set = set the matrix
## .$get = returns the matrix
## .$setInv = set the inverse matrix
## .$getInv = returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set_int <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_int <- function() x
  setInv_int <- function(solveX) m <<- solveX
  getInv_int <- function() m
  list(set = set_int, get = get_int,
       setInv = setInv_int,
       getInv = getInv_int)
}


## Functin "cacheSolve" first tests if inverse matrix exists. If the inverse matrix exists,
## then it is returned, otherwise it calls the R-function solve (a function that computes
## the inverse matrix) and set the inverse matrix. For getting (testing if the inverse
## matrix exist) and setting the inverse matrix, it uses the "makeCacheMatrix" subfunctions.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
