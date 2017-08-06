## Function pairs that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {   ##Function pairs that cache the inverse of a matrix
  i <- NULL         ##Initialization
  set <- function(matrix) {  ##Setting m as a matrix
    m <<- matrix
    i <<- NULL
  }
  {
  get <- function() 
    m  ##Return matrix m
  } 
  setInverse <- function(inverse){   ##Setting inverse of the matrix
    i <<- inverse
  }
  getInverse <- function() {     ##Getting the inverse of the matrix
    i  ##Returning inversed matrix
  }
  list(set = set, get = get,       ##Returning a list of methods
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve will retrieve inverse from cache
cacheSolve <- function(x, ...) {    
  m <- x$getInverse()  ##Matrix that is inverse of m will be returned
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) %*% data
  x$setInverse(m)       ##Inverse set to the object data
  m  ##Return matrix
}
