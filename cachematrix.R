## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # Define empty object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse #calculate the inverse of m
  getinverse <- function() m #obtain the inverse of m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #store values obtained to a list

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse() # obtain inverse matrix
  if(!is.null(m)) { #if m is not empty, display the cached inverse
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...) # calculate the inverse if the cached info isn't found.
  x$setinverse(m)
  m
}
