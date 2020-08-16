## Put comments here that give an overall description of what your
## functions do
# Below are the two functions that are used to create a special object that stores a matrix and caches its inverse 

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to 
# 1. set the values of the matrix 
# 2. get the values of the matrix 
# 3. set the inverse 
# 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize matrix with NULL 
  m <- NULL
  # set the values of the matrix 
  set <- function(y){
    x <<- y 
    m <<- NULL
  }
  # get the values of the matrix 
  get <- function() x
  # set the inverse matrix 
  setinverse <- function(inverse) m <<- inverse
  # get the inverse matrix 
  getinverse <- function() m
  # list containing the function above  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
# cacheSolve calculates the inverse of the matrix created with the function makeCacheMatrix 
# This function checks to see if the inverse has already been calculated 
# If so, this function gets the inverse from the cache and skips calculating the inverse
# If not, it calculates the inverse and sets it in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # check if it is null 
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  # if it is null, get the matrix and calculate inverse 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
