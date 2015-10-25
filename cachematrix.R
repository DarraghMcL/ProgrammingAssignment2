## Put comments here that give an overall description of what your
## functions do

##These functions are used to calculate the inverse of a matrix. When the inverse is calculated the value will be cached for quick retrieval later.

## Write a short comment describing this function

#This function creates a list of 'getter' and 'setter' functions to assist in storing the inverse of the provided matrix
makeCacheMatrix <- function(x = matrix()) {
  
  #placeholder for the inverse of the matrix
  m <- NULL
  
  #setter function - sets the input value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #getter function - gets the input value
  get <- function() x
  
  #setter function - sets the inverse value of the matrix
  setInverse <- function(inverse) m <<- inverse
  
  #getter function - gets the inverse value of the matrix
  getInverse <- function() m
  
  #returns a list of all of the inclosed functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

#This function returns the inverse of the suplied matrix. If the inverse has already been solved it will pull the value from the cached location
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Checks if the inverse has already been calcualted. If it has then just return the pre-calculated value
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Gets the input matrix and sets to 'data'
  data <- x$get()
  
  #calculates the inverse of the matrix
  m <- solve(data, ...)
  
  #Stores the value of the inverse matrix to the cache
  x$setInverse(m)
  
  #returns the inverse of the matrix
  m
}
