## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #set matrix inverse
  inverse <- NULL
  
  ##create a function
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## assign fn that stores matrix
  get <- function() x
  
  ## assign fn that replace inverse
  setinverse <- function(matinverse) inverse <<- matinverse
  
  ##assign fn to retreive current value of "inverse"
  getinverse <- function() inverse
  
  ##return a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## used $ to get elements
  inverse <- x$getinverse()
  
  ##if the mean is not null then cacheinverve will calculate inverse matrix, return will skip all the code
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## if the mean is null, retreive the get function from input vector
  matrixdata <- x$get()
  
  ## solved the inverse
  inverse <- solve(matrixdata)
  
  x$setinverse(inverse)
  
  inverse
        ## Return a matrix that is the inverse of 'x'
}
