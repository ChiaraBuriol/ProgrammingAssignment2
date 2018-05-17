## In this program we define 2 particular functions that create a special matrix object which caches its inverse:
## A. makecacheMatrix () 
## B. cacheSolve ()


####################### makecacheMatrix ############################
##    1. Create a special matrix object that can cache its inverse
##    2. The function takes the parameter x. x is a matrix
##    3. The function returns a list with 4 functions that set/get the value of the matrix x and then set/get the inverse of this matrix

makecacheMatrix <- function(x = matrix()) {
  
  # Initialize inv_m
  inv_m <- NULL
  
  # Define function set: set the value of the matrix
  set <- function(y) {
    x <<- y              # set the value
    inv_m <<- NULL       # clear the cache
  }
  
  #Define function get: get the value of the matrix
  get <- function() x
  
  #Define function setinverse: set the inverse
  setinverse <- function(inverse) inv_m <<- inverse
  
  #Define functione getinverse: 
  getinverse <- function() inv_m
  
  #Create and return a list with the functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



####################### Function cacheSolve ############################
##  1. Create and return the inverse of matrix x
##     If the inverse has already been calculated the cachesolve gets back the inverse from the cache
##  2. The function takes the parameter x. x is a special matrix created with the function makecacheMatrix


cacheSolve <- function(x, ...) {
  
  # retrieve the cache value for the inverse
  inv_m <- x$getinverse()    
  
  # If the cache is not empty we can return it
  if(!is.null(inv_m)) {
    message("getting cached matrix")
    return(inv_m)
  }
  
  # The cache is empty. So we have to: 
  
  #1. Get value of the matrix
  data <- x$get()
  #2. Calculate the inverse of the matrix
  inv_m <- solve(data, ...)
  #3. Cache the result
  x$setinverse(inv_m)
  #4. Return the inverse of the matrix
  inv_m
}




