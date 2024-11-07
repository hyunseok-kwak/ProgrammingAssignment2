## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Creates an object with functions to set/get the matrix 
## and set/get its inverse, enabling caching.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initialize the inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL # Clear any previously cached inverse
  }
  get <- function() x  ## Define a function to get the matrix
  setinv <- function(inversion) inv <<- inversion  ## Define a function to set the inverse of the matrix
  getinv <- function() inv ## Define a function to get the cached inverse of the matrix
  
  ## Return a list of the above functions to access them externally
  list (set = set, get = get,
        setinv = setinv, getinv = getinv)
}

## cacheSolve: Computes the matrix inverse, 
## checking if it’s already cached to avoid redundant calculations. 
## If cached, it retrieves the inverse; otherwise, it calculates and stores it in the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ## If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) 
  }
  ## Otherwise, calculate the inverse, cache it, and return it
  data <- x$get() # Get the original matrix
  inv <- solve(data, ...) # Compute the inverse
  x$setinv(inv) # Cache the computed inverse
  inv # Return the inverse
}