makeCacheMatrix <- function(X = matrix()) {
  # Initialize the cache for the inverse matrix
  M <- NULL
  
  # Define the set function to update the matrix and reset the cache
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  
  # Define the get function to retrieve the current matrix
  get <- function() X
  
  # Define the setinverse function to cache the inverse of the matrix
  setinverse <- function(inverse) M <<- inverse
  
  # Define the getinverse function to retrieve the cached inverse
  getinverse <- function() M
  
  # Return a list of the above functions to manage the matrix and its inverse
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(X, ...) {
  # Retrieve the cached inverse if it exists
  M <- X$getinverse()
  
  # If the cached inverse is found, return it with a message
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  
  # Otherwise, get the matrix, calculate its inverse, cache the result, and return it
  matrix <- X$get()
  M <- solve(matrix, ...)
  X$setinverse(M)
  M
}

# Example usage:

# Create a 2x2 matrix
mymatrix <- matrix(c(1, 2, 3, 4), 2, 2)
print(mymatrix)
