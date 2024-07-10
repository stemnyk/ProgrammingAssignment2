makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) M <<- inverse
  getinverse <- function() M
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(X, ...) {
  M <- X$getinverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  matrix <- X$get()
  M <- solve(matrix, ...)
  X$setinverse(M)
  M
}

mymatrix <- matrix(c(1, 2, 3, 4), 2, 2)
print(mymatrix)
