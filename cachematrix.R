#Below function will do -
#Function - I Creates a "Matrix" object that can cache its inverse####
#Function - II Computes conditionally inverse of the matrix returned by Function I.####


##Function I####
#1) Set the value of the vector
#2) Get the value of the vector
#3) Set the value of the inverse
#4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinv <- function() invrs <<- solve(x)
  getinv <- function() invrs
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function II####
#Conditionally compute inverse.Skip if already calculated

cacheSolve <- function(x, ...) {
  invrs <- x$getinv()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get
  invrs <- solve(data, ...)
  x$setinv(invrs)
  invrs
}

# Clean Up####

# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L
