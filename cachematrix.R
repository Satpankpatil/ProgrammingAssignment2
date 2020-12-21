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

check <- makeCacheMatrix()
check$set(matrix(1:4, 2))
check$get()

check$setinv()
check$getinv()
ls(environment(check$set))

## Function II####
#Conditionally compute inverse.Skip if already calculated

cacheSolve <- function(x, ...) {
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}

# Clean Up####

# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L