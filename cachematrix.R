## Week3 assignment: calculate the inverse of a matrix
## and use a simple cache algorithm for performance reasons
#
# Usage Example:
# > m <- matrix(rnorm(4), nrow = 2)   // Create the dataset
# > cm <- makeCacheMatrix(m)          // Create the cached object
# > cm$get()                          // get the data
# > n <- cacheSolve(cm)                    // calculate inverse
# > n <- cacheSolve(cm)                    // calculate inverse (get the inverse with cache)
# > if so, round(m%*%n) is the identity matrix   
#
## NOTE: don't forget to change the (1) current-dir and (2) load the source file
#  comnmands: (1) setwd (2) source 



## Creates a closure to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # object modifier/accesor methods -------------
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){ 
    x
  }
  
  # inverse modifier/accessor methods ------------
  setinverse <- function(inverse){ 
    m <<- inverse
  }
  getinverse <- function(){ 
    m
  }
  # closure object assembly ----------------------
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}


## Solves the inverse of a matrix given a cacheMatrixObject

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  # if inverse is already calculated, return it from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # caches the calculated data
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  # return data
  m
  
}
