## Overall Functions Description:

# The following two functions create a special "matrix" object that can cache its 
# inverse and computes the inverse of the special "matrix" returned by the first 
# function. If the inverse has already been calculated (and the matrix has
# not changed), then the cachesolve will retrieve the inverse from the cache. 



## makeCacheMatrix Function:
# The <<- operator is used to assign a value to an object in an environment 
# that is different from the current environment. Below is a function that 
# is used to create a special object that stores a Matrix and cache's 
# its inverse.


makeCacheMatrix <- function(x = matrix()) {
        
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
  

## cacheSolve function:
# The following function calculates the inverse of the special "Matrix" created with 
# the above function. However, it first checks to see if the Inverse has already been
# calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
# the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix)   ## Return a matrix that is the inverse of 'x'
    x$setinverse(m)
    m 
  }
