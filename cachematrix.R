##The function, makeCacheMatrix creates a special matrix, which is really a list containing a function to

##set the matrix
##get the matrix
##set the inverse
##get the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve()
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## The following function calculates the inverse of the special "matrix" created with the above function. 
#First checks if the inverse has already been calculated. If Yes, then the function takes that inverse. 
#Otherwise, it calculates the inverse of the data and sets the inverse in the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return (inv)
 }
 
