# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.


# This function creates a special "matrix" object that can cache its inverse.

# This special matrix creates a list containing functions to:
# 1. set the value of a matrix
# 2. get the value of a matrix
# 3. set the value of the inverse of a matrix
# 4. get the value of the inverse of a matrix


makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
             x <<- y
             inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv

      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

# For this function, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...){
      inv <- x$getinv()
      if (!is.null(inv)){
          message ("getting cached data")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv  
}
