## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of functions will cache the inverse of a square matrix.
## Note that not all square matrices have matrix inverses, although most do.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(),...) {
  inv <- NULL
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated then 
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  matrixdata <- x$get()
  inv <- solve(matrixdata,...)
  x$setinv(inv)
  inv
}


