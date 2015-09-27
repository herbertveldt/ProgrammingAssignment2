## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## Its puspose is to store a martix and a cached value of the inverse of the matrix. 
## Returns the following functions:
##   setMatrix  = set the value of a matrix
##   getMatrix  = get the value of a matrix
##   setInverse = set the cached inverse of the matrix
##   getInverse = get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      ## initialize the stored inverse value to NULL
    inv <- NULL 

      ## set the matrix
    setmatrix <- function(y) {
        x <<- y
          ## matrix has assigned, reassign "inv" to NULL
        inv <<- NULL 
    }
    
      ## get value of matrix
    getmatrix <- function() x
    
      ## set inverse of matrix
    setinverse <- function(inverse)
        inv <<- inverse
    
      ## get inverse of matrix
    getinverse <- function() inv
    
      ## return a list containing all functions defined above
    list(setmatrix  = setmatrix, 
         getmatrix  = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse
         )
}


## The function cachesolve computes the inverse of the special "matrix" 
## returned by the function makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed, 
## then the cachesolve should reuse the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## get inverse
    inv <- x$getinverse()
    
      ## if inverse exists, check if already cached
      ## if yes, return cached inverse
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
      ## if not, get matrix
    data <- x$getmatrix()
    
      ## compute inverse of matrix
    inv <- solve(data, ...)
    
      ## cache inverse of matrix
    x$setinverse(inv)
    
      ## return inverse
    inv
}
