##we create 2 functions: makeCacheMatrix, to create a matrix that can be cached,
## and cacheSolve, that can invert a matrix or retrieve its inverse

## The function makeCacheMatrix creates a special "matrix" 
##object that can cache its inverse


makeCacheMatrix <- function(X = matrix()) {
  inv <- NULL
  set <- function(Y){
    X <<- Y
    inv <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}




##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse is already there,
##then the cacheSolve function will get the inverse from the cache.

cacheSolve <- function(X, ...) 
{
  
  inverse <- X$getinverse()
  if(!is.null(inverse)){
    message("matrix is already in the cache")
    return(inverse)
  }
  message("inverse is not in the cache so we will compute the inverse")
  tobeinverted <- X$get()
  inverse <- solve(tobeinverted)
  X$setinverse(inverse)
  inverse
}
