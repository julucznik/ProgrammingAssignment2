
## The makeCacheMatrix allows you to create a matrix that is actually a list containing functions enabling you to set the value of
## the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse

	
makeCacheMatrix <- function(x = matrix()) 
{
  
  ## Set the cached inverse(i) to null  
  i <- NULL
  
  ## Sets the internal matrix(x) to the value supplied by the argument. 
  ## THe inverse(i) is reset to NULL
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  
  ## Gets the matrix(x)
  get <- function() x
  
  ## Sets the cached inverse(i)
  setinverse <- function(inverse) i <<- inverse
  
  ## Gets the cached inverse(i)
  getinverse <- function() i
  
  ## List that exposes the methods available
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
	
## cacheSolve calculates the mean of the matrix created with makeCacheMatrix. It first checks whether an inverse has been calculated. 
## If it has, it gets the inverse from the cachce and skips computation. Otherwise, it calculates the inverse and the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) 
{
  
  ## Gets the inverse(i)
  i <- x$getinverse()
  
  ## if the cache exists return cache
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  
  ## if the cache does not exist solve the inverse and then save in cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
