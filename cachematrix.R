## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function setters and getters for a matrix in cache and its inverse  

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## Write a short comment describing this function
#The function uses makeCacheMatrix to return the cached inversed matrix that is already exists in cache skipping its calculation, or if it does not exist it calculates the matrix inverse and stores it in the cache. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
    if(!is.null(j)){
      message("getting cached data")
      return(j)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
  j
}

