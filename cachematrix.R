## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inversem <- NULL
  set <- function(y){
    x <<- y
    inversem <- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inversem <<- inverse
  getInverse <- function() inversem
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversem <- x$getInverse()
  if(!is.null(inversem)){
    message('getting cache data')
    return(inversem)
  }
  m <- x$get()
  inversem <- solve(m, ...)
  x$setInverse(inversem)
  inversem
}
