## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a caches a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function(){
    inv
  }
  return(list(set=set,get=get,setInverse = setInverse, getInverse=getInverse))
}


## Write a short comment describing this function
## This functions returns the inverse of a square cache matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  Matrix <- x$get()
  inv <- solve(Matrix,...)
  x$setInverse(inv)
  return(inv)
  
}
