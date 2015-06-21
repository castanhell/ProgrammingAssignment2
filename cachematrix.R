## Put comments here that give an overall description of what your
## functions do

## Make cache matrix
## Based on Roger Peng code for makeMean

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    #setmean() turns setinverse
    setinverse<- function(inverse) i <<- inverse
    #getmean() turns setinverse
    getinverse<- function() i
    list(set = set, get = get,
         getinverse = getinverse,
         setinverse= setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i 
}

#An example
#This is an invertible matrix
m <- c(1,0,5,2,1,6,3,4,0)
m <- matrix(m,nrow=3,ncol=3)
x <- makeCacheMatrix(m)
cacheSolve(x)
cacheSolve(x)