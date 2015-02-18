## These two functions are designed to compute the inverse of a matrix in a sensible
## way, i.e. it is not computed twice if the inverse is already stored in memory.
## For doing so, you first create a new kind of matrix using makeCacheMatrix, which
## is a list of functions indeed. This "object" can be passed to cacheSolve which
## computes the inverse or just get it from memory
## ------ Usage ----------
## mymat <- matrix(c(1,2,3,0,1,5,5,6,0),nrow=3,ncol=3) ## This is an invertible mat
## cachemat <- makeCacheMatrix(mymat) ## the CacheMatrix 
## inversemat <- cacheSolve(cachemat) ## compute the inverse... or not
## inversemat2 <- solve(mymat)        ## This is the common way of getting the inverse
## inversemat == inversemat2          ## You can check both are equal
## ------ History ----------
## 2015/02/18 ---> created and debugged

## This function gets a matrix and transforms it into a CacheMatrix object
## Inputs : a matrix
## Outputs: a list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function provides the inverted mat if it exists, returns an error otherwise
## If it had already been calculated, then it is retrieved from memory
## Inputs : a CacheMatrix
## Outputs: the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
