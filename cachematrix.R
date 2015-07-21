## These are a pair of functions that caches the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
matinv<-NULL
  set <-function(y) {
    x<<-y
    matinv<<-NULL
  }
  get<-function() x
  setinv <- function(inv) matinv <<- inv
  getinv<-function() matinv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
matinv<-x$getinv()
  if(!is.null(matinv)) {
      message("getting cached mat inverse")
      return(matinv)
  }
  data<-x$get()
  matinv<-solve(data)
  x$setinv(matinv)
  matinv
}
