
## These 2 functions are Programming asign 2
#  Caching excersise.

## makeCacheMatrix
## This creates a special matrics which includes other attributs

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function (y) {
    x<<- y
    i<<- NULL
  }
  get <- function () x
  setInv <- function (inv) i<<- inv
  getInv <- function () i
  list (set=set, get = get, setInv = setInv, getInv = getInv )
}

## Cache Solve
## This function checks if the inverse of a matrix
## has already been calculated
## If it has, it returns the cached
## If not, it calcs, saves to cache and retrns

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)){
##    message ("getting cashed")
    return (inv)
  }
    inv<-solve(x$get())
    x$setInv (inv)
    inv
}

##########################################
##Example Code
makeVector <- function(x = numeric()){
  m <- NULL
  set <- function (y) {
    x<<- y
    m <<- NULL
  }
  get <- function () x
  setmean <- function (mean) m<<- mean
  getmean <- function () m
  list (set=set, get = get, setmean = setmean, getmean = getmean )
}

cachemean <- function(x, ...){
  m <- x$getmean()
  if (!is.null(m)){
    message ("getting cached data")
    return (m)
  }
  data <- x$get()
  m<-mean(data,...)
  x$setmean(m)
  m
}
