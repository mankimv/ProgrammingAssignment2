## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix() works in tandem with cacheSolve(), delivering the inverse of a matrix
##either fresh if no value cached by previous run
##or extracting from cache if already calculated

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix generates the list of 4 functions:
## set(), get(), setinv(), getinv()
## which will be called from cacheSolve() below
## feeding makeCache() requires definition of a matrix to be inversed, like
## > m<-matrix (1:4,2,2)
## then define a variable to store the list of 4 functions, like
## > a<-makeCacheMatrix(m)
  i <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve() gets input matrix from makeCacheMatrix()
## and either: calculates the inverse by solve() function
## or returns the cached inverese value if already calculated
## triggering cacheSolve () with the variable "a" from above
## > cacheSolve(a)
## makes cacheSolve calculate an inverse matrix
## for the 2nd time though it returns the cached value ("getting cached data message")

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve (data, ...)
  x$setinv(i)
  i
}
  
