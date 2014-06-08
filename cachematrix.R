## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix() and cacheSolve() functions let create a matrix
# which can set/get its value and inverse; inverse can be either 
# computed or retrieved from the cache with function cacheSolve()
# if it already exists for the object created with makeCacheMatrix()

## Write a short comment describing this function

## Function makeCacheMatrix() creates an object(matrix) which
## has four "methods":
##    1+2)  set and get, which in fact are used to set
##          and get the value of the matrix
##    3+4)  setinv and getinv, which are used to set
##          and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   # set "method": set the matrix
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   # get "method": returns the matrix
   get <- function() x
   # setinv "method": set the inverse of the matrix
   setinv <- function(solve) m <<- solve
   # getinv "method": get the inverse of the matrix
   getinv <- function() m
   # create a list containing the "methods"
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Write a short comment describing this function

## Function cacheSolve() gets as argument a matrix created with
# makeCacheMatrix() function and returns the inverse of the argument
# by inverting the matrix or getting the inverse of that matrix in case
# it is already cached

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   # get the inverse of 'x', which may be null or not null
   m <- x$getinv()
   # if the inverse of 'x' is not null, the cached inverse 
   # is returned as the result of cacheSolve() function
   if(!is.null(m)) {
      message("getting cached inverse data")
      return(m)
   }
   # otherwise the matrix to be inverted is retrieved
   message("Inverting matrix")
   data <- x$get()
   # the matrix is inverted and the inverse is assigned to 'm'
   m <- solve(data, ...)
   # the inverse of the matrix is set by the proper setinv "method"
   x$setinv(m)
   # the inverse 'm' is returned as the result of cacheSolve() function
   m
}