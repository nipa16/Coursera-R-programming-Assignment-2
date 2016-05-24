 
##The following two functions cache the inverse value of a given matrix in order to achieve faster computation. 
##If the input matrix is already existed int the system, then the inversion value from the cache will be returned. 
##Otherwise, it will perform matrix inversion operation on the given matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}
#The "makeCacheMatrix" function creates a matrix object that can set/get the value of a matrix object 
#and also can set/get the inverse value of a matrix object. It preassumes that the matrix is invertible.



cacheSolve <- function(x, ...) {
      m <- x$getinv()
    if(!is.null(m)) {
      print("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
    
  }
## The CacheSolve function checks whether the entered matrix is new or the same that stored in the "m". If it is new
## then CacheSole calculate inverse of the matrix by using solve( data,...); otherwise it cache the the inverse value
## form the cache




