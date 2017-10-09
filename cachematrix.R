## These functions illustrate the cache functionality of R by using the
## <<- operator to allow the access of memory outside the current
## scope

## This function creates a matrix data structure and instantiates
## the member functions which will be used to perform operations upon it
## set, get, setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## This function will check to see if an inverse has already been
## solved for the given matrix.  If it has, it will output the result
## if not, it will compute the inverse with solve()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
