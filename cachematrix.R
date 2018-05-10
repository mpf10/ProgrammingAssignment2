## The code computes for the inverse of a matrix. 
## It also determines if a matrix has an inverse.

## The makeCacheMatrix get and set the value of the matrix.
## It also contains the function to get and set the value of the 
## inverse which will be computed on the second function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## the cacheSolve function determine if the inverse is already saved in the memory
## else it will compute for the inverse when a new value of x is passed to the function.
## it also contains a condition where it will determine if the matrix has an inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("getting the cached data")
    return(i)
  }
  else if (det(x$get())==0){
    message("No inverse: The determinant is equal to 0.")
  }
  else {
    ma <- x$get()
    inv <- solve(ma) 
    x$setinv(inv)
    inv
  }
}
