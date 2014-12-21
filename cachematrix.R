## This is  code to cache inverted matrix results given a matrix x

## Exactly the same as get mean function, m is now the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  #m is resulant matrix, initially set to NULL
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  #Sets values of the original matrix in the outside enviornment
  get <- function() x
  #get x
  
  setinv <- function(inv) m <<- inv
  #sets m of the outside enviormnet to the calculated inverse
  
  getinv <- function() m
  #gets the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  #returns the list of functions
}


## The arguemen x below is the list created by above function

cacheSolve <- function(x, ...) {
    ##m is inverse matrix of x

  m <- x$getinv()
  #checks if cached value for x exists
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  
  #otherwise calculates it below:
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
