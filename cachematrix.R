#a funtion to create a matrix and caches the inverse
makeCacheMatrix <- function(x = matrix()) {
  #empty element to initialise the invertion
  m <- NULL
  #set the matrix
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  #get and return the matrix
  get <- function(){x} 
  #set the inverse of the matrix
  setInv <- function(inverse){m <<- inverse} 
  #get the inverse of the matrix
  getInv <- function(){m}
  #gives a list of all the above funtions and operations
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

#funtion to retrieve the inverse that was calculated in makeCacheMatrix.R
#from the cache
cacheSolve <- function(x, ...) {
  #return the inverse of the matrix
  m <- x$getInv()
  #or just return the inverse if it has already been determined
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #get the matrix from the cache
  data <- x$get()
  #calculate the inverse, %*% signifies matrix multiplication
  m <- solve(data) %*% data
  #set inverse matrix to the object 
  x$setInv(m)
  #return the matrix
  m
}
