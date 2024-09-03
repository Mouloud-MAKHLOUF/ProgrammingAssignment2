##  I created the function makeCacheMatrix that can store the inverse matrix if seted for a given matrix x
## if the inverse matrix is never calculated before it remain NULL

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) im <<- solve
  getinversematrix <- function() im
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
  
}

## cacheSolve get an special object of type makeCacheMatrix (x) and return the inverse matrix of x directly 
## if the inverse matrix of x had been stored before otherwise it's calculate it 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinversematrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinversematrix(im)
  im
}

## Practicle exemple

x <- makeCacheMatrix(matrix(
  
  # Taking sequence of elements  
  c(1, 2, -1, 2), 
  
  # No of rows
  nrow = 2,   
  
  # No of columns
  ncol = 2,         
  
  # By default matrices are in column-wise order
  # So this parameter decides how to arrange the matrix
  byrow = TRUE          
))

x$get()
cacheSolve(x)
