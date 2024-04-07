## Put comments here that give an overall description of what your
## functions do

##First of all, i will demonstrate how to calculate the invertion a matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##VERY IMPORTANT: The result of makeCacheMatrix MUST be stored in a variable
##in order for the next function to work. This is something that was not
##very clear to me as i read the assessment.

a <- makeCacheMatrix(974)

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##In order to check if the function REALLY returns the inversion of a matrix,
## i'll teste with solve(974). Here is important to know that, if we
## don't specify the number of rows and columns and give just one element
## to the function, it will assume it a matrix of dimensions 1X1 with
## ONLY the number we passed to the function.

solve(974)

#[,1]
#[1,] 0.001026694