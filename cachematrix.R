## Put comments here that give an overall description of what your
## functions do

## This function is used to find the inverse of a matrix
## The unique thing about this function is rather than computing 
## the matrix inverse of the same matrix again and again
## it saves the matrix inverse in cache after the first computation
## and next time it uses this matrix inverse stored in the cache

## Write a short comment describing this function
## The below function is a function to create a special type of matrix
## which is a list of 4 functions - to create a new matrix, retrieve an
## existing matrix, set the inverse of this matrix and lastly retrive 
## the inverse of this matrix which has been saved

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setMatrixInverse <- function(matrixinverse) m <<- matrixinverse
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function
## The below function is the main function that will be called 
## with the argument as the special matrix computed in the previous function.
## It will return the inverse of the matrix.
## This function will use the internal "Solve" function of R to compute the 
## inverse of this matrix the 1st time. But next time when it is called then 
## it will not call this "Solve" function but rather return the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setMatrixInverse(m)
  m
}
