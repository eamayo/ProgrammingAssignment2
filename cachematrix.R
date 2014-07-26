## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. The two functions
## below provide support to cache the computation of the inverse of a matrix.

## makeCacheMatrix creates a special object wrapping a matrix. It is really just a list containing 
## the functions below:
## 1. set(): sets the value of the matrix. Whenever set() is called it sets modifiedSinceLastCompute to TRUE
##           indicating that the matrix object has been modified since inverse was last computed
## 2. get(): gets the value of the matrix
## 3. setInverse(): sets the value of the inverse. Whenever setInverse() is called it also sets 
##                  modifiedSinceLastCompute to TRUE indicating that the matrix object has been 
##                  modified since the inverse was last computed
## 4. getInverse(): gets the value of the inverse
## 5. modifiedSinceLastInverseUpdate(): checks if the value of the matrix has been modified since the inverse
##                            was last computed

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      modifiedSinceLastCompute <- FALSE
      
      # set the valueof the matrix
      set <- function(y) {
        if (nrow(y) == ncol(y)) {
            x <<- y
            inv <<- NULL
            modifiedSinceLastCompute <<- TRUE
        }
        else {
            stop("matrix must be square!!")
        }
      }
      
      get <- function() x
      
      # set the value of the inverse and indicate that the stored inverse corresponds
      # to the value of the wrapped matrix object
      setInverse <- function(inverse) {
        modifiedSinceLastCompute <<- FALSE
        inv <<- inverse
      }
      
      # get the cached value of the inverse
      getInverse <- function() inv
      
      # check if value of matrix has changed since inverse was last computed
      modifiedSinceLastInverseUpdate <- function() modifiedSinceLastCompute
      
      # return the special object wrapping x
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse,
           modifiedSinceLastInverseUpdate = modifiedSinceLastInverseUpdate)

}


## This function takes a special "matrix" object created using the makeCacheMatrix() function
## above and returns the cached matrix inverse if it exists and is valid, otherwise it 
## returns the result of a fresh computation

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    
    # return cached value if it exists and is valid
    inv <- x$getInverse()
    if(!is.null(inv) & !x$modifiedSinceLastInverseUpdate()) {
      message("getting cached data")
      return(inv)
    }
    
    # otherwise re-compute the inverse and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    # return the updated inverse of x
    inv
}

## I ran the smoke tests below to make sure things work as expected
# > source("cachematrix.R")
#
# > test <- matrix(c(3,0,0,0,3,0,0,0,3), nrow=3,ncol=3)
#
# > testp <- makeCacheMatrix(test)
#
# > cacheSolve(testp)
# [,1]      [,2]      [,3]
# [1,] 0.3333333 0.0000000 0.0000000
# [2,] 0.0000000 0.3333333 0.0000000
# [3,] 0.0000000 0.0000000 0.3333333
##
## Test that caching works
#
# > cacheSolve(testp)
# getting cached data
# [,1]      [,2]      [,3]
# [1,] 0.3333333 0.0000000 0.0000000
# [2,] 0.0000000 0.3333333 0.0000000
# [3,] 0.0000000 0.0000000 0.3333333
#
# > cacheSolve(testp)
# getting cached data
# [,1]      [,2]      [,3]
# [1,] 0.3333333 0.0000000 0.0000000
# [2,] 0.0000000 0.3333333 0.0000000
# [3,] 0.0000000 0.0000000 0.3333333
##
## Test that cacheSolve() returns expected value
#
# > solve(test) == cacheSolve(testp)
# getting cached data
# [,1] [,2] [,3]
# [1,] TRUE TRUE TRUE
# [2,] TRUE TRUE TRUE
# [3,] TRUE TRUE TRUE
## 
## Check that things work as expected even when the underlying
## value of the matrix object is changed
##
# > test2 <- matrix(c(4,0,0,0,4,0,0,0,4), nrow=3,ncol=3)
# > test
# [,1] [,2] [,3]
# [1,]    4    0    0
# [2,]    0    4    0
# [3,]    0    0    4
#
# > testp$set(test)
#
# > cacheSolve(testp)
# [,1] [,2] [,3]
# [1,] 0.25 0.00 0.00
# [2,] 0.00 0.25 0.00
# [3,] 0.00 0.00 0.25
#
# > cacheSolve(testp)
# getting cached data
# [,1] [,2] [,3]
# [1,] 0.25 0.00 0.00
# [2,] 0.00 0.25 0.00
# [3,] 0.00 0.00 0.25
#
# > solve(test) == cacheSolve(testp)
# getting cached data
# [,1] [,2] [,3]
# [1,] TRUE TRUE TRUE
# [2,] TRUE TRUE TRUE
# [3,] TRUE TRUE TRUE
