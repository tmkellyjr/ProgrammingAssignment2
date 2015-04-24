## Matrix Inverse for invertible (square Matrices)
## Tom Kelly April 23, 2015
## These two functions calculate, store and retreive the inverse of a matrix

## This Function Caches (stores) a matrix (a square matrix is assumed)
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # clears the inv matrix first time through
  set <- function(y) {  # creates or changes the matrix stored in the main
    x <<- y  #Sets the x matrix in the parent environment to y
    inv <<- NULL # clears the inverse
  }
  get <- function() x  #return matrix x stored in the parent
  setsolve <- function(inverse) inv <<- inverse #sets the inverse
  getsolve <- function() inv #retrieves the inverse matrix
  ## the following line loads the functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
#
# Sample 2 x 2 matrix created called ttt
# > ttt<-matrix(c(4,2,7,6), nrow=2)
# > ttt
# [,1] [,2]
# [1,]    4    7
# [2,]    2    6



## This function caches (stores) a matrix and calculates the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getsolve() # loads the inverse if there is none then NULL
  if(!is.null(inv)) {
    message("getting cached data") # if the inv was stored then retreive
    return(inv) #return the stored inverse
  }
  data <- x$get() #retrieves the matrix and places into data matrix
  inv <- solve(data) #calculates the inverse
  x$setsolve(inv) #stores the inverse of the matrix 
  inv # returns the inverse
}

##Sample:
#> inv<-cacheSolve(x)
# > inv
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > inv<-cacheSolve(x)
#  getting cached data
# > inv
# [,1] [,2]
# [1,]  0.6 -0.7
#[2,] -0.2  0.4
 
