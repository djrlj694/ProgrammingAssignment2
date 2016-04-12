################################################################################
# FILE:   cachematrix.R
# AUTHOR: Robert L. Jones (RLJ)
#
# ABSTRACT:
# This 2nd programming assignment requires the student to write an R function
# that is able to cache potentially time-consuming computations. For example,
# taking the inverse of matrix may take too long to compute, especially if it is
# large and has to be computed repeatedly (e.g., in a loop). If the contents of
# a matrix are not changing, it may make sense to cache the inverse so that when
# we need it again, it can be looked up in the cache rather than recompute it.
# In this Programming Assignment, the student takes advantage of the scoping
# rules of the R language and how they can be manipulated to preserve state
# inside of an R object.
#
# DATE:      AUTHOR:  COMMENT:
# 11APR2016  RLJ      Initial creation
################################################################################


################################################################################
# FUNCTION: makeCacheMatrix
# 
# ABSTRACT:
# Creates a special "matrix", which is really a list containing a function to
# 1. Set the value of the matrix;
# 2. Get the value of the matrix;
# 3. Set the value of the matrix inverse;
# 4. Get the value of the matrix inverse.
#
# ARGS:
# x: A numeric matrix
#
# RETURNS: ???
################################################################################

makeCacheMatrix <- function(x = matrix()) {
  
  # Instantiate an empty matrix.
  i <- NULL
  
  # Declare function to set the value of matrix. 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  # Declare function to get the value of matrix. 
  get <- function() x
  
  # Declare function to set the value of the matrix inverse.  
  setinverse <- function(inverse) i <<- inverse

  # Declare function to get the value of the matrix inverse. 
  getinverse <- function() i
  
  # Return a list containing the above functions.
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
    )
}


################################################################################
# FUNCTION: cacheSolve
# 
# ABSTRACT:
# Calculates the inverse of the special "matrix" created with the
# makeCacheMatrix function. However, it first checks to see if the inverse has
# already been calculated. If so, it gets the inverse from the cache and skips
# the computation. Otherwise, it calculates the inverse of the matrix and sets
# the inverse in the cache via the setinverse function.
#
# ARGS:
# x:   A numeric matrix
# ...:
#
# RETURNS: A numeric matrix that is the inverse of 'x'
################################################################################

cacheSolve <- function(x, ...) {
  
  # Return the matrix inverse if it was cached.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data.")
    return(i)
  }
  
  # Compute the matrix inverse.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  # Return the matrix inverse.
  i
}
