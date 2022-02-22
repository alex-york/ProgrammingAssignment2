## Written following the makeVector
## Our first function will be similar to makeVector
## it will set up i as a NULL value.
## This will later store the inverse of our matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # we will use i for the inverse of our matrix
  set <- function (y) { # allows us to set a new matrix x
    x <<- y # set the new matrix as x
    i <<- NULL # if we set a new value we want to erase our inverse
  }
  get <- function () x #Allows us to retrieve the matrix (x)
  set_inverse <- function(solve) i <<-solve 
  #solve (find the inverse) of the matrix
  get_inverse <- function() i 
  #Retrieve the stored inverse value
  list(set=set, get = get,
       set_inverse=set_inverse,
       get_inverse=get_inverse)
}


## We want our funciton to look up if a matrix inverse has already been solved
## If it has been solved it will return the already stored inverse matrix.
## If not it will find the inverse and return it.

cacheSolve <- function(x, ...) {
  #First we look into the Cached matrix to see if the inverse is already cached.
  i <- x$get_inverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  #If not we will solve it.
  data <- x$get()
  i <-solve(data,...)
  x$set_inverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
