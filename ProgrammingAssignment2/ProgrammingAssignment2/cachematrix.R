## This .R file contains two functions.
## 1. makeCacheMatrix()
##    This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve()
##    This function 

## makeCacheMatrix function creates a special list which contains four functions.
##     list(set, get, set.inv, get.inv)
## parametres:
## set : set the value of the matrix.
## get : get the value of the matrix
## set.inv : set the value of the inverse
## get.inv : get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
        {
            x <<- y
            m <<- NULL
        }
    get <- function() x
    set.inv <- function(solve) m <<- solve
    get.inv <- function() m
    list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## Write a short comment describing this function

cacheSolve <- function( x = matrix(), ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$get.inv()
  ## the following section judges if the matrix has been cached
  ## cached m returns a value other than NULL and the cached value
  ## is returned without further calculation.
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  ## the following section operation when is.null(m)==TRUE
  ## that is to say that m has not been cached before.
  data <- x$get()
  m <- solve(data, ...)
  x$set.inv(m)
  m
    
}
