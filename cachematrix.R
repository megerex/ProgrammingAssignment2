## This .R file contains two functions.
## 1. makeCacheMatrix()
##    This function creates a special "matrix" object that can cache its data.
## 2. cacheSolve()
##    This function inverse/solve a matrix.
## This R code file is written as an assignment of R programming 2015 Jan coursera course of John Hopkins Univ. 

makeCacheMatrix <- function(x = matrix()) 
{   
    ## makeCacheMatrix function creates a special list which contains four functions.
    ## makeCacheMatrix() has 1 parameter. 1 return value.
    ## parameter list...
    ## x : parameter x initiate a matrix, and it is in default an empty matrix.
    ## return value list...
    ## list(set, get, set.inv, get.inv)
    ##     set     : set the value of the matrix.
    ##     get     : get the value of the matrix
    ##     set.inv : set the value of the inverse
    ##     get.inv : get the value of the inverse 
  
    m <- NULL
    ## m is predefined as the container to store whatever function
    ## passed to set.inv() as its parameter. 
    set <- function(y) 
        {
            ## set() is a function that takes a matrix and store it.
            ## in the meantime, set clear up function container m.
            ## the stored matrix can be fetched by get().
            ## set() has 1 parameter. 1 return value.
            ## parameter list...
            ## y : parameter y is expected to be a matrix object.
            ## return value list...
            ## m : returned value m is a function container set to m <<- NULL.
            
            x <<- y
            ## parameter y is stored in x which can be fetched by get().
            
            m <<- NULL
            ## function container m is cleared to NULL here so that each time
            ## set is run, set.inv() can be used later to redefine the function 
            ## to be used in processing the target matrix y.
        }
    
    get <- function() x
    ## get() is a one line function that fetches the matrix set() stored.
    ## get() has 0 parameter. 1 return value
    ## return value list...
    ## x : returned value x is a matrix.
    
    set.inv <- function(a.method) m <<- a.method
    ## set.inv() is a one line function that takes a function and store it.
    ## the stored matrix can be fetched by get.inv().
    ## set.inv() has 1 parameter.
    ## parameter list...
    ## a.method : parameter m is expected to be a function object which processes matrix.
    
    get.inv <- function() m
    ## get.inv() is a one line function that fetches the function set.inv() stored.
    ## get.inv() has 0 parameter.
    
    list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
    ## list above is the returned value of function makeCacheMatrix().
    ## list above has 4 elements. all 4 elements are functionobjects.
    
}



cacheSolve <- function( x = matrix(), ...) 
{
    ## cacheSolve() is a function that inverse/solve a matrix defined in the parameter.
    ## cacheSolve() has 2 parameters. 1 return value
    ## parameter list...
    ## x   : parameter x is a matrix, x is in default an empty matrix.
    ## ... : parameter ... inherits parameters.
    ## return value list...
    ## m   : returned value m is the inverse/solved matrix based on parameter x.
    
    m <- x$get.inv()
    
    ## the if() section below judges if the matrix has been cached.
    ## cached m returns a value other than NULL and the cached value
    ## is returned without further calculation.
    if(!is.null(m)) 
        {     
            
            message("getting cached data")
            return(m)
        }
    else
            ## the following section operates when is.null(m)==TRUE
            ## that is to say that m has not been cached before.  
            data <- x$get()
            m <- solve(data, ...)
            x$set.inv(m)
            m
            ## returned value m is the inverse/solved matrix based on parameter x.
    
}

## Note to who marks this work:
## Thanks for your patience! If I can ask you to kindly leave me some comments on this work 
## if at somewhere along the texts you read error or ameliorable approaches. Thus leaves me
## a chance to improve myself on these skills. Bisous! 
## Quan
