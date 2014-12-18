## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix prepares the functions and the value assignements which will be used with cacheSolve. 
## Its input is a matrix (e.g., matrix(c(2,2,3,2),2,2) )
## Its output is used as input for cacheSolve -- NOT the original matrix.


makeCacheMatrix <- function(X = matrix()){
  inv <- NULL  #declares default value is null, until we call cacheSolve. 
  set<-function(y){
    X<<-y # makes R look for an existing definition of y in the parent environment,
    #to make cacheSolve able to automatically update this value when its called. 
    # otherwise, two adresses would be created. 
    inv<<- NULL # variable in the containing environment is updated to be NULL
  }
  get<-function()X # will call the input matrix when called by cacheSolve
  
  setinv <- function(solve) X <<- solve # will calculate the inverse matrix when called by cacheSolve. Works like set.
  getinv <- function() inv # simply retrieves the computed inv.
  list(set = set, get = get, setinv = setinv, getinv = getinv) # the output is a list of functions.
  
}


## Using the output of makeCacheMatrix as input, checks whethere the inverse 
## matrix has already been computed. If not, sends out a 
## message and computes it. 

cacheSolve <- function(X, ...) {
  inv<-X$getinv() # try to pull out the inverse
  if(!is.null(inv)){ # is it non-null(i.e., has it actually been computed?)
    message("getting cached data") # command prompt message
    return(inv) # retrieve the precomputed inv, this will be the output.
  }
  data <-X$get() # will pull out the data according to the function defined in 
                 # makeCacheMatrix -- not the global get function :-) 
  inv<- solve (data, ...) # declare inv
  X$setinv(inv) 
  inv
}
