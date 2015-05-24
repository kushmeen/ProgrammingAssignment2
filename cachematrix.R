## Put comments here that give an overall description of what your
## functions do 
## These functions are used to create, store and recall a matrix
## and it's inverse from cache.

## Write a short comment describing this function
## makeCaceMatrix is an R function which creates custom matrix 
##capable of running four functions in total i.e.
##1. set stores a matrix in cache
##2.get recalls the matrix
## setInverse and getInverse do the same but for an inverse 
##matrix of the original

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y){
    x <<- y  
    m <<- NULL #store matrix in cache 
  }
  get <- function() x #get matrix
  setInverse <- function(solve) m<<- solve #set inverse matrix
  getInverse <- function() m #get inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  ## create list of functions
}


## Write a short comment describing this function

##cacheSolve take a custom matrix created by the makeCacheMatrix 
##function and calculated the inverse matrix
##Before doing the calculation it checks if the calculation 
##has been done before.
## If that is so, calculation is recalled from the cache memory.
##If not, then calculation is done and stored in cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
 m <- x$getInverse()                 #query the x matrix's cache
  if(!is.null(m)){                    #if there is a cache the inverse has been previously calculated
    message("getting cached data")    # sent message indicating this is just cache 
    return(m)                         # return the cache  
  }
  data <- x$get()                     # get the matrix used by makeCacheMatrix function 
  m <- solve(data, ...)               # calculate the inverse of the matrix
  x$setInverse(m)
}
