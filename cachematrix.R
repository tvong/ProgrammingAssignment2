## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix and cacheSolve function adapted from the makeVector and cachemean functions provided in class. This code assumes that the matrix actually has an inverse. 
## The purpose of this function is to cache the inverse of a matrix and return it from memory

makeCacheMatrix <- function(x = matrix()) {
    m <-NULL # Initializes x to an empty matrix and m to NULL type
    set <- function(y){
        x <<- y ## Stores values of y as x
        m <<- NULL ## Resets m to NULL
    }
    get <- function() x ## retrieves x
    setinv <- function(solve) m <<- solve ## Sets function for solve
    getinv <- function() m ## retrieves m
    list(set =set,get=get, ## creates a list for cacheMean to pull from
         setinv = setinv,
         getinv = getinv)
    }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv() #retrives m from makeCacheMatrix
        if(!is.null(m)){ # Checks to see if there is a cached matrix computed
          message("getting cached data")
          return(m) # retrieves m from 
        }
        data <-x$get() # stores values from get as data
        m <- solve(data) %*% data ## calculates the inverse matrix
        x$setinv(m) # stores the value of m by calling function stored in setinv
        m ## prints inverse matrix
}
