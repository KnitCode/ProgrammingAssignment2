## Put comments here that give an overall description of what your
## functions do

## Overall example usage of these functions together:  
## y<-makeCacheMatrix(x) -- set up contextually based (on x) functions
## cacheSolve(y) -- use above functions to retrieve/set the inverse of x

## makeCacheMatrix takes in the invertible matrix and creates contextual functions that can be used 
## later to repeatedly calculate the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     ## defines four functions dependent on the passed value x which will then be
     ## available for other functions to utilize. 
     ## use example: create matrix x, then y<-makeCacheMatrix(x) creates an object with four functions.
     ## y$get() will return the matrix x, y$setinv(solve) is a function that takes a matrix and returns
     ## its inverse... contextually equivalent to solve(x).
     
     ## Start by initializing the inv
     ## matrix to NULL so we'll know if it was called before.
     inv<-NULL
     set<-function(y) {
          x<<- y  ## this superassignment puts the passed value in the cached variable x
          inv<<- NULL  ## this is a new matrix so we want to initialize the inverted matrix again
     }
     get<-function() x ## retrieves the original matrix x from variable store
     setinv<-function(value) inv<<-value ## sets the inverse matrix to whatever is passed in value
     getinv<-function() inv  ## retrieves the inverse matrix from the cached variable inv
     list(set = set, get=get, setinv=setinv, getinv=getinv) ## returns the four functions
}


## cacheSolve is a function that takes a set of four contextually-defined functions, produced by 
## makeCacheMatrix, for an invertible matrix, and returns its inverse. It relies upon the 
## existance of contextually established functions and cached variables created in makeCacheMatrix.
## it is written to take in an object of unspecified class, but in practice is the list of functions
## returned by the makeCacheMatrix function above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv<-x$getinv()  ## pulls the current value for the inverse of x
     if(!is.null(inv)) {   ## if there is an inverse already stored, it retrieves it 
          message("Getting cached data... ")
          return(inv)  ## dump out early since we found the inverse
     }
     mat<-x$get()  ## if the inverse isn't already set we retrieve the matrix
     inv<-solve(mat, ...)  ## solve is R function that inverts matrices 
     x$setinv(inv)  ## and we push the inverted matrix into the cached store using <<- 
     inv
     
}
