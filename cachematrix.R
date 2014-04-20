## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix :: creates a special "matrix", which is really a list containing functions to
##    1. set the value of the matrix 
##    2. get the value if the matrix 
##    3. set the value of the inverse
##    4. get the value of the inverse 
##    
##    makeCacheMatrix() :: creates object containing an empty matrix, use set(<matrix>) to set the value
##    makeCacheMatrix(<matrix>) :: creates object initialized with <matrix> 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## cached inverse matrix
    
    ## update matrix
    set <- function(y) {
        x <<- y           
        i <<- NULL  ## clear cached inverse         
    }
    get <- function() x  ## return enclosed matrix
    
    ## cache inverse
    setInverse <- function(inverse) i <<- inverse
    
    ## get cached inverse
    getInverse <- function() i

    ## return list of functions for manipulating matrix and its cached inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function
## cacheSolve :: returns inverse of the special "matrix" object created with makeCacheMatrix
##               if the inverse was already computed, returns pre-computed value
##               Note : cashSolve allows the caller to pass additional parameters to r funcion solve
##                      calling solve on same matrix but different additional parameters might produce
##                      different results, which is not accounted for by cashSolve. 
##                      This was discussed on forum and based on example not addressing similar problem for mean,
##                      it was decided to keep similar behavior for cacheSolve. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()  
    
    ## check if object x has cached the inverse value
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## inverse hadn't been cached yet, compute it
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)    ## cache the inverse value
    i    ## Note : returnued value is a matrix, not a "special matrix"
}
