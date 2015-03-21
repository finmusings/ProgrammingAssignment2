## Put comments here that give an overall description of what your
## functions do

## This function creates a pointer to an environment where the inverse of 
##      a matrix can be cached .

makeCacheMatrix <- function(x = matrix()) {
        ## initialise cached inverse
        c_inv <- NULL
        ## create the set function to be used to set the matrix from which the
        ##    inverse will be calculated
        set <- function(y){
                x <<- y
                c_inv <<- NULL
        }
        ## the get function echoes the input matrix
        get <- function() x
        ## the setinv function calls the R function solve() to create the
        ##    cached inverse
        setinv <- function(solve) c_inv<<- solve
        ## the getinv function echoes the cached inverse
        getinv <- function() c_inv
        ## the makeCacheMatrix function will return the following list which
        ##    contains the functions above
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function takes the list output from makeCacheMatrix as input
##      If the inverse has not yet been computed, then it calls setinv()
##      to compute the inverse.  If the inverse has already been cached,
##      it simply returns the cached copy

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        c_inv <- x$getinv()
        ## if the inverse hasn't already been computed, the line above will 
        ## return NULL.  Therefore if c_inv is not NULL, we are done
        if(!is.null(c_inv)) return(c_inv)
        ## we only get here if c_inv is NULL, which means we haven't cached
        ##   the inverse yet.
        work_mat <- x$get()
        c_inv <- solve(work_mat, ...)
        x$setinv(c_inv)
        c_inv
}
