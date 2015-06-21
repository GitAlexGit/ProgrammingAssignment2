## makeCacheMatrix
## This function accepts a matrix as input and returns a list of functions
## The function's internal state consists of a matrix ("invCache") storing the 
## cached inverse of the last saved input matrix. The actual matrix is preserved
## in the function's input variable, x
##
## The functions returned by makeCacheMatrix are
## setMatrix(x=matrix()): saves the passed matrix in invCache and flushes the cache
## getMatrix(): returns the currently stored matrix
## setInverse(x=matrix()): saves the passed matrix as the inverse (invCache)
## getInverse: returns the currently cached inverse. If the cache is empty, 
##           NULL is returned
## 
## The first call to makeCacheMatrix will save the passed matrix, create the 
## list of functions based on that matrix, and return them.
## Callers can then save an inverse in the cache by calling setInverse, and 
## retrieve as needed. A new matrix can be saved with setMatrix and the caller
## is responsible for saving the new cache subsequently.
## 
makeCacheMatrix <- function(x = matrix()) {
        
        # create and initialize the cache
        invCache <- NULL
        
        #debug printing
        cat("starting make function, invCache is = ", invCache, "\n")
        
        # returned the cached matrix
        getInverse <-function() invCache
        
        # return the saved matrix
        getMatrix <- function() x
        
        # set the cached matrix to the passed value
        setInverse <- function(x) {
                
                # debug
                cat("setInverse: x = ", x, "\n") 
                
                # save the cache
                invCache <<- x
        }
        
        # save the new matrix, flush the cache
        # Note: the function avoids saving an identical copy of the matrix. This
        # is based on the fact  that inverse calcuations are more intense
        # computationaly than matrix comparisons
        
        setMatrix <- function(setmat) {
                # check if the same matrix is being passed
                if (identical(setmat, x)) {
                        message("Warning: Identical matrix passed, cache not flushed")
                } else { 
                        # flush the cache and set the newlly passed matrix
                        # in the matrix storage
                        invCache <<-NULL    
                        x <<- setmat  
                }
        }
        # construct and return the list of functions
        list(getInverse=getInverse, 
             getMatrix=getMatrix, 
             setInverse =setInverse , 
             setMatrix= setMatrix)
        
}
        
        
## makeCacheMatrix
## cacheSolve accepts a list of functions (in the same format as returned by
## makeCacheMatrix) and returns the inverse of the matrix stored in makeCacheMatrix
## making use of the respective cache as needed
## The caller needs to have initiallized the makeCacheMatrix and passes it as 
## the input argument

cacheSolve <- function(x, ...) {

        # first, get the cache stored in the special matrix        
        inv <- x$getInverse()
        
        # in the cache is not null, then the inverse is returned from the cache
        if (!is.null(inv)) { 
                message("getting cached data")
                return(inv)
        }
        
        # else, if the cache is null, the inverse needs to be calculated
        # first, get the matrix stored in the special matrix object
        va <- x$getMatrix() 
        
        # find the inverse
        # we assume the matrix has an inverse, therefore no error management
        ret <- solve(va, ...)
        
        # set he cache to the newly calculated inverse
        x$setInverse(ret)
        
        # return the new inverse
        return(ret)
}
