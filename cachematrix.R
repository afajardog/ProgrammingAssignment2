## The following functions try to calculate the inverse of a matrix. The first one initalize a vector
## to store the cache of previsouly calculated inverse matrices and the second one search the cacheVector
## for the inverse or calculates if necessary. 


## The following function creates a list with the value of matriz and it's inverse if it has been calculated

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){ x}
        setinverse <- function(inverse) {inv<<- inverse}
        getinverse <- function() {inv}
        
        list(set=set, get= get, 
             setinverse=setinverse, getinverse=getinverse)
        
}


## The following functions receives a cacheVector and a matrix and looks for the inverse of the matrix
## if it has already been calculated or calculates the new inverse of the matrix and stores it in the
## cache vector. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Gets the previous matrix x
        oldX <- x$get()
        #Gets the previously calculated inverse x
        inv<-x$getinverse()
        
        #Checks first if the matrix has changed and if it has been calculated
        #If both conditions are true it retunrs the previous calcualted matrix
        
        if(identical(oldX,...) && !is.null(inv)){
                message("Getting cached data")
                return (inv)
        }
        
        #Calculates the inverse if the above conditions aren't compiled
        inv <- t(solve(... ))
        
        x$setinverse(inv)
        
        inv
        
}
