

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL #inverse matrix
        set <- function(matrix){
                
                x <<- matrix
                inv <<- NULL
        }
        
        get <- function() x
        setInv <- function(inverseM) inv <<- inverseM
        getInv <- function() inv
        
        list(set=set, get=get, setInv=setInv, getInv=getInv)
        
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cMatrix, ...) {
        
        inv <- cMatrix$getInv()
        if(!is.null(inv)){
                
                message('gettin cached inverse')
                return(inv)
                
        }
        
        x <- cMatrix$get()
        inv <- solve(x)
        cMatrix$setInv(inv)
        inv   
        
}

