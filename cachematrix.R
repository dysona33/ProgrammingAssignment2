# makeCacheMatrix returns a list with four functions: set,get,setInv,getInv.
# It constructs x in a form that cana be cached

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x<<-y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set=set,get=get,setInv=setInv,getInv=getInv)
}

# if inverse is cached, return it. Otherwise, solve for inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached inverse")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        inv
}
