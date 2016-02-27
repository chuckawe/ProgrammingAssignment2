## MakeCacheMatrix creates a unique matrix able to cache its inverse
## Setting the value of the matrix
## Getting the value of the matrix


makeCacheMatrix <- function(x = matrix()) {
        M<-NULL
        set<-function(y){
                x<<-y
                M<<-NULL
        }
        get<-function()x
        setInvm<-function(Invm) M<<-Invm
        getInvm<-function()M
        list(set=set,get=get,setInvm=setInvm,getInvm=getInvm)
}


## CacheSolve computes the inverse of the unique matrix
## Setting the value of inverse matrix
## Getting the value of cached inverse matrix

cacheSolve <- function(x, ...) {
        M<-x$getInvm()
        if(!is.null(M)){
                message("Retrieving cached Inverse")
                return(M)
        }
        matdat<-x$get()
        M<-solve(matdat)
        x$setInvm(M)
        M
}
