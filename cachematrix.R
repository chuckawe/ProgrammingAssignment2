## MakeCacheMatrix creates a unique matrix able to cache its inverse
## Set sets the value of the matrix
## Get gets the value of the matrix


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


## CacheSolve computes the inverse of the unique matrix and either returns
##the cachched inverse or recomputes the inverse and caches the new return)

cacheSolve <- function(x, ...) {
        M<-x$getInvm()
        if(!is.null(M)){
                message("Retrieving cached Inverse")
                return(M)
        }
        matdata<-x$get()
        M<-solve(matdat)
        x$setInvm(M)
        M
}
