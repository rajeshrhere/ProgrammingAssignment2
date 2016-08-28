## Put comments here that give an overall description of what your
## functions do

## Function will create an empty matrix which will cache the data in memory. 
makeCacheMatrix <- function(a = matrix()) {
    i<-NULL;
    set <- function(b) {
        print("Set Called");
        a<<-b;
        i<<-NULL;
    }
    get <- function() { print("Get Called"); a; }
    setInverse <- function(inverse) i<<-inverse;
    getInverse <- function() {print("getInverse called"); i; }
    list (set=set, get=get, setInverse=setInverse, getInverse=getInverse);
}

## Creates or returns the cached inverse of a matrix. 
cacheSolve <- function(c, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- c$getInverse();
    if(!is.null(i)) {
        message("getting cached data");
        return(i);
    }
    
    data <- c$get();
    i <- solve(data, ...);
    c$setInverse(i);
    i;
}

##Output from function
##> m<-matrix(1:4,2,2)
##> mat<-makeCacheMatrix(m)
##> mat$get()
##[1] "Get Called"
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(mat)
##[1] "getInverse called"
##[1] "Get Called"
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(mat)
##[1] "getInverse called"
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> 