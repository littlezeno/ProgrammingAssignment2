## Week 3 Assignment
## 07/13/2017 by Zeno Zhang

## MaxkCasheMatrix would 
#  1. set the value of the matrix
#  2. Get the value of the matrix
#  3. Set the value of the inverse of the matrix
#  4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    } # Set the value of matrix
    get<-function() {x} # return matrix
    setinverse<-function(inve) m<<-inve
    # Assign inverse of a matrix
    getinverse<-function() {m}
    # Get the inferse of a matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve would 
#  1. Retrive the inverse of a matrix if an inverse exists
#  2. Create the inverse of a matirx if an inverse is absent
#  3. Return the inverse of a matrix


cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message("Getting Catched matrix")
        return(m) # Return the matrix if the matrix is not NULL
    }
    data<-x$get() #If no interse was found, than get the original matrix
    m<-solve(data)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

## Test code
## A <- matrix( c(4, 9, 0,  3,-1, 2, 1, 0,-1), nrow=3, byrow=TRUE)
## Aa<-makeCacheMatrix(A)
## cacheSolve(Aa)