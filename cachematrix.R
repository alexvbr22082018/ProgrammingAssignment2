## As the mean example, here are two functions:
## 1. makeCacheMatrix: that transform a matrix in a list of functions described below
## 2. cacheSolve: a function that assign (if it's not already stored) the inverted matrix to the object
#     created object whit makeCacheMatrix

## This function does the following:
## 1. set the matrix to ve used calling x$set(matrix(c(2,1,5,3),2,2)) i.e.
## 2. return the set before calling x$get()
## 3. setinv assigns all changes done in inv calling x$setinv(), this is the key for the next function
## 4. getinv returns the inverted matrix stored before

makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()
    list(set = function(y){
        x <<- y
        inv <<- matrix()
    },
    get = function(){
        x
    },
    setinv = function(solve){
        inv <<- solve
    },
    getinv = function(){
        inv
    }
    )
    

}


## Changes the element getinv in the object x created whit makeCacheMatrix, gives a message if 
# the inverted matrix is already set up in object x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    y <- x$getinv()
    
    if(!all(dim(y) == c(1,1))){
        message("giving a cached matrix")
        return(y)
    }
    
    x$setinv(solve(x$get()))
    
    x$getinv()
}
