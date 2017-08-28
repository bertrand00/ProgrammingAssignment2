## makeCacheMatrix() creates a list of 4 functions that allow to 
## put in cache the datas of the matrix and its inverse. 
## INPUT : 
## (opt.) the matrix to store (can be done later)
## OUTPUT : 
## List of functions that allows : 
## 1 : $set => allow to store the matrix in memory
## 2 : $get => allow to return the stored matrix
## 3 : $setinverse => allow to store the inverse matrix 
## 4 : $getinverse => allow to return the inverse matrix

## example : 
## M=makeCacheMatrix()
## M$set(matrix(1:4,nrow=2,ncol=2))
## M$get()
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## cacheSolve() allows to compute the inversion of the matrix, if possible. It 
## It requires a structure defined by makeCacheMatrix. 
## INPUT : 
## the said structure
## OUTPUT : 
## the inverse matrix
##
## example : 
## M=makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
##cacheSolve()
## M$get()

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat)
        x$setinverse(m)
        m
}