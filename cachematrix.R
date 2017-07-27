## Caching the Inverse of a Matrix:
## The function makeCacheMatrix() create a matrix and cache(store)its inverse
## The function cacheSolve() checks whether inverse already exists 
## if not calculate it's inverse

## This function Function makeCacheMatrix gets a matrix as an input,set the value of the matrix,
## get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
## can cache its own object. 
## <<- operator is used to assign a value to an object in an environment that is different 
## from the current environment 
makeCacheMatrix <- function(x = matrix()) {
        inV <- NULL
        set <- function(y) {
                x <<- y
                inV <<- NULL
        }
        get <- function() x
        setinverse <- function(matInv) inV <<- matInv
        getinverse <- function() inV
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## The function cacheSolve takes the output of makeCacheMatrix(matrix) as an 
## input , checks whether the inverse of matrix already stored 
## if yes it displays a message  "getting cached inverse of given matrix" 
## and the cached object
## if not gets the original matrix data and calculate it's inverse using solve()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inV <- x$getinverse()
        if(!is.null(inV)) {
                message("getting cached inverse of given matrix")
                return(inV)
        }
        data <- x$get()
        inV <- solve(data, ...)
        x$setinverse(inV)
        inV


}
