## When given a matrix these functions will be able to store
## the original matrix and cache its inverse

## In this first function, when given a matrix it will
## create a list of functions: $set <- sets value of matrix
## that you wish to find inverse of, $get <- retrieves the 
## matrix you have stored, $setInv <- is used to write the 
## calculated inverse of the matrix to the cache, and 
## $getInv <- reads the inverse matrix from the cache.


makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) Inv <<- inverse 
        getInv <- function() Inv
        list(set = set, get = get, setInv = setInv, 
             getInv = getInv)
}


## This function takes the list generated form the previous
## function and uses the functions created within the list 
## to read a cached matrix, calculate its inverse, and cache
## its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInv()
        if(!is.null(Inv)){
                message("getting cached data")
                return(Inv)
        }
        Matr <- x$get()
        Inv <- solve(Matr)
        x$setInv(Inv)
        Inv
}
