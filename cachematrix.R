## function initializes with empty inverse matrix value
## after providing input (either by x argument or using makeCacheMatrix$set() it includes matrix(x), inverseMatrix = NULL)
## x value and inverseMatrix value can be accessed by makeCacheMatrix$get function
## inverseMatrix can be accessed through makeCacheMatrix$getInverse() function
## inverseMatrix can be set through makeCacheMatrix$setInverse() function
## inverseMatrix is a variable of makeCacheMatrix scope and is assigned from child functions with '<<-' operator

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y){
            x <<- y
            inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## function starts with checking if provided argument x, being a result of makeCacheMatrix
## stores inverse matrix. If not, it calculates inverse Matrix and stores it in x$inverseMatrix variable
## using x$setInverse() function

cacheSolve <- function(x) {
    inverse <- x$getInverse()
    if (!is.null(inverse)){
        return(inverse)
    }
    originalMatrix <- x$get()
    inverse <- solve(originalMatrix)
    x$setInverse(inverse) 
}
